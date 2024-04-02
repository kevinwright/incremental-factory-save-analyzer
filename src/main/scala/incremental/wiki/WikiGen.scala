package incremental.wiki

import cats.implicits.given
import cats.Traverse
import cats.effect.{ExitCode, IO, Resource}
import incremental.commands.PublishWiki
import incremental.model.*
import incremental.model.skills.{Skill, Skills}

extension (io: IO[Any]) {
  def when(condition: Boolean): IO[Unit] =
    if condition then io.as(()) else IO.unit
}

class WikiGen(
    args: PublishWiki.ParsedArgs,
    parallelism: Int = 200
) {
  def run(): IO[ExitCode] = {
    val apiResource = for {
      creds <- Resource.liftK(WikiCredentials.load(args.credentialsPath))
      api <- MediaWikiApi.login(creds, logActivityToConsole = true)
    } yield api

    apiResource.use { api =>
      for {
        _ <- upsertMainPages(api).when(args.publishMain)
        _ <- upsertPages(api, Item.ordered).when(args.publishItems)
        _ <- upsertPages(api, Buildings.ordered).when(args.publishBuildings)
        _ <- upsertPages(api, Skills.ordered).when(args.publishSkills)
        _ <- upsertPages(api, ParcelTypes.ordered).when(args.publishParcelTypes)
//        _ <- dumpWikiTablesToConsole
//        _ <- dumpSkillsPagesToConsole
      } yield ExitCode.Success
    }
  }

  private def upsertMainPages(api: MediaWikiApi): IO[Unit] = for {
    _ <- api.upsertPage("All Items", WikiTables.itemsTable)
    _ <- api.upsertPage("All Buildings", WikiTables.buildingsTable)
    _ <- api.upsertPage("All Parcel Types", WikiTables.parcelTypesTable)
    _ <- api.upsertPage("Skills Tree", PageContentMaker.skillTree())
  } yield ()

  private def parallellise[T[_] : Traverse, A](ta: T[A])(fn: A => IO[Any]): IO[Unit] =
    IO.parTraverseN(parallelism)(ta)(a =>
      retryOnRateLimit(fn(a))
    ).as(())
    
  private def upsertPages[T[_]: Traverse, A: PageSource](
      api: MediaWikiApi,
      ta: T[A]
  ): IO[Unit] =
    parallellise(ta)(a =>
      api.upsertPage(
        title = a.title,
        newContent = a.mkContent
      ) >> a.redirectTitles.traverse(alt => api.upsertRedirect(alt, a.title))
    )

  def dumpWikiTablesToConsole: IO[Unit] =
    for {
      _ <- IO.println("\n=== ITEMS WIKI TABLE ===\n")
      _ <- IO.println(WikiTables.itemsTable)
      _ <- IO.println("\n=== BUILDINGS WIKI TABLE ===\n")
      _ <- IO.println(WikiTables.buildingsTable)
      _ <- IO.println("\n=== PARCEL TYPES WIKI TABLE ===\n")
      _ <- IO.println(WikiTables.parcelTypesTable)
    } yield ()

  def dumpSkillsPagesToConsole: IO[Unit] =
    Skill.values.toSeq
      .traverse(s =>
        for {
          _ <- IO.println(s.displayName)
          _ <- IO.println(PageContentMaker.skillPage(s))
        } yield ()
      )
      .as(())
}
