package incremental.wiki

import cats.implicits.given
import cats.Traverse
import cats.effect.{ExitCode, IO, Resource}
import incremental.commands.PublishWiki
import incremental.model.{Buildings, Item, ParcelType}
import incremental.model.skills.Skill

import scala.concurrent.duration.{FiniteDuration, given}

class WikiGen(args: PublishWiki.ParsedArgs) {

  def parallelism: Int = 200

  def retryOnRateLimit[A](
      ioa: IO[A],
      initialDelay: FiniteDuration,
      maxRetries: Int
  ): IO[A] =
    ioa.recoverWith {
      case err: WikiException if err.isRateLimited && maxRetries > 0 =>
        IO.sleep(initialDelay) *> retryOnRateLimit(
          ioa,
          initialDelay * 2,
          maxRetries - 1
        )
      case err => IO.raiseError(err)
    }

  def processBlock[T[_]: Traverse, A](ta: T[A])(fn: A => IO[Any]): IO[Unit] =
    IO.parTraverseN(parallelism)(ta)(a =>
      retryOnRateLimit(fn(a), initialDelay = 10.seconds, maxRetries = 10)
    ).as(())

  extension (io: IO[Any]) {
    def when(condition: Boolean): IO[Unit] =
      if condition then io.as(()) else IO.unit
  }

  def run(): IO[ExitCode] = {
    val apiResource = for {
      creds <- Resource.liftK(WikiCredentials.load(args.credentialsPath))
      api <- MediaWikiApi.login(creds, logActivityToConsole = true)
    } yield api

    apiResource.use { api =>
      for {
        _ <- upsertMainPages(api).when(args.publishMain)
        _ <- upsertItemPages(api).when(args.publishItems)
        _ <- upsertBuildingPages(api).when(args.publishBuildings)
        _ <- upsertSkillPages(api).when(args.publishSkills)
        _ <- upsertParcelTypePages(api).when(args.publishParcelTypes)
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

  private def upsertItemPages(api: MediaWikiApi): IO[Unit] =
    processBlock(Item.ordered)(r =>
      api.upsertPage(
        title = r.displayName,
        newContent = PageContentMaker.itemPage(r)
      ) >> api.upsertRedirect(r.name(), r.displayName)
    )

  private def upsertBuildingPages(api: MediaWikiApi): IO[Unit] =
    processBlock(Buildings.ordered)(b =>
      api.upsertPage(
        title = b.displayName,
        newContent = PageContentMaker.buildingPage(b)
      ) >> api.upsertRedirect(b.name(), b.displayName)
    )

  private def upsertSkillPages(api: MediaWikiApi): IO[Unit] =
    processBlock(Skill.values.toSeq)(s =>
      api.upsertPage(
        title = s.displayName,
        newContent = PageContentMaker.skillPage(s)
      ) >> api.upsertRedirect(s.toString, s.displayName)
    )

  private def upsertParcelTypePages(api: MediaWikiApi): IO[Unit] =
    processBlock(ParcelType.values.toSeq)(pt =>
      api.upsertPage(
        title = pt.displayName,
        newContent = PageContentMaker.parcelTypePage(pt)
      ) >> api.upsertRedirect(pt.toString, pt.displayName)
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
