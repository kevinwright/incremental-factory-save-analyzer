package incremental.wiki

import cats.effect.{ExitCode, IO, Resource}
import incremental.commands.PublishWiki
import incremental.model.{Buildings, Item, ParcelType}
import incremental.model.skills.Skill

import java.nio.file.Path
import scala.util.Random
import scala.concurrent.duration.given

object WikiGen {
  
  extension (io: IO[Any]) {
    def when(condition: Boolean): IO[Unit] =
      if condition then io.as(()) else IO.unit
  }
  
  def run(args: PublishWiki.ParsedArgs): IO[ExitCode] = {
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
    IO.parTraverseN(5)(Item.ordered)(r =>
      api.upsertPage(
        title = r.displayName,
        newContent = PageContentMaker.itemPage(r)
      ) >> api.upsertRedirect(r.name(), r.displayName)
    ).as(())

  private def upsertBuildingPages(api: MediaWikiApi): IO[Unit] =
    IO.parTraverseN(5)(Buildings.ordered)(b =>
      IO.sleep(Random.between(0, 2).seconds) >>
        api.upsertPage(
          title = b.displayName,
          newContent = PageContentMaker.buildingPage(b)
        ) >> api.upsertRedirect(b.name(), b.displayName)
    ).as(())

  private def upsertSkillPages(api: MediaWikiApi): IO[Unit] =
    IO.parTraverseN(1)(Skill.values.toSeq)(s =>
      IO.sleep(Random.between(0, 2).seconds) >>
        api.upsertPage(
          title = s.displayName,
          newContent = PageContentMaker.skillPage(s)
        ) >> api.upsertRedirect(s.toString, s.displayName)
    ).as(())

  private def upsertParcelTypePages(api: MediaWikiApi): IO[Unit] =
    IO.parTraverseN(1)(ParcelType.values.toSeq)(pt =>
      IO.sleep(Random.between(0, 2).seconds) >>
        api.upsertPage(
          title = pt.displayName,
          newContent = PageContentMaker.parcelTypePage(pt)
        ) >> api.upsertRedirect(pt.toString, pt.displayName)
    ).as(())

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
    IO.parTraverseN(10)(Skill.values.toSeq)(s =>
      for {
        _ <- IO.println(s.displayName)
        _ <- IO.println(PageContentMaker.skillPage(s))
      } yield ()
    ).flatMap(_ => IO.unit)
}
