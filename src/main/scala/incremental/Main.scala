package incremental

import incremental.consoleui.Table
import incremental.model.{
  Buildings,
  GameState,
  GameStateRoot,
  Item,
  ParcelType
}
import incremental.model.skills.Skill
import incremental.Simulation.SimulationState
import incremental.wiki.{
  MediaWikiApi,
  PageContentMaker,
  WikiCredentials,
  WikiPage,
  WikiTables
}
import cats.implicits.*
import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.given
import scala.util.Random

object Main extends IOApp {
  def run(rawArgs: List[String]): IO[ExitCode] =
    CommandLineParser.process(rawArgs)(parsedArgs =>
      SaveGameLoader
        .load(parsedArgs.saveFile)
        .flatMap(gsr => run2(parsedArgs, gsr))
    )

  def run2(args: CommandLineParser.Args, gsr: GameStateRoot): IO[ExitCode] = {
    val gameState = gsr.gameState

    val apiResource = for {
      creds <- Resource.liftK(WikiCredentials.load(args.credentialsPath))
      api <- MediaWikiApi.login(creds, logActivityToConsole = true)
    } yield api

    apiResource.use { api =>
      for {
//        _ <- api.upsertPage("All Items", WikiTables.itemsTable)
//        _ <- api.upsertPage("All Buildings", WikiTables.buildingsTable)
//        _ <- api.upsertPage("All Parcel Types", WikiTables.parcelTypesTable)
//        _ <- api.upsertPage(
//          title = "Skills Tree",
//          newContent = PageContentMaker.skillTree(gameState.skilltree)
//        )
//        _ <- IO.parTraverseN(5)(Item.ordered)(r =>
//          api.upsertPage(
//            title = r.displayName,
//            newContent = PageContentMaker.itemPage(r)
//          ) >> api.upsertRedirect(r.name(), r.displayName)
//        )
//        _ <- IO.parTraverseN(5)(Buildings.ordered)(b =>
//          IO.sleep(Random.between(0, 2).seconds) >>
//            api.upsertPage(
//              title = b.displayName,
//              newContent = PageContentMaker.buildingPage(b)
//            ) >> api.upsertRedirect(b.name(), b.displayName)
//        )
        _ <- IO.parTraverseN(1)(Skill.values.toSeq)(s =>
          IO.sleep(Random.between(0, 2).seconds) >>
            api.upsertPage(
              title = s.displayName,
              newContent = PageContentMaker.skillPage(s)
            ) >> api.upsertRedirect(s.toString, s.displayName)
        )
//        _ <- IO.parTraverseN(1)(ParcelType.values.toSeq)(pt =>
//          IO.sleep(Random.between(0, 2).seconds) >>
//            api.upsertPage(
//              title = pt.displayName,
//              newContent = PageContentMaker.parcelTypePage(pt)
//            ) >> api.upsertRedirect(pt.toString, pt.displayName)
//        )
//        _ <- dumpSkillsToConsole
//        _ <- dumpSummariesToConsole(gameState, args)
//        _ <- dumpWikiTablesToConsole
      } yield ExitCode.Success

    //    println(PageContentMaker.buildingPage(Building.kiln))

    //    val flowTree = summaries.flowTreeTable
    //    println(flowTree.withBorders().toStringBlock)
    //    flowTree.bodyRows.foreach(r => println(r))

    //    val simState = Simulation(gameState).run()
    //    printDeficitParcels(simState)

    }
  }

  def dumpWikiTablesToConsole: IO[Unit] =
    for {
      _ <- IO.println("\n=== ITEMS WIKI TABLE ===\n")
      _ <- IO.println(WikiTables.itemsTable)
      _ <- IO.println("\n=== BUILDINGS WIKI TABLE ===\n")
      _ <- IO.println(WikiTables.buildingsTable)
      _ <- IO.println("\n=== PARCEL TYPES WIKI TABLE ===\n")
      _ <- IO.println(WikiTables.parcelTypesTable)
    } yield ()

  def dumpSummariesToConsole(
      gameState: GameState,
      args: CommandLineParser.Args
  ): IO[Unit] = IO {
    val summaries = Summaries(gameState)
    if args.summaryFlags.buildings then
      println(summaries.buildingsTable.withBorders().toStringBlock)

    if args.summaryFlags.resources then
      println(summaries.resourcesTable.withBorders().toStringBlock)

    if args.summaryFlags.skills then
      println(summaries.skillsTreeTable.withBorders().toStringBlock)
  }

  def dumpSkillsToConsole: IO[Unit] =
    IO.parTraverseN(10)(Skill.values.toSeq)(s =>
      for {
        _ <- IO.println(s.displayName)
        _ <- IO.println(PageContentMaker.skillPage(s))
      } yield ()
    ).flatMap(_ => IO.unit)

  def printDeficitParcels(simState: Simulation.SimulationState): Unit = {
    simState.parcels
      .find(_.deficit.nonEmpty)
      .foreach { p =>
        println(s"parcel: ${p.underlying.displayName} [${p.underlying.id}]")
        p.deficit.foreach { case (r, qty) =>
          println(s"● deficit on ${r.displayName} [${r.name}] = $qty")
          println(s"● producing ${p.production.getOrElse(r, 0.0d)}")
          println(s"● consuming ${p.consumption.getOrElse(r, 0.0d)}")
          simState
            .importsTo(p, r)
            .foreach(c =>
              println(
                s"● importing ${c.actualThroughput} from ${c.source.underlying.displayName} [${c.source.underlying.id}]"
              )
            )
          p.production
        }
      }
  }

}
