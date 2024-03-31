package gameanalyzer

import gameanalyzer.consoleui.Table
import gameanalyzer.model.{
  Building,
  Buildings,
  GameState,
  GameStateRoot,
  Item,
  Skill
}
import gameanalyzer.Simulation.SimulationState
import gameanalyzer.wiki.{
  MediaWikiApi,
  PageContentMaker,
  WikiCredentials,
  WikiPage,
  WikiTables
}
import cats.implicits.*
import cats.effect.{ExitCode, IO, IOApp, Resource}

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
        _ <- api.upsertPage(
          title = "All Items",
          newContent = WikiTables.itemsTable
        )
        _ <- api.upsertPage(
          title = "All Buildings",
          newContent = WikiTables.buildingsTable
        )
        _ <- api.upsertPage(
          title = "All Parcel Types",
          newContent = WikiTables.parcelTypesTable
        )
        _ <- api.upsertPage(
          title = "Skills Tree",
          newContent = PageContentMaker.skillTree(gameState.skilltree)
        )
        _ <- IO.parTraverseN(50)(Item.ordered)(r =>
          api.upsertPage(
            title = r.displayName,
            newContent = PageContentMaker.itemPage(r)
          ) &> api.upsertRedirect(r.name(), r.displayName)
        )
        _ <- IO.parTraverseN(50)(Buildings.ordered)(b =>
          api.upsertPage(
            title = b.displayName,
            newContent = PageContentMaker.buildingPage(b)
          ) &> api.upsertRedirect(b.name(), b.displayName)
        )
        _ <- IO.parTraverseN(50)(Skill.values.toSeq)(s =>
          api.upsertPage(
            title = s.displayName,
            newContent = PageContentMaker.skillPage(s)
          ) &> api.upsertRedirect(s.toString, s.displayName)
        )
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
