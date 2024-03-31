package gameanalyzer

import gameanalyzer.consoleui.Table
import gameanalyzer.model.Building.remoteConstructionFacility
import gameanalyzer.model.{
  Building,
  Buildings,
  GameState,
  GameStateRoot,
  Item,
  Skill
}
import cats.implicits.*
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
import cats.Parallel

import java.time.LocalDateTime
import scala.util.{Failure, Success}

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
      api <- MediaWikiApi.login(creds)
    } yield api

    apiResource.use { api =>
      for {
        _ <- IO.println(s"Uploading All Items Page") >>
          api.upsertPage(
            title = "All Items",
            newContent = WikiTables.itemsTable
          )
        _ <- IO.println(s"Uploading All Buildings Page") >>
          api.upsertPage(
            title = "All Buildings",
            newContent = WikiTables.buildingsTable
          )
        _ <- IO.println(s"Uploading All Parcel Types Page") >>
          api.upsertPage(
            title = "All Parcel Types",
            newContent = WikiTables.parcelTypesTable
          )
        _ <- IO.println(s"Uploading Skills Tree Page") >>
          api.upsertPage(
            title = "Skills Tree",
            newContent = PageContentMaker.skillTree(gameState.skilltree)
          )
        _ <- IO.parTraverseN(200)(Item.ordered)(r =>
          IO.println(s"Uploading page for Item ${r.displayName}") >>
            api.upsertPage(
              title = r.displayName,
              newContent = PageContentMaker.itemPage(r)
            ) >> {
              if r.displayName.equalsIgnoreCase(r.name()) then IO.unit
              else
                api.upsertPage(
                  title = r.name(),
                  newContent = s"#REDIRECT [[${r.displayName}]]"
                )
            }
        )
        _ <- IO.parTraverseN(200)(Buildings.ordered)(b =>
          IO.println(s"Uploading page for ${b.displayName}") >>
            api.upsertPage(
              title = b.displayName,
              newContent = PageContentMaker.buildingPage(b)
            ) >> {
              if b.displayName.equalsIgnoreCase(b.name()) then IO.unit
              else
                api.upsertPage(
                  title = b.name(),
                  newContent = s"#REDIRECT [[${b.displayName}]]"
                )
            }
        )
        _ <- IO.parTraverseN(200)(Skill.values.toSeq)(s =>
          IO.println(s"Uploading ${s.displayName}") >>
            api.upsertPage(
              title = s.displayName,
              newContent = PageContentMaker.skillPage(s)
            ) >>
            IO.println(
              s"Uploading Redirect for ${s.toString} => ${s.displayName}"
            ) >>
            api.upsertPage(
              title = s.toString,
              newContent = s"#REDIRECT [[${s.displayName}]]"
            )
        )
//        _ <- dumpSkillsToConsole
        _ <- dumpSummariesToConsole(gameState, args)
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

  def mainPageWikiMarkup(gameState: GameState): String = {
    s"""
       ~
       ~ {{:About The Game}}
       ~
       ~== Items ==
       ~
       ~${WikiTables.itemsTable}
       ~
       ~== Buildings ==
       ~
       ~${WikiTables.buildingsTable}
       ~
       ~== Parcels ==
       ~
       ~${WikiTables.parcelTypesTable}
       ~
       ~== Skills ==
       ~
       ~${PageContentMaker.skillTree(gameState.skilltree)}
       ~
       ~This page was generated by a bot on ${LocalDateTime.now()}
       ~
       ~""".stripMargin('~')
  }

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
