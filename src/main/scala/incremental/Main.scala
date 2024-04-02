package incremental

import incremental.consoleui.Table
import incremental.Simulation.SimulationState
import incremental.wiki.WikiGen
import cats.effect.{ExitCode, IO}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import commands.*

object Main
    extends CommandIOApp(
      name = "Incremental Factory Tools",
      header = "The factory must grow",
      version = "0.0.x"
    ) {
  override def main: Opts[IO[ExitCode]] =
    (AnalyseSaveFile.subcommand orElse PublishWiki.subcommand).map {
      case args: AnalyseSaveFile.ParsedArgs =>
        analyseSaveFile(args)
      case args: PublishWiki.ParsedArgs =>
        publishWiki(args)
    }

  def analyseSaveFile(args: AnalyseSaveFile.ParsedArgs): IO[ExitCode] = for {
    saveRoot <- SaveGameLoader.load(args.saveFile)
    gameState = saveRoot.gameState
    summaries <- IO(Summaries(gameState))
    _ <- dumpSummariesToConsole(summaries, args)
//    flowTree <- IO(summaries.flowTreeTable)
//    _ <- IO.println(flowTree.withBorders().toStringBlock)
//    _ <- flowTree.bodyRows.traverse(IO.println)
//    simState <- IO.blocking(Simulation(gameState).run())
//    _ <- IO(printDeficitParcels(simState))
  } yield ExitCode.Success

  def publishWiki(args: PublishWiki.ParsedArgs): IO[ExitCode] = for {
    _ <- WikiGen(args).run()
  } yield ExitCode.Success

  def dumpSummariesToConsole(
      summaries: Summaries,
      args: AnalyseSaveFile.ParsedArgs
  ): IO[Unit] = IO {
    if args.includeBuildings then
      println(summaries.buildingsTable.withBorders().toStringBlock)

    if args.includeResources then
      println(summaries.resourcesTable.withBorders().toStringBlock)

    if args.includeSkills then
      println(summaries.skillsTreeTable.withBorders().toStringBlock)
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
