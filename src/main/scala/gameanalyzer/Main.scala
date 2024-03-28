package gameanalyzer

import com.monovore.decline.*
import gameanalyzer.consoleui.Table
import gameanalyzer.model.Building.remoteConstructionFacility
import gameanalyzer.model.{
  Building,
  GameState,
  GameStateRoot,
  ParcelInstance,
  SkillTree
}
import cats.implicits.*
import gameanalyzer.Simulation.SimulationState
import gameanalyzer.wiki.{MediaWikiApi, WikiCredentials, WikiPage}
import gameanalyzer.wikigen.WikiTables

import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import scala.util.{Failure, Success}

object Main {
  case class SummaryFlags(
      buildings: Boolean,
      resources: Boolean,
      skills: Boolean
  )

  object SummaryFlags {
    def parse(s: String): SummaryFlags = {
      val parts = s.split(",").toSet
      val all = parts("a") || parts("all")
      SummaryFlags(
        buildings = all || parts("b") || parts("buildings"),
        resources = all || parts("r") || parts("resources"),
        skills = all || parts("s") || parts("skills")
      )
    }

    val opt: Opts[SummaryFlags] = Opts
      .option(
        long = "summarise",
        short = "s",
        metavar = "b|buldings|r|resources|s|skills|a|all",
        help = "Comma-Separated list of things to summarise"
      )
      .withDefault("")
      .map(parse)
  }

  case class Args(
      summaryFlags: SummaryFlags,
      customCredentialsPath: Option[Path],
      blueprint: Option[String],
      saveFile: String
  ) {
    def credentialsPath: Path = customCredentialsPath.getOrElse(
      Path.of(
        System.getProperty("user.home"),
        "incremental-factory-wiki.credentials"
      )
    )
  }

  private val credentialsFileOpt: Opts[Option[Path]] = Opts
    .option[Path](
      long = "credentials",
      metavar = "string",
      help = "A blueprint to analyze."
    )
    .orNone

  private val blueprintOpt: Opts[Option[String]] = Opts
    .option[String](
      long = "blueprint",
      metavar = "string",
      help = "A blueprint to analyze."
    )
    .orNone

  private val saveFileOpt: Opts[String] = Opts
    .argument[String](
      metavar = "filename"
    )

  private val command: Command[Args] = Command(
    name = "game-analyser",
    header = "Print the last few lines of one or more files."
  ) {
    (
      SummaryFlags.opt,
      credentialsFileOpt,
      blueprintOpt,
      saveFileOpt
    ).mapN(Args.apply)
  }

  def main(rawArgs: Array[String]): Unit =
    command.parse(rawArgs, sys.env) match {
      case Left(help) if help.errors.isEmpty =>
        println(help)
        sys.exit(0)

      case Left(help) =>
        System.err.println(help)
        sys.exit(1)

      case Right(parsedArgs) =>
        SaveGameLoader.load(parsedArgs.saveFile) match {
          case Success(gsr)       => run(parsedArgs, gsr)
          case Failure(exception) => exception.printStackTrace(System.err)
        }
    }

  def run(args: Args, gsr: GameStateRoot): Unit = {

    val wikiOps = for {
      creds <- WikiCredentials.load(args.credentialsPath)
      api <- MediaWikiApi.login(creds)
      _ <- api.getPage("stone")
      _ <- api.upsertPage(
        title = "bot_test_page",
        newContent = s"Updated on ${LocalDateTime.now()}"
      )
      _ <- api.upsertPage(
        title = "bot_test_page_2",
        newContent = s"Updated on ${LocalDateTime.now()}"
      )
    } yield ()

    val _ = wikiOps.get

    val gameState = gsr.gameState

    if args.summaryFlags.buildings then printBuildingSummary(gameState)
    if args.summaryFlags.resources then printResourceSummary(gameState)
    if args.summaryFlags.skills then printSkillTree(gameState.skilltree)

//    printFlowTree(gameState)
//    println("")
//    println("═══════════════════════════")
//    println("")

//    val simState = Simulation(gameState).run()
//    printDeficitParcels(simState)

    println("")
    println("=== ITEMS WIKI TABLE ===")
    println("")
    println(WikiTables.itemsTable())

    println("")
    println("=== BUILDINGS WIKI TABLE ===")
    println("")
    println(WikiTables.buildingsTable())
  }

//
//  private def printParcelTypeWikiTable(): Unit = {
//
//    println("=== PARCEL TYPES WIKI TABLE ===")
//    println("")
//    println("{| class=\"sortable\"")
//    println(
//      "! Name !! Base<br>Connections !! Base<br>Buildings !! Base<br>Storage"
//    )
//    val allTypes = ParcelType.values
//
//    println(
//      allTypes
//        .map(p => {
//          val limits = p.baseLimits
//          s"| ${p.name} || ${limits.connections} || ${limits.buildings} || ${limits.storage}"
//        })
//        .mkString("|-\n", "\n|-\n", "\n|}\n")
//    )
//
//    println("=== END WIKI TABLE ===")
//
//  }

  private def printBuildingSummary(gameState: GameState): Unit = {
    val allBuildings =
      gameState.parcels.instances
        .flatMap(_.buildings)
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sum)
        .toMap

    println(
      Table(
        Some("Buildings"),
        Seq("Building ID", "Name", "Qty"),
        allBuildings.toList
          .sortBy(_._1.ordinal())
          .map((b, v) => Seq(b.name, b.displayName, v))
      ).withBorders().toStringBlock
    )
  }

  private def printResourceSummary(gameState: GameState): Unit = {
    val allResources =
      gameState.parcels.instances
        .flatMap(_.resources)
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sum)
        .toMap

    println(
      Table(
        Some("Resources"),
        Seq("Resource ID", "Name", "Qty", "Throughput", "Per-Building Boost"),
        allResources.toList
          .sortBy(_._1.ordinal())
          .map((r, v) =>
            Seq(
              r.name,
              r.displayName,
              v.toInt,
              gameState.skilltree.maxThroughputFor(r),
              gameState.skilltree.specializationBoostFor(r)
            )
          )
      ).withBorders().toStringBlock
    )
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

  private def printSkillTree(skillTree: SkillTree): Unit = {
    val allNodes = skillTree.nodes
    val nonRootNodeIds = allNodes.flatMap(_.connectedNodes).toSet
    val rootNodes = allNodes.filterNot(n => nonRootNodeIds.contains(n.id))

    val table = Table.buildTreeTable(
      title = Some("Skills"),
      headerRow = Seq("ID", "Name", "Type", "Level", "Value"),
      roots = rootNodes,
      walkDown =
        node => allNodes.filter(n => node.connectedNodes.contains(n.id)),
      mkRow = node =>
        Seq(
          node.id,
          node.name,
          node.skillType.getOrElse("n/a"),
          node.currentLevel,
          node.currentSimpleValue
            .orElse(
              node.currentCompoundValue
                .map(opt => opt.map((k, v) => s"$v×$k").mkString(", "))
            )
            .getOrElse("")
        )
    )
    println(table.withBorders().toStringBlock)
  }

  private def printFlowTree(gameState: GameState): Unit = {
    val parcels: Seq[ParcelInstance] = gameState.parcels.instances
    val connections = gameState.nodeConnections

    val exporterIds = connections.map(_.sourceId).toSet
    val roots = parcels.filter(p =>
      !exporterIds.contains(p.id) || p.buildings.contains(
        remoteConstructionFacility
      )
    )

    val table = Table.buildTreeTable(
      title = Some("Flows"),
      headerRow = Seq("Id", "Name"),
      roots = roots,
      walkDown = node =>
        parcels.filter(p =>
          connections
            .filter(_.targetId == node.id)
            .map(_.sourceId)
            .contains(p.id)
        ),
      mkRow = node => Seq(node.id, node.displayName)
    )

    println(table.withBorders().toStringBlock)
    table.bodyRows.foreach(r => println(r))
  }
}
