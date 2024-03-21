package gameanalyzer

import com.monovore.decline.*
import gameanalyzer.consoleui.Table
import gameanalyzer.model.Building.remoteConstructionFacility
import gameanalyzer.model.{
  Building,
  GameState,
  GameStateRoot,
  Parcel,
  SkillTree
}
import cats.implicits.*
import gameanalyzer.Simulation.SimulationState

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
      blueprint: Option[String],
      saveFile: String
  )

  private val blueprintOpt: Opts[Option[String]] = Opts
    .option[String](
      long = "blueprint",
      short = "b",
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
      blueprintOpt,
      saveFileOpt
    ).mapN(Args.apply)
  }

  def main(rawArgs: Array[String]): Unit =
    command.parse(rawArgs, sys.env) match {
      case Left(help) if help.errors.isEmpty =>
        // help was requested by the user, i.e.: `--help`
        println(help)
        sys.exit(0)

      case Left(help) =>
        // user needs help due to bad/missing arguments
        System.err.println(help)
        sys.exit(1)

      case Right(parsedArgs) =>
        SaveGameLoader.load(parsedArgs.saveFile).map { gsr =>
          run(parsedArgs, gsr)
        }
    }

  def run(args: Args, gsr: GameStateRoot): Unit = {
    val gameState = gsr.gameState

    if args.summaryFlags.buildings then printBuildingSummary(gameState)
    if args.summaryFlags.resources then printResourceSummary(gameState)
    if args.summaryFlags.skills then printSkillTree(gameState.skilltree)

//    printFlowTree(gameState)

    println("")
    println("═══════════════════════════")
    println("")

    val simState = Simulation(gameState).run()
    printDeficitParcels(simState)
  }

  private def printBuildingSummary(gameState: GameState): Unit = {
    val allBuildings =
      gameState.parcels.parcelList
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
      gameState.parcels.parcelList
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
    val parcels: Seq[Parcel] = gameState.parcels.parcelList
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
  }
}
