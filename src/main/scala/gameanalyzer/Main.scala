package gameanalyzer

import com.github.plokhotnyuk.jsoniter_scala.core.*
import gameanalyzer.consoleui.Table
import gameanalyzer.model.Building.remoteConstructionFacility
import gameanalyzer.model.{
  Building,
  GameState,
  GameStateRoot,
  Parcel,
  SkillTree,
  SkillTreeNode
}

import scala.util.Using
import model.ModelCodecs.given

object Main {

  def main(args: Array[String]): Unit = {
    val tryGameStateRoot = Using(
      io.Source.fromInputStream(
        getClass.getResourceAsStream("/savegame-reference.json")
      )
    ) { src =>
      val jsonText = src.mkString
      readFromString[GameStateRoot](jsonText)
    }
    val gameState = tryGameStateRoot.get.gameState

    val allBuildings =
      gameState.parcels.parcelList
        .flatMap(_.buildings)
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sum)
        .toMap
    val allResources =
      gameState.parcels.parcelList
        .flatMap(_.resources)
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sum)
        .toMap

    println(
      Table(
        Seq("Building ID", "Name", "Qty"),
        allBuildings.toList
          .sortBy(_._1.ordinal())
          .map((b, v) => Seq(b.name, b.displayName, v))
      ).withBorders().toStringBlock
    )

    println(
      Table(
        Seq("Resource ID", "Name", "Qty", "Throughput", "Per-Building Mult"),
        allResources.toList
          .sortBy(_._1.ordinal())
          .map((r, v) =>
            Seq(
              r.name,
              r.displayName,
              v.toInt,
              gameState.skilltree.maxThroughputFor(r),
              gameState.skilltree.specializationMultiplierFor(r)
            )
          )
      ).withBorders().toStringBlock
    )

    printSkillTree(gameState.skilltree)
    // printFlowTree(gameState)

//    pprint.pprintln(gameState.skilltree)

    val starvedParcels = Simulation(gameState).run().starvedParcels
    pprint.pprintln(starvedParcels)

  }

  private def printSkillTree(skillTree: SkillTree): Unit = {
    val allNodes = skillTree.nodes
    val nonRootNodeIds = allNodes.flatMap(_.connectedNodes).toSet
    val rootNodes = allNodes.filterNot(n => nonRootNodeIds.contains(n.id))

    val table = Table.buildTreeTable(
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
