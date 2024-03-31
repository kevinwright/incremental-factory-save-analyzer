package gameanalyzer

import gameanalyzer.consoleui.Table
import gameanalyzer.model.Building.remoteConstructionFacility
import gameanalyzer.model.{GameState, ParcelInstance, SaveSkillTree}

class Summaries(gameState: GameState) {
  lazy val allBuildings =
    gameState.parcels.instances
      .flatMap(_.buildings)
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sum)
      .toMap

  lazy val allResources =
    gameState.parcels.instances
      .flatMap(_.resources)
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sum)
      .toMap

  def buildingsTable: Table =
    Table(
      Some("Buildings"),
      Seq("Building ID", "Name", "Qty"),
      allBuildings.toList
        .sortBy(_._1.ordinal())
        .map((b, v) => Seq(b.name, b.displayName, v))
    )

  def resourcesTable: Table =
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
    )

  def skillsTreeTable: Table = {
    val allNodes = gameState.skilltree.nodes
    val nonRootNodeIds = allNodes.flatMap(_.connectedNodeIds).toSet
    val rootNodes = allNodes.filterNot(n => nonRootNodeIds.contains(n.id))

    Table.buildTreeTable(
      title = Some("Skills"),
      headerRow = Seq("ID", "Name", "Type", "Level", "Value"),
      roots = rootNodes,
      walkDown =
        node => allNodes.filter(n => node.connectedNodeIds.contains(n.id)),
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
  }

  def flowTreeTable: Table = {
    val parcels: Seq[ParcelInstance] = gameState.parcels.instances
    val connections = gameState.nodeConnections

    val exporterIds = connections.map(_.sourceId).toSet
    val roots = parcels.filter(p =>
      !exporterIds.contains(p.id) || p.buildings.contains(
        remoteConstructionFacility
      )
    )

    Table.buildTreeTable(
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
  }
}
