package gameanalyzer.model

case class ParcelLimits(
    connections: Int,
    buildings: Int
) {
  def storage: Int =
    math.floor((1 + buildings.doubleValue / 8.0d) * 250.0d).intValue

  def withIncreaseBuildingLimit(pctBoost: Double): ParcelLimits = copy(
    buildings = math.round(buildings.doubleValue * (1.0d + pctBoost)).intValue
  )

  def withIncreaseNodeConnections(pctBoost: Double): ParcelLimits = copy(
    connections =
      math.round(connections.doubleValue * (1.0d + pctBoost)).intValue
  )

  def withSkills(skillTree: SaveSkillTree): ParcelLimits = {
    val buildingLimitBoost: Double =
      skillTree.valueFor("increaseBuildingLimit")
    val nodeConnectionBoost: Double =
      skillTree.valueFor("increaseNodeConnections")
    this
      .withIncreaseBuildingLimit(buildingLimitBoost)
      .withIncreaseNodeConnections(nodeConnectionBoost)
  }
}
