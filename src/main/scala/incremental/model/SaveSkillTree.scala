package incremental.model

case class SaveSkillTree(
    nodes: Seq[SaveSkillTreeNode]
) {
  def specializationNodes = nodes.filter(_.skillType == Some("specialization"))
  def throughputOptimizationNodes =
    nodes.filter(_.skillType == Some("throughputOptimization"))

  def baseThroughput: Double =
    valueFor(nodeId = "increaseEdgeThroughput", default = 8.0d)

  def maxThroughputFor(resource: Item): Double = {
    val multiplier = throughputOptimizationNodes
      .find(_.affectedResources.contains(resource.name))
      .flatMap(_.currentSimpleValue)
      .getOrElse(1.0d)
    baseThroughput * multiplier
  }

  def specializationBoostFor(resource: Item): Double = {
    val pctBoost = specializationNodes
      .find(_.affectedResources.contains(resource.name))
      .flatMap(_.currentSimpleValue)
      .getOrElse(0.0d)
    pctBoost / 100.0d
  }

  def valueFor(nodeId: String, default: Double = 0.0d): Double =
    nodes
      .find(_.id == nodeId)
      .flatMap(_.currentSimpleValue)
      .getOrElse(default)
}
