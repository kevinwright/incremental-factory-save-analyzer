package gameanalyzer.model

case class SkillTree(
    nodes: Seq[SkillTreeNode]
) {
  def specializationNodes = nodes.filter(_.skillType == Some("specialization"))
  def throughputOptimizationNodes =
    nodes.filter(_.skillType == Some("throughputOptimization"))

  def baseThroughput: Double = nodes
    .find(_.id == "increaseEdgeThroughput")
    .flatMap(_.currentSimpleValue)
    .getOrElse(8.0d)

  def maxThroughputFor(resource: Resource): Double = {
    val multiplier = throughputOptimizationNodes
      .find(_.affectedResources.contains(resource.name))
      .flatMap(_.currentSimpleValue)
      .getOrElse(1.0d)
    baseThroughput * multiplier
  }

  def specializationMultiplierFor(resource: Resource): Double = {
    val pctBoost = specializationNodes
      .find(_.affectedResources.contains(resource.name))
      .flatMap(_.currentSimpleValue)
      .getOrElse(0.0d)
    1.0d + (pctBoost / 100)
  }
}
