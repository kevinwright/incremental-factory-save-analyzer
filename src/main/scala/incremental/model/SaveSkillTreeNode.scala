package incremental.model

import scala.collection.immutable.ListMap
import com.github.plokhotnyuk.jsoniter_scala.macros.named

sealed trait SaveSkillTreeNodeValues

object SaveSkillTreeNodeValues {
  case class Simple(
      map: Map[String, Double]
  ) extends SaveSkillTreeNodeValues

  case class Compound(
      map: Map[String, Map[String, Int]]
  ) extends SaveSkillTreeNodeValues

  case object Empty extends SaveSkillTreeNodeValues
}

case class SaveSkillTreeNode(
    name: String,
    id: String,
    @named("type") skillType: Option[String],
    levels: Int,
    cost: Seq[Int],
    currentLevel: Int,
    description: Seq[String],
    values: ListMap[String, Double] | ListMap[String, Map[String, Int]],
    @named("values2") affectedResources: Seq[String],
    @named("connectedNodes") connectedNodeIds: Seq[String]
) {
  def currentLevelDesc: String =
    if currentLevel <= 0 then "n/a"
    else description(currentLevel - 1)

  def currentValue: Option[Double | Map[String, Int]] =
    values.get(currentLevel.toString)
  def currentSimpleValue: Option[Double] = currentValue.collect {
    case d: Double => d
  }
  def currentCompoundValue: Option[Map[String, Int]] = currentValue.collect {
    case m: Map[_, _] => m
  }

}
