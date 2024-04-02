package incremental.model
package skills

case class SkillLevelSummary(
    category: SkillCategory,
    level: Int,
    cost: Int,
    description: String,
    numericValue: Double,
    affectedItems: Seq[Item],
    provides: Set[CountedItem]
)
