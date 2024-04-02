package incremental.model
package skills

import cats.data.NonEmptyList
import incremental.{Formatter, StringTemplate}

sealed trait SkillLevelDescriptor {
  private[skills] def summarizeLevels(
      formatter: Formatter,
      skillDisplayName: String,
      category: SkillCategory,
      affectedItems: Seq[Item]
  ): NonEmptyList[SkillLevelSummary]
}

object SkillLevelDescriptor {
  private def nonEmptyRange(r: Range): NonEmptyList[Int] =
    NonEmptyList.fromListUnsafe(r.toList)

  private def formatAffectedItems(
      affectedItems: Seq[Item],
      formatter: Formatter
  ): String = {
    val entries = affectedItems.map(r => formatter.formatItem(r))
    formatter.formatCommaList(entries, conjunction = "or")
  }

  case class SingleLevel(
      cost: Int
  ) extends SkillLevelDescriptor {
    private[skills] def summarizeLevels(
        formatter: Formatter,
        skillDisplayName: String,
        category: SkillCategory,
        affectedItems: Seq[Item]
    ): NonEmptyList[SkillLevelSummary] =
      NonEmptyList.of(
        SkillLevelSummary(
          category = category,
          level = 1,
          cost = cost,
          description = skillDisplayName,
          numericValue = Double.NaN,
          affectedItems = affectedItems,
          provides = Set.empty
        )
      )
  }

  sealed trait MultiLevels extends SkillLevelDescriptor {
    def descriptionTemplate: StringTemplate
    def costs: NonEmptyList[Int]
  }

  case class ItemLevels(
      descriptionTemplate: StringTemplate,
      costs: NonEmptyList[Int],
      values: NonEmptyList[Set[CountedItem]]
  ) extends MultiLevels {
    assert(costs.length == values.length)
    assert(costs.length > 1)

    private[skills] def summarizeLevels(
        formatter: Formatter,
        skillDisplayName: String,
        category: SkillCategory,
        affectedItems: Seq[Item]
    ): NonEmptyList[SkillLevelSummary] = {

      nonEmptyRange(1 until costs.length) map { level =>
        val value = values.toList(level)
        val formattedValue = formatter.formatCommaList(
          value.map(formatter.formatCountedItem).toSeq
        )
        val cost = costs.toList(level)
        val description = descriptionTemplate.substitute(
          "$VALUE" -> formattedValue,
          "$AFFECTED" -> formatAffectedItems(affectedItems, formatter)
        )
        SkillLevelSummary(
          category = category,
          level = level,
          cost = cost,
          description = description,
          numericValue = Double.NaN,
          affectedItems = affectedItems,
          provides = values.toList(level)
        )
      }
    }
  }

  case class NumericLevels(
      descriptionTemplate: StringTemplate,
      valueFn: Int => Double,
      costs: NonEmptyList[Int]
  ) extends MultiLevels {
    private[skills] def summarizeLevels(
        formatter: Formatter,
        skillDisplayName: String,
        category: SkillCategory,
        affectedItems: Seq[Item]
    ): NonEmptyList[SkillLevelSummary] = {
      val returnSeq = (1 until costs.length) map { level =>
        val value = valueFn(level)
        val cost = costs.toList(level)
        val description = descriptionTemplate.substitute(
          "$VALUE" -> formatter.formatNumber(value),
          "$PERCENT_VALUE" -> (formatter.formatNumber(value * 100) + "%"),
          "$AFFECTED" -> formatAffectedItems(affectedItems, formatter)
        )
        SkillLevelSummary(
          category = category,
          level = level,
          cost = cost,
          description = description,
          numericValue = value,
          affectedItems = affectedItems,
          provides = Set.empty
        )
      }
      NonEmptyList.fromListUnsafe(returnSeq.toList)
    }
  }

  val specializationLevels: NumericLevels = NumericLevels(
    descriptionTemplate = StringTemplate(
      "If a parcel contains only one type of building that produces $AFFECTED, " +
        "increase production rate by $PERCENT_VALUE per building in that parcel."
    ),
    costs = NonEmptyList.of(0, 3, 8, 13, 18, 31, 40, 50),
    valueFn = (level: Int) => level.doubleValue / 100
  )

  val throughputOptimizationLevels: NumericLevels = NumericLevels(
    descriptionTemplate = StringTemplate(
      "Increase the edge throughput for $AFFECTED by ×$VALUE."
    ),
    costs = NonEmptyList.of(0, 3, 8, 13, 18, 31, 40, 50),
    valueFn = (level: Int) => 2.0 * level.doubleValue
  )
}
