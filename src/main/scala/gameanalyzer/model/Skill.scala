package gameanalyzer.model

import Item.*
import cats.data.NonEmptyList
import gameanalyzer.{Formatter, StringTemplate}

sealed trait SkillLevels

case class SingleLevel(
    cost: Int
) extends SkillLevels

sealed trait MultiLevels extends SkillLevels {
  def costs: NonEmptyList[Int]

  def applyTemplate(
      affected: String,
      formatter: Formatter,
      level: Int
  ): (Int, Int, String)
}

case class ItemLevels(
    descriptor: StringTemplate,
    costs: NonEmptyList[Int],
    values: NonEmptyList[Set[CountedItem]]
) extends MultiLevels {
  assert(costs.length == values.length)

  def applyTemplate(
      affected: String,
      formatter: Formatter,
      level: Int
  ): (Int, Int, String) = {
    val value = values.toList(level)
    val formattedValue = formatter.formatCommaList(
      value.map(formatter.formatCountedItem).toSeq
    )
    val cost = costs.toList(level)
    val description = descriptor.substitute(
      "$VALUE" -> formattedValue,
      "$AFFECTED" -> affected
    )
    (level, cost, description)
  }

  def levelsWithItem(r: Item): Seq[Int] =
    values.toList.zipWithIndex.collect {
      case (set, level) if set.exists(_.item == r) => level
    }
}

case class NumericLevels(
    descriptor: StringTemplate,
    valueFn: Int => Double,
    costs: NonEmptyList[Int]
) extends MultiLevels {
  def applyTemplate(
      affected: String,
      formatter: Formatter,
      level: Int
  ): (Int, Int, String) = {
    val value = valueFn(level)
    val cost = costs.toList(level)
    val description = descriptor.substitute(
      "$VALUE" -> formatter.formatNumber(value),
      "$PERCENT_VALUE" -> (formatter.formatNumber(value * 100) + "%"),
      "$AFFECTED" -> affected
    )
    (level, cost, description)
  }
}

private val specializationLevels = NumericLevels(
  descriptor = StringTemplate(
    "If a parcel contains only one type of building that produces $AFFECTED, " +
      "increase production rate by $PERCENT_VALUE per building in that parcel."
  ),
  costs = NonEmptyList.of(0, 3, 8, 13, 18, 31, 40, 50),
  valueFn = (level: Int) => level.doubleValue / 100
)

private val throughputOptimizationLevels = NumericLevels(
  descriptor = StringTemplate(
    "Increase the edge throughput for $AFFECTED by ×$VALUE."
  ),
  costs = NonEmptyList.of(0, 3, 8, 13, 18, 31, 40, 50),
  valueFn = (level: Int) => 2.0 * level.doubleValue
)

enum SkillCategory {
  case Unlock, LimitIncrease, Specialization, ThroughputOptimization
}

import SkillCategory.*

object Skills {
  def affecting(r: Item): Seq[Skill] =
    Skill.values.filter(s => s.affectedItems.contains(r))
}

enum Skill(
            val category: SkillCategory,
            val displayName: String,
            val affectedItems: Seq[Item] = Nil,
            val levels: SkillLevels,
            val unlocks: Option[Any] = None,
            lazyConnectedNodes: () => Set[Skill] = () => Set.empty
) {

  lazy val connectedNodes: Set[Skill] = lazyConnectedNodes()

  private def formatAffectedItems(
      formatter: Formatter
  ): String = {
    val entries = affectedItems.map(r => formatter.formatItem(r))
    formatter.formatCommaList(entries, conjunction = "or")
  }

  def makeRows(formatter: Formatter): Seq[(Int, Int, String)] = {
    val formattedItems = formatAffectedItems(formatter)
    levels match {
      case SingleLevel(cost) => Seq((1, cost, displayName))
      case ml: MultiLevels =>
        (1 until ml.costs.length) map (level =>
          ml.applyTemplate(
            formattedItems,
            formatter,
            level
          )
        )
    }
  }

  case starterKitResources
      extends Skill(
        category = Unlock,
        displayName = "Starter Kit (Resources)",
        levels = ItemLevels(
          descriptor = StringTemplate("You start with $VALUE."),
          costs = NonEmptyList.of(0, 1, 3, 8, 25, 30),
          values = NonEmptyList.of(
            Set.empty,
            Set(
              bricks * 100,
              ironPlates * 100,
              stone * 100
            ),
            Set(
              bricks * 200,
              ironPlates * 200,
              stone * 200,
              gears * 100
            ),
            Set(
              bricks * 300,
              ironPlates * 300,
              stone * 300,
              gears * 200,
              copperCables * 100
            ),
            Set(
              bricks * 400,
              ironPlates * 400,
              stone * 400,
              gears * 300,
              copperCables * 200,
              steel * 100
            ),
            Set(
              bricks * 500,
              ironPlates * 500,
              stone * 500,
              gears * 400,
              copperCables * 300,
              steel * 200
            )
          )
        ),
        lazyConnectedNodes = () => Set(oresSpecialization, bricksSpecialization)
      )

  case startWithBlueprintsUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Blueprints Unlocked",
        levels = SingleLevel(cost = 10),
        lazyConnectedNodes = () =>
          Set(
            increaseBuildingLimit,
            increaseNodeConnections,
            increaseEdgeThroughput,
            increaseParcelLimitSkill
          )
      )

  case startWithExpansionUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Expansion Tech Unlocked",
        levels = SingleLevel(cost = 10),
        lazyConnectedNodes = () =>
          Set(
            increaseBuildingLimit,
            increaseNodeConnections,
            increaseEdgeThroughput,
            increaseParcelLimitSkill
          )
      )

  case startWithDroneTechUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Drone Tech Unlocked",
        levels = SingleLevel(cost = 10)
      )

  case startWithAdvancedParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Advanced Parcel Unlocked",
        unlocks = Some(ParcelType.advanced),
        levels = SingleLevel(cost = 20),
        lazyConnectedNodes = () =>
          Set(
            startWithLargeParcelUnlocked
          )
      )

  case startWithLargeParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Large Parcel Unlocked",
        unlocks = Some(ParcelType.large),
        levels = SingleLevel(cost = 25),
        lazyConnectedNodes = () =>
          Set(
            startWithNodeParcelUnlocked
          )
      )

  case startWithNodeParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Node Parcel Unlocked",
        unlocks = Some(ParcelType.node),
        levels = SingleLevel(cost = 30),
        lazyConnectedNodes = () =>
          Set(
            startWithHiTechParcelUnlocked
          )
      )

  case startWithHiTechParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Hi-Tech Parcel Unlocked",
        unlocks = Some(ParcelType.hitech),
        levels = SingleLevel(cost = 35)
      )

  case increaseBuildingLimit
      extends Skill(
        category = LimitIncrease,
        displayName = "Increase Building Limit",
        levels = NumericLevels(
          descriptor = StringTemplate(
            "Increase the parcel building limit & storage capacity by $PERCENT_VALUE."
          ),
          costs = NonEmptyList.of(0, 1, 8, 13, 26, 37, 51, 65, 81, 92, 100),
          valueFn = (level: Int) => level * 0.2d
        ),
        lazyConnectedNodes = () => Set(oresSpecialization, bricksSpecialization)
      )

  case increaseNodeConnections
      extends Skill(
        category = LimitIncrease,
        displayName = "Increase Node Connections",
        levels = NumericLevels(
          descriptor = StringTemplate(
            "Increase parcel node connections by $PERCENT_VALUE."
          ),
          costs = NonEmptyList.of(0, 15, 50, 240, 480),
          valueFn = (level: Int) => level / 2.0d
        )
      )

  case increaseParcelLimitSkill
      extends Skill(
        category = LimitIncrease,
        displayName = "Increase Parcel Limit",
        levels = NumericLevels(
          descriptor = StringTemplate(
            "Increase the total parcel limit by $PERCENT_VALUE."
          ),
          costs =
            NonEmptyList.of(0, 8, 16, 23, 31, 40, 120, 240, 480, 960, 1920),
          valueFn = (level: Int) => level * 0.2d
        )
      )

  case increaseEdgeThroughput
      extends Skill(
        category = LimitIncrease,
        displayName = "Increase Edge Throughput",
        levels = NumericLevels(
          descriptor = StringTemplate(
            "Increase parcel edge throughput to $VALUE."
          ),
          costs = NonEmptyList.of(0, 3, 8, 13, 18, 31, 40, 50),
          valueFn = (level: Int) => 8.0 * (level + 1)
        ),
        lazyConnectedNodes = () =>
          Set(oresThroughputOptimization, bricksThroughputOptimization)
      )

  case oresSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Ores Specialization",
        affectedItems = Seq(stone, ironOre, copperOre, bauxiteOre),
        levels = specializationLevels,
        lazyConnectedNodes = () => Set(basicSmeltingSpecialization)
      )

  case bricksSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Bricks Specialization",
        affectedItems = Seq(bricks),
        levels = specializationLevels,
        lazyConnectedNodes = () => Set(hydrocarbonsSpecialization)
      )

  case hydrocarbonsSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Hydrocarbons Specialization",
        affectedItems = Seq(coal, oilBarrel),
        levels = specializationLevels,
        lazyConnectedNodes = () =>
          Set(bioreactorSpecialization, coalAshSpecialization)
      )

  case coalAshSpecialization
      extends Skill(
        category = Specialization,
        displayName = "CoalAsh Specialization",
        affectedItems = Seq(coalAsh),
        levels = specializationLevels,
        lazyConnectedNodes = () => Set(rareEarthSpecialization)
      )

  case basicSmeltingSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Basic Smelting Specialization",
        affectedItems = Seq(ironPlates, copperPlates),
        levels = specializationLevels,
        lazyConnectedNodes = () => Set(advancedSmeltingSpecialization)
      )

  case advancedSmeltingSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Advanced Smelting Specialization",
        affectedItems = Seq(steel, aluminium, hgCopperPlate),
        levels = specializationLevels
      )

  case bioreactorSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Bioreactor Specialization",
        affectedItems = Seq(oxygen),
        levels = specializationLevels
      )

  case rareEarthSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Rare Earth Specialization",
        affectedItems = Seq(hafnium, germanium),
        levels = specializationLevels
      )

  case oresThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Ores Throughput Optimization",
        affectedItems = Seq(stone, ironOre, copperOre, bauxiteOre),
        levels = throughputOptimizationLevels,
        lazyConnectedNodes = () => Set(basicSmeltingThroughputOptimization)
      )

  case bricksThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Bricks Throughput Optimization",
        affectedItems = Seq(bricks),
        levels = throughputOptimizationLevels,
        lazyConnectedNodes = () => Set(hydrocarbonsThroughputOptimization)
      )

  case hydrocarbonsThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Hydrocarbons Throughput Optimization",
        affectedItems = Seq(coal, oilBarrel),
        levels = throughputOptimizationLevels,
        lazyConnectedNodes = () =>
          Set(
            bioreactorThroughputOptimization,
            coalAshThroughputOptimization
          )
      )

  case coalAshThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "CoalAsh Throughput Optimization",
        affectedItems = Seq(coalAsh),
        levels = throughputOptimizationLevels
      )

  case basicSmeltingThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Basic Smelting Throughput Optimization",
        affectedItems = Seq(ironPlates, copperPlates),
        levels = throughputOptimizationLevels,
        lazyConnectedNodes = () => Set(advancedSmeltingThroughputOptimization)
      )

  case advancedSmeltingThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Advanced Smelting Throughput Optimization",
        affectedItems = Seq(steel, aluminium, hgCopperPlate),
        levels = throughputOptimizationLevels
      )

  case bioreactorThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Bioreactor Throughput Optimization",
        affectedItems = Seq(oxygen),
        levels = throughputOptimizationLevels
      )

}
