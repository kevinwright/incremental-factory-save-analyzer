package incremental.model
package skills

import cats.data.NonEmptyList
import incremental.StringTemplate
import incremental.Formatter
import incremental.model.Item.*
import incremental.model.{Item, ParcelType}
import SkillCategory.*
import SkillLevelDescriptor.*

enum Skill(
    val category: SkillCategory,
    val displayName: String,
    val affectedItems: Seq[Item] = Nil,
    val levelDescriptors: SkillLevelDescriptor,
    val unlocks: Unlock = NoUnlock,
    lazyConnectedNodes: () => Set[Skill] = () => Set.empty
) {

  lazy val connectedNodes: Set[Skill] = lazyConnectedNodes()

//  def makeRows(formatter: Formatter): Seq[(Int, Int, String)] =
//    summarizeLevels(formatter).toList.map(sls =>
//      (sls.level, sls.cost, sls.description)
//    )

  def summarizeLevels(formatter: Formatter): NonEmptyList[SkillLevelSummary] =
    levelDescriptors.summarizeLevels(
      formatter,
      displayName,
      category,
      affectedItems
    )

  case starterKitResources
      extends Skill(
        category = Unlock,
        displayName = "Starter Kit (Resources)",
        levelDescriptors = ItemLevels(
          descriptionTemplate = StringTemplate("You start with $VALUE."),
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
        levelDescriptors = SingleLevel(cost = 10),
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
        levelDescriptors = SingleLevel(cost = 10),
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
        levelDescriptors = SingleLevel(cost = 10)
      )

  case startWithAdvancedParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Advanced Parcel Unlocked",
        unlocks = UnlockParcelType(ParcelType.advanced),
        levelDescriptors = SingleLevel(cost = 20),
        lazyConnectedNodes = () =>
          Set(
            startWithLargeParcelUnlocked
          )
      )

  case startWithLargeParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Large Parcel Unlocked",
        unlocks = UnlockParcelType(ParcelType.large),
        levelDescriptors = SingleLevel(cost = 25),
        lazyConnectedNodes = () =>
          Set(
            startWithNodeParcelUnlocked
          )
      )

  case startWithNodeParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Node Parcel Unlocked",
        unlocks = UnlockParcelType(ParcelType.node),
        levelDescriptors = SingleLevel(cost = 30),
        lazyConnectedNodes = () =>
          Set(
            startWithHiTechParcelUnlocked
          )
      )

  case startWithHiTechParcelUnlocked
      extends Skill(
        category = Unlock,
        displayName = "Start with Hi-Tech Parcel Unlocked",
        unlocks = UnlockParcelType(ParcelType.hitech),
        levelDescriptors = SingleLevel(cost = 35)
      )

  case increaseBuildingLimit
      extends Skill(
        category = LimitIncrease,
        displayName = "Increase Building Limit",
        levelDescriptors = NumericLevels(
          descriptionTemplate = StringTemplate(
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
        levelDescriptors = NumericLevels(
          descriptionTemplate = StringTemplate(
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
        levelDescriptors = NumericLevels(
          descriptionTemplate = StringTemplate(
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
        levelDescriptors = NumericLevels(
          descriptionTemplate = StringTemplate(
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
        levelDescriptors = specializationLevels,
        lazyConnectedNodes = () => Set(basicSmeltingSpecialization)
      )

  case bricksSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Bricks Specialization",
        affectedItems = Seq(bricks),
        levelDescriptors = specializationLevels,
        lazyConnectedNodes = () => Set(hydrocarbonsSpecialization)
      )

  case hydrocarbonsSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Hydrocarbons Specialization",
        affectedItems = Seq(coal, oilBarrel),
        levelDescriptors = specializationLevels,
        lazyConnectedNodes = () =>
          Set(bioreactorSpecialization, coalAshSpecialization)
      )

  case coalAshSpecialization
      extends Skill(
        category = Specialization,
        displayName = "CoalAsh Specialization",
        affectedItems = Seq(coalAsh),
        levelDescriptors = specializationLevels,
        lazyConnectedNodes = () => Set(rareEarthSpecialization)
      )

  case basicSmeltingSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Basic Smelting Specialization",
        affectedItems = Seq(ironPlates, copperPlates),
        levelDescriptors = specializationLevels,
        lazyConnectedNodes = () => Set(advancedSmeltingSpecialization)
      )

  case advancedSmeltingSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Advanced Smelting Specialization",
        affectedItems = Seq(steel, aluminium, hgCopperPlate),
        levelDescriptors = specializationLevels
      )

  case bioreactorSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Bioreactor Specialization",
        affectedItems = Seq(oxygen),
        levelDescriptors = specializationLevels
      )

  case rareEarthSpecialization
      extends Skill(
        category = Specialization,
        displayName = "Rare Earth Specialization",
        affectedItems = Seq(hafnium, germanium),
        levelDescriptors = specializationLevels
      )

  case oresThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Ores Throughput Optimization",
        affectedItems = Seq(stone, ironOre, copperOre, bauxiteOre),
        levelDescriptors = throughputOptimizationLevels,
        lazyConnectedNodes = () => Set(basicSmeltingThroughputOptimization)
      )

  case bricksThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Bricks Throughput Optimization",
        affectedItems = Seq(bricks),
        levelDescriptors = throughputOptimizationLevels,
        lazyConnectedNodes = () => Set(hydrocarbonsThroughputOptimization)
      )

  case hydrocarbonsThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Hydrocarbons Throughput Optimization",
        affectedItems = Seq(coal, oilBarrel),
        levelDescriptors = throughputOptimizationLevels,
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
        levelDescriptors = throughputOptimizationLevels
      )

  case basicSmeltingThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Basic Smelting Throughput Optimization",
        affectedItems = Seq(ironPlates, copperPlates),
        levelDescriptors = throughputOptimizationLevels,
        lazyConnectedNodes = () => Set(advancedSmeltingThroughputOptimization)
      )

  case advancedSmeltingThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Advanced Smelting Throughput Optimization",
        affectedItems = Seq(steel, aluminium, hgCopperPlate),
        levelDescriptors = throughputOptimizationLevels
      )

  case bioreactorThroughputOptimization
      extends Skill(
        category = ThroughputOptimization,
        displayName = "Bioreactor Throughput Optimization",
        affectedItems = Seq(oxygen),
        levelDescriptors = throughputOptimizationLevels
      )

}
