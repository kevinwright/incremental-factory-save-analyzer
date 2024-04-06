package incremental.model

import TechTier.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

enum ItemCategory {
  case Raw, Basic, Advanced, Points, Military, Rocket, Science, Helper
}

import ItemCategory.*

object Item {
  def ordered: Seq[Item] =
    Item.values.toSeq
      .sortBy(_.ordinal())
      .filterNot(_ == Item.nullItem)

  given codec: JsonValueCodec[Item] =
    JsonCodecMaker.makeWithoutDiscriminator
}
enum Item(
    val displayName: String,
    val category: ItemCategory,
    val techTier: TechTier,
    val order: Int,
    val icon: String
) extends Enum[Item] {
  case nullItem
      extends Item(
        displayName = "<None>",
        category = Basic,
        techTier = Foundation,
        order = 1,
        icon = "null-96.png"
      )
  case stone
      extends Item(
        displayName = "Stone",
        category = Basic,
        techTier = Foundation,
        order = 1,
        icon = "stone-96.png"
      )
  case bricks
      extends Item(
        displayName = "Bricks",
        category = Basic,
        techTier = Foundation,
        order = 1,
        icon = "bricks-96.png"
      )
  case ironOre
      extends Item(
        displayName = "Iron Ore",
        category = Basic,
        techTier = Foundation,
        order = 3,
        icon = "ironOre-96.png"
      )
  case ironPlates
      extends Item(
        displayName = "Iron Plates",
        category = Basic,
        techTier = Foundation,
        order = 2,
        icon = "ironPlates-96.png"
      )
  case gears
      extends Item(
        displayName = "Gears",
        category = Basic,
        techTier = Foundation,
        order = 4,
        icon = "gears-96.png"
      )
  case coal
      extends Item(
        displayName = "Coal",
        category = Raw,
        techTier = Foundation,
        order = 2,
        icon = "coal-96.png"
      )
  case coalAsh
      extends Item(
        displayName = "Coal Ash",
        category = Basic,
        techTier = Foundation,
        order = 19,
        icon = "coalAsh-96.png"
      )
  case copperOre
      extends Item(
        displayName = "Copper Ore",
        category = Raw,
        techTier = Foundation,
        order = 4,
        icon = "copperOre-96.png"
      )
  case copperPlates
      extends Item(
        displayName = "Copper Plates",
        category = Basic,
        techTier = Foundation,
        order = 3,
        icon = "copperPlates-96.png"
      )
  case copperCables
      extends Item(
        displayName = "Copper Cables",
        category = Basic,
        techTier = Foundation,
        order = 5,
        icon = "copperCables-96.png"
      )
  case redScience
      extends Item(
        displayName = "Red Science",
        category = Science,
        techTier = Foundation,
        order = 3,
        icon = "redScience-96.png"
      )
  case steel
      extends Item(
        displayName = "Steel",
        category = Basic,
        techTier = Steel,
        order = 6,
        icon = "steel-96.png"
      )
  case quartz
      extends Item(
        displayName = "Quartz",
        category = Basic,
        techTier = Steel,
        order = 18,
        icon = "quartz-96.png"
      )
  case silicon
      extends Item(
        displayName = "Silicon",
        category = Advanced,
        techTier = Steel,
        order = 3,
        icon = "silicon-96.png"
      )
  case bauxiteOre
      extends Item(
        displayName = "Bauxite Ore",
        category = Raw,
        techTier = Steel,
        order = 6,
        icon = "bauxiteOre-96.png"
      )
  case aluminium
      extends Item(
        displayName = "Aluminium",
        category = Basic,
        techTier = Steel,
        order = 13,
        icon = "aluminium-96.png"
      )
  case gen1Chip
      extends Item(
        displayName = "Gen 1 Chip",
        category = Advanced,
        techTier = Gen1,
        order = 1,
        icon = "gen1Chip-96.png"
      )
  case greenScience
      extends Item(
        displayName = "Green Science",
        category = Science,
        techTier = Gen1,
        order = 5,
        icon = "greenScience-96.png"
      )
  case oilBarrel
      extends Item(
        displayName = "Oil Barrel",
        category = Raw,
        techTier = Gen1,
        order = 5,
        icon = "oilBarrel-96.png"
      )
  case petroleumBarrel
      extends Item(
        displayName = "Petroleum Barrel",
        category = Basic,
        techTier = Gen1,
        order = 7,
        icon = "petroleumBarrel-96.png"
      )
  case plastics
      extends Item(
        displayName = "Plastics",
        category = Basic,
        techTier = Gen1,
        order = 8,
        icon = "plastics-96.png"
      )
  case sulfur
      extends Item(
        displayName = "Sulfur",
        category = Basic,
        techTier = Gen1,
        order = 9,
        icon = "sulfur-96.png"
      )
  case oxygen
      extends Item(
        displayName = "Oxygen",
        category = Raw,
        techTier = Gen1,
        order = 7,
        icon = "oxygen-96.png"
      )
  case siliconDioxide
      extends Item(
        displayName = "Silicon Dioxide",
        category = Advanced,
        techTier = Gen1,
        order = 11,
        icon = "siliconDioxide-96.png"
      )
  case gen2Chip
      extends Item(
        displayName = "Gen 2 Chip",
        category = Advanced,
        techTier = Gen2,
        order = 2,
        icon = "gen2Chip-96.png"
      )
  case drone
      extends Item(
        displayName = "Drone",
        category = Basic,
        techTier = Gen2,
        order = 19,
        icon = "drone-96.png"
      )
  case blueScience
      extends Item(
        displayName = "Blue Science",
        category = Science,
        techTier = Gen2,
        order = 6,
        icon = "blueScience-96.png"
      )
  case phenolicResin
      extends Item(
        displayName = "Phenolic Resin",
        category = Basic,
        techTier = Gen2,
        order = 17,
        icon = "phenolicResin-96.png"
      )
  case lowkDielectric
      extends Item(
        displayName = "Low-k Dielectric",
        category = Advanced,
        techTier = Gen2,
        order = 3,
        icon = "lowkDielectric-96.png"
      )
  case germanium
      extends Item(
        displayName = "Germanium",
        category = Advanced,
        techTier = Gen2,
        order = 4,
        icon = "germanium-96.png"
      )
  case siliconGermanium
      extends Item(
        displayName = "Silicon Germanium",
        category = Advanced,
        techTier = Gen2,
        order = 4,
        icon = "siliconGermanium-96.png"
      )
  case hgCopperPlate
      extends Item(
        displayName = "High-Grade Copper Plate",
        category = Advanced,
        techTier = Gen2,
        order = 5,
        icon = "hgCopperPlate-96.png"
      )
  case gen3Chip
      extends Item(
        displayName = "Gen 3 Chip",
        category = Advanced,
        techTier = Gen3,
        order = 6,
        icon = "gen3Chip-96.png"
      )
  case purpleScience
      extends Item(
        displayName = "Purple Science",
        category = Science,
        techTier = Gen3,
        order = 7,
        icon = "purpleScience-96.png"
      )
  case hafnium
      extends Item(
        displayName = "Hafnium",
        category = Raw,
        techTier = Gen3,
        order = 7,
        icon = "hafnium-96.png"
      )
  case highkDielectric
      extends Item(
        displayName = "High-k Dielectric",
        category = Advanced,
        techTier = Gen3,
        order = 8,
        icon = "highkDielectric-96.png"
      )
  case metalGate
      extends Item(
        displayName = "Metal Gate",
        category = Advanced,
        techTier = Gen3,
        order = 9,
        icon = "metalGate-96.png"
      )
  case gen4Chip
      extends Item(
        displayName = "Gen 4 Chip",
        category = Advanced,
        techTier = Gen4,
        order = 10,
        icon = "gen4Chip-96.png"
      )
  case yellowScience
      extends Item(
        displayName = "Yellow Science",
        category = Science,
        techTier = Gen4,
        order = 8,
        icon = "yellowScience-96.png"
      )
  case graphene
      extends Item(
        displayName = "Graphene",
        category = Advanced,
        techTier = Gen4,
        order = 11,
        icon = "graphene-96.png"
      )
  case hafniumDisulfide
      extends Item(
        displayName = "Hafnium Disulfide",
        category = Advanced,
        techTier = Gen4,
        order = 12,
        icon = "hafniumDisulfide-96.png"
      )
  case nanowires
      extends Item(
        displayName = "Nanowires",
        category = Advanced,
        techTier = Gen4,
        order = 13,
        icon = "nanowires-96.png"
      )
  case apMaterial
      extends Item(
        displayName = "AP Material",
        category = Advanced,
        techTier = Gen4,
        order = 14,
        icon = "apMaterial-96.png"
      )
  case gen5Chip
      extends Item(
        displayName = "Gen 5 Chip",
        category = Advanced,
        techTier = Gen5,
        order = 15,
        icon = "gen5Chip-96.png"
      )
  case rocketControlUnit
      extends Item(
        displayName = "Rocket Control Unit",
        category = Advanced,
        techTier = Gen5,
        order = 9,
        icon = "rocketControlUnit-96.png"
      )
  case fuelCell
      extends Item(
        displayName = "Fuel Cell",
        category = Advanced,
        techTier = Gen5,
        order = 7,
        icon = "fuelCell-96.png"
      )
  case lightDensityStructures
      extends Item(
        displayName = "Light Density Structures",
        category = Advanced,
        techTier = Gen5,
        order = 8,
        icon = "lightDensityStructures-96.png"
      )
  case rocketParts
      extends Item(
        displayName = "Rocket Parts",
        category = Science,
        techTier = Gen5,
        order = 1,
        icon = "rocketParts-96.png"
      )
  case satelite
      extends Item(
        displayName = "Satellite",
        category = Rocket,
        techTier = Gen5,
        order = 2,
        icon = "satelite-96.png"
      )
  case whiteScience
      extends Item(
        displayName = "White Science",
        category = Science,
        techTier = Gen5,
        order = 9,
        icon = "whiteScience-96.png"
      )

  /////////////////////////////////////
  // Currently not seen in save files

  /*
  case greenChips
      extends Resource(
        displayName = "Green Chips",
        category = Advanced,
        techTier = Foundation,
        order = 1,
        icon = "greenChips-96.png"
      )
  case redChips
      extends Resource(
        displayName = "Red Chips",
        category = Advanced,
        techTier = Foundation,
        order = 2,
        icon = "redChips-96.png"
      )
  case researchPoints
      extends Resource(
        displayName = "Research Points",
        category = Points,
        techTier = Foundation,
        order = 1,
        icon = "researchPoints-96.png"
      )
  case expansionPoints
      extends Resource(
        displayName = "Expansion Points",
        category = Points,
        techTier = Foundation,
        order = 2,
        icon = "expansionPoints-96.png"
      )
  case alienArtefacts
      extends Resource(
        displayName = "Alien Artifacts",
        category = Points,
        techTier = Foundation,
        order = 3,
        icon = "alienArtefacts-96.png"
      )
  case standardAmmunition
      extends Resource(
        displayName = "Standard Ammunition",
        category = Military,
        techTier = Foundation,
        order = 1,
        icon = "standardAmmunition-96.png"
      )
  case armorPenetratingAmmunition
      extends Resource(
        displayName = "Armor Penetrating Ammunition",
        category = Military,
        techTier = Foundation,
        order = 2,
        icon = "armorPenetratingAmmunition-96.png"
      )
  case piercingAmmunition
      extends Resource(
        displayName = "Piercing Ammunition",
        category = Military,
        techTier = Foundation,
        order = 3,
        icon = "piercingAmmunition-96.png"
      )
  case pipe
      extends Resource(
        displayName = "Pipe",
        category = Basic,
        techTier = Foundation,
        order = 10,
        icon = "pipe-96.png"
      )
  case engineUnit
      extends Resource(
        displayName = "Engine Unit",
        category = Advanced,
        techTier = Foundation,
        order = 4,
        icon = "engineUnit-96.png"
      )
  case ironRod
      extends Resource(
        displayName = "Iron Rod",
        category = Basic,
        techTier = Foundation,
        order = 11,
        icon = "ironRod-96.png"
      )
  case rail
      extends Resource(
        displayName = "Rail",
        category = Basic,
        techTier = Foundation,
        order = 12,
        icon = "rail-96.png"
      )
  case electricalEngineUnits
      extends Resource(
        displayName = "Electrical Engine Units",
        category = Advanced,
        techTier = Foundation,
        order = 5,
        icon = "electricalEngineUnits-96.png"
      )
  case battery
      extends Resource(
        displayName = "Battery",
        category = Advanced,
        techTier = Foundation,
        order = 6,
        icon = "battery-96.png"
      )
  case blueChips
      extends Resource(
        displayName = "Blue Chips",
        category = Advanced,
        techTier = Foundation,
        order = 10,
        icon = "blueChips-96.png"
      )
  case turret
      extends Resource(
        displayName = "Turret",
        category = Military,
        techTier = Foundation,
        order = 4,
        icon = "turret-96.png"
      )
  case wall
      extends Resource(
        displayName = "Wall",
        category = Military,
        techTier = Foundation,
        order = 5,
        icon = "wall-96.png"
      )
  case darkScience
      extends Resource(
        displayName = "Dark Science",
        category = Science,
        techTier = Foundation,
        order = 4,
        icon = "darkScience-96.png"
      )
  case inputResource
      extends Resource(
        displayName = "Rocket",
        category = Helper,
        techTier = Foundation,
        order = 3,
        icon = "input-96.png"
      )
  case highGradeCopper
      extends Resource(
        displayName = "High Grade Copper",
        category = Advanced,
        techTier = Foundation,
        order = 16,
        icon = "highGradeCopper-96.png"
      )
   */
}

case class CountedItem(
    item: Item,
    qty: Double
)

extension (i: Item) def *(qty: Double): CountedItem = CountedItem(i, qty)
