package gameanalyzer.model

enum ResourceCategory {
  case Raw, Basic, Advanced, Points, Military, Rocket, Science, Helper
}

import ResourceCategory.*
import TechTier.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object Resource {
  given codec: JsonValueCodec[Resource] =
    JsonCodecMaker.makeWithoutDiscriminator
}
enum Resource(
    val displayName: String,
    val category: ResourceCategory,
    val techTier: TechTier,
    val order: Int,
    val icon: String
) extends Enum[Resource] {
  case nullResource
      extends Resource(
        displayName = "<None>",
        category = Basic,
        techTier = Foundation,
        order = 1,
        icon = "null-96.png"
      )
  case stone
      extends Resource(
        displayName = "Stone",
        category = Basic,
        techTier = Foundation,
        order = 1,
        icon = "stone-96.png"
      )
  case bricks
      extends Resource(
        displayName = "Bricks",
        category = Basic,
        techTier = Foundation,
        order = 1,
        icon = "bricks-96.png"
      )
  case ironOre
      extends Resource(
        displayName = "Iron Ore",
        category = Basic,
        techTier = Foundation,
        order = 3,
        icon = "ironOre-96.png"
      )
  case ironPlates
      extends Resource(
        displayName = "Iron Plates",
        category = Basic,
        techTier = Foundation,
        order = 2,
        icon = "ironPlates-96.png"
      )
  case gears
      extends Resource(
        displayName = "Gears",
        category = Basic,
        techTier = Foundation,
        order = 4,
        icon = "gears-96.png"
      )
  case coal
      extends Resource(
        displayName = "Coal",
        category = Raw,
        techTier = Foundation,
        order = 2,
        icon = "coal-96.png"
      )
  case coalAsh
      extends Resource(
        displayName = "Coal Ash",
        category = Basic,
        techTier = Foundation,
        order = 19,
        icon = "coalAsh-96.png"
      )
  case copperOre
      extends Resource(
        displayName = "Copper Ore",
        category = Raw,
        techTier = Foundation,
        order = 4,
        icon = "copperOre-96.png"
      )
  case copperPlates
      extends Resource(
        displayName = "Copper Plates",
        category = Basic,
        techTier = Foundation,
        order = 3,
        icon = "copperPlates-96.png"
      )
  case copperCables
      extends Resource(
        displayName = "Copper Cables",
        category = Basic,
        techTier = Foundation,
        order = 5,
        icon = "copperCables-96.png"
      )
  case redScience
      extends Resource(
        displayName = "Red Science",
        category = Science,
        techTier = Foundation,
        order = 3,
        icon = "redScience-96.png"
      )
  case steel
      extends Resource(
        displayName = "Steel",
        category = Basic,
        techTier = Steel,
        order = 6,
        icon = "steel-96.png"
      )
  case quartz
      extends Resource(
        displayName = "Quartz",
        category = Basic,
        techTier = Steel,
        order = 18,
        icon = "quartz-96.png"
      )
  case silicon
      extends Resource(
        displayName = "Silicon",
        category = Advanced,
        techTier = Steel,
        order = 3,
        icon = "silicon-96.png"
      )
  case bauxiteOre
      extends Resource(
        displayName = "Bauxite Ore",
        category = Raw,
        techTier = Steel,
        order = 6,
        icon = "bauxiteOre-96.png"
      )
  case aluminium
      extends Resource(
        displayName = "Aluminium",
        category = Basic,
        techTier = Steel,
        order = 13,
        icon = "aluminium-96.png"
      )
  case gen1Chip
      extends Resource(
        displayName = "Gen1 Chip",
        category = Advanced,
        techTier = Gen1,
        order = 1,
        icon = "gen1Chip-96.png"
      )
  case greenScience
      extends Resource(
        displayName = "Green Science",
        category = Science,
        techTier = Gen1,
        order = 5,
        icon = "greenScience-96.png"
      )
  case oilBarrel
      extends Resource(
        displayName = "Oil Barrel",
        category = Raw,
        techTier = Gen1,
        order = 5,
        icon = "oilBarrel-96.png"
      )
  case petroleumBarrel
      extends Resource(
        displayName = "Petroleum Barrel",
        category = Basic,
        techTier = Gen1,
        order = 7,
        icon = "petroleumBarrel-96.png"
      )
  case plastics
      extends Resource(
        displayName = "Plastics",
        category = Basic,
        techTier = Gen1,
        order = 8,
        icon = "plastics-96.png"
      )
  case sulfur
      extends Resource(
        displayName = "Sulfur",
        category = Basic,
        techTier = Gen1,
        order = 9,
        icon = "sulfur-96.png"
      )
  case oxygen
      extends Resource(
        displayName = "Oxygen",
        category = Raw,
        techTier = Gen1,
        order = 7,
        icon = "oxygen-96.png"
      )
  case siliconDioxide
      extends Resource(
        displayName = "Silicon Dioxide",
        category = Advanced,
        techTier = Gen1,
        order = 11,
        icon = "siliconDioxide-96.png"
      )
  case gen2Chip
      extends Resource(
        displayName = "Gen2 Chip",
        category = Advanced,
        techTier = Gen2,
        order = 2,
        icon = "gen2Chip-96.png"
      )
  case drone
      extends Resource(
        displayName = "Drone",
        category = Basic,
        techTier = Gen2,
        order = 19,
        icon = "drone-96.png"
      )
  case blueScience
      extends Resource(
        displayName = "Blue Science",
        category = Science,
        techTier = Gen2,
        order = 6,
        icon = "blueScience-96.png"
      )
  case phenolicResin
      extends Resource(
        displayName = "Phenolic Resin",
        category = Basic,
        techTier = Gen2,
        order = 17,
        icon = "phenolicResin-96.png"
      )
  case lowkDielectric
      extends Resource(
        displayName = "Low-k Dielectric",
        category = Advanced,
        techTier = Gen2,
        order = 3,
        icon = "lowkDielectric-96.png"
      )
  case germanium
      extends Resource(
        displayName = "Germanium",
        category = Advanced,
        techTier = Gen2,
        order = 4,
        icon = "germanium-96.png"
      )
  case siliconGermanium
      extends Resource(
        displayName = "Silicon Germanium",
        category = Advanced,
        techTier = Gen2,
        order = 4,
        icon = "siliconGermanium-96.png"
      )
  case hgCopperPlate
      extends Resource(
        displayName = "High-Grade Copper Plate",
        category = Advanced,
        techTier = Gen2,
        order = 5,
        icon = "hgCopperPlate-96.png"
      )
  case gen3Chip
      extends Resource(
        displayName = "Gen3 Chip",
        category = Advanced,
        techTier = Gen3,
        order = 6,
        icon = "gen3Chip-96.png"
      )
  case purpleScience
      extends Resource(
        displayName = "Purple Science",
        category = Science,
        techTier = Gen3,
        order = 7,
        icon = "purpleScience-96.png"
      )
  case hafnium
      extends Resource(
        displayName = "Hafnium",
        category = Raw,
        techTier = Gen3,
        order = 7,
        icon = "hafnium-96.png"
      )
  case highkDielectric
      extends Resource(
        displayName = "High-k Dielectric",
        category = Advanced,
        techTier = Gen3,
        order = 8,
        icon = "highkDielectric-96.png"
      )
  case metalGate
      extends Resource(
        displayName = "Metal Gate",
        category = Advanced,
        techTier = Gen3,
        order = 9,
        icon = "metalGate-96.png"
      )
  case gen4Chip
      extends Resource(
        displayName = "Gen4 Chip",
        category = Advanced,
        techTier = Gen4,
        order = 10,
        icon = "gen4Chip-96.png"
      )
  case yellowScience
      extends Resource(
        displayName = "Yellow Science",
        category = Science,
        techTier = Gen4,
        order = 8,
        icon = "yellowScience-96.png"
      )
  case graphene
      extends Resource(
        displayName = "Graphene",
        category = Advanced,
        techTier = Gen4,
        order = 11,
        icon = "graphene-96.png"
      )
  case hafniumDisulfide
      extends Resource(
        displayName = "Hafnium Disulfide",
        category = Advanced,
        techTier = Gen4,
        order = 12,
        icon = "hafniumDisulfide-96.png"
      )
  case nanowires
      extends Resource(
        displayName = "Nanowires",
        category = Advanced,
        techTier = Gen4,
        order = 13,
        icon = "nanowires-96.png"
      )
  case apMaterial
      extends Resource(
        displayName = "AP Material",
        category = Advanced,
        techTier = Gen4,
        order = 14,
        icon = "apMaterial-96.png"
      )
  case gen5Chip
      extends Resource(
        displayName = "Gen5 Chip",
        category = Advanced,
        techTier = Gen5,
        order = 15,
        icon = "gen5Chip-96.png"
      )
  case rocketControlUnit
      extends Resource(
        displayName = "Rocket Control Unit",
        category = Advanced,
        techTier = Gen5,
        order = 9,
        icon = "rocketControlUnit-96.png"
      )
  case fuelCell
      extends Resource(
        displayName = "Fuel Cell",
        category = Advanced,
        techTier = Gen5,
        order = 7,
        icon = "fuelCell-96.png"
      )
  case lightDensityStructures
      extends Resource(
        displayName = "Light Density Structures",
        category = Advanced,
        techTier = Gen5,
        order = 8,
        icon = "lightDensityStructures-96.png"
      )
  case rocketParts
      extends Resource(
        displayName = "Rocket Parts",
        category = Science,
        techTier = Gen5,
        order = 1,
        icon = "rocketParts-96.png"
      )
  case satelite
      extends Resource(
        displayName = "Satellite",
        category = Rocket,
        techTier = Gen5,
        order = 2,
        icon = "satelite-96.png"
      )
  case whiteScience
      extends Resource(
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

case class CountedResource(
    resource: Resource,
    qty: Double
)

extension (r: Resource)
  def *(qty: Double): CountedResource = CountedResource(r, qty)
