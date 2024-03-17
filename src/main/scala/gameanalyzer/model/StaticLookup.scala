package gameanalyzer.model

import scala.collection.immutable.ListMap

object StaticLookup {
  val buildingFullNames = ListMap(
    "kiln" -> "Kiln",
    "ironSmelter" -> "Iron Smelter",
    "coalMiner" -> "Coal Miner",
    "coalPowerPlant" -> "Coal Power Plant",
    "ironMiner" -> "Iron Miner",
    "stoneMiner" -> "Stone Miner",
    "copperMiner" -> "Copper Miner",
    "copperSmelter" -> "Copper Smelter",
    "gearPress" -> "Gear Press",
    "cableExtruder" -> "Cable Extruder",
    "researchCenter" -> "Research Center",
    "redScienceLab" -> "Red Science Laboratory",
    "steelMill" -> "Steel Mill",
    "quartzRefinery" -> "Quartz Refinery",
    "siliconFurnace" -> "Silicon Furnace",
    "bauxiteMiner" -> "Bauxite Miner",
    "aluminiumSmelter" -> "Aluminium Smelter",
    "gen1ChipPlant" -> "Gen 1 Chip Plant",
    "greenScienceLab" -> "Green Science Laboratory",
    "remoteConstructionFacility" -> "Remote Construction Facility",
    "oilWell" -> "Oil Well",
    "oilRefinery" -> "Oil Refinery",
    "plasticsPlant" -> "Plastics Plant",
    "sulfurPlant" -> "Sulfur Plant",
    "productionChallengePlant" -> "Production Challenge Plant",
    "bioreactor" -> "Bioreactor",
    "oxidationFurnace" -> "Oxidation Furnace",
    "gen2ChipPlant" -> "Gen 2 Chip Plant",
    "blueScienceLab" -> "Blue Science Laboratory",
    "droneFactory" -> "Drone Factory",
    "dronePort" -> "Drone Port",
    "phenolicResinPlant" -> "Phenolic Resin Plant",
    "lowDepositionChamber" -> "Low Deposition Chamber",
    "germaniumRefinery" -> "Germanium Refinery",
    "alloyFurnace" -> "Silicon-Germanium Furnace",
    "highGradeSmelter" -> "High Grade Smelter",
    "gen3ChipPlant" -> "Gen 3 Chip Plant",
    "purpleScienceLab" -> "Purple Science Laboratory",
    // speed beacon T1
    // prod beacon T1
    "hafniumMine" -> "Hafnium Mine",
    "advancedDepositionChamber" -> "Advanced Deposition Chamber",
    "metalGatePlant" -> "Metal Gate Plant",
    "gen4ChipPlant" -> "Gen 4 Chip Plant",
    "yellowScienceLab" -> "Yellow Science Laboratory",
    // speed beacon T2
    // prod beacon T2
    "grapheneLab" -> "Graphene Lab",
    "hafniumDisulfidePlant" -> "Hafnium-Disulfide Plant",
    "nanowireLab" -> "Nanowire Lab",
    "apMateriallLab" -> "AP-Material Lab",
    "gen5ChipPlant" -> "Gen 5 Chip Plant",
    "rocketControlUnitFactory" -> "Rocket Control Unit Factory",
    "fuelProcessingFacility" -> "Fuel Processing Facility",
    "LDSFactory" -> "LDS Factory",
    "rocketPartAssembly" -> "Rocket Part Assembly",
    "sateliteAssembly" -> "Satellite Assembly",
    "rocketLaunchPad" -> "Rocket Launch Pad"
    // speed beacon T2
    // prod beacon T2
  )

  val resourceFullNames = ListMap(
    "stone" -> "Stone",
    "bricks" -> "Bricks",
    "ironOre" -> "Iron Ore",
    "ironPlates" -> "Iron Plates",
    "gears" -> "Gears",
    "coal" -> "Coal",
    "coalAsh" -> "Coal Ash",
    "copperOre" -> "Copper Ore",
    "copperPlates" -> "Copper Plates",
    "copperCables" -> "Copper Cables",
    "redScience" -> "Red Science",
    "steel" -> "Steel",
    "quartz" -> "Quartz",
    "silicon" -> "Silicon",
    "bauxiteOre" -> "Bauxite Ore",
    "aluminium" -> "Aluminium",
    "gen1Chip" -> "Gen 1 Chip",
    "greenScience" -> "Green Science",
    "oilBarrel" -> "Oil Barrel",
    "petroleumBarrel" -> "Petroleum Barrel",
    "plastics" -> "Plastics",
    "sulfur" -> "Sulfur",
    "oxygen" -> "Oxygen",
    "siliconDioxide" -> "Silicon Dioxide",
    "gen2Chip" -> "Gen 2 Chip",
    "drone" -> "Drone",
    "blueScience" -> "Blue Science",
    "phenolicResin" -> "Phenolic Resin",
    "lowkDielectric" -> "Low-K Dielectric",
    "germanium" -> "Germanium",
    "siliconGermanium" -> "Silicon Germanium",
    "hgCopperPlate" -> "High-Grae Copper Plate",
    "gen3Chip" -> "Gen 3 Chip",
    "purpleScience" -> "Purple Science",
    "hafnium" -> "Hafnium",
    "highkDielectric" -> "High-K Dielectric",
    "metalGate" -> "Metal Gate",
    "gen4Chip" -> "Gen 4 Chip",
    "yellowScience" -> "Yellow Science",
    "graphene" -> "Graphene",
    "hafniumDisulfide" -> "Hafnium-Disulfide",
    "nanowires" -> "Nanowires",
    "apMaterial" -> "AP-Material",
    "gen5Chip" -> "Gen 5 Chip",
    "rocketControlUnit" -> "Rocket Control Unit",
    "fuelCell" -> "FuelCell",
    "lightDensityStructures" -> "Light Density Structures",
    "rocketParts" -> "Rocket Parts",
    "satelite" -> "Satellite",
    "whiteScience" -> "White Science"
  )

  case class ResourceAmount(
      resourceId: String,
      qty: Double
  )

  case class Recipe(
      buildingId: String,
      inputs: Set[ResourceAmount],
      output: ResourceAmount
  )

  private def mkRecipe(
      buildingId: String,
      output: (String, Double),
      inputs: (String, Double)*
  ): Recipe = Recipe(
    buildingId,
    inputs.map((id, qty) => ResourceAmount(id, qty)).toSet,
    ResourceAmount(output._1, output._2)
  )
  val recipes: Set[Recipe] = Set(
    mkRecipe("kiln", "bricks" -> 1, "stone" -> 2, "coal" -> 0.2)
  )

}
