package gameanalyzer.model

enum BuildingCategory {
  case Basics, Energy, Intermediates, `Progress & Expansion`, Beacons
}

import Resource.*
import BuildingCategory.*

enum Building(
    val displayName: String,
    val category: BuildingCategory,
    val cost: Set[CountedResource],
    val inputs: Set[CountedResource],
    val outputs: Set[CountedResource],
    val netEnergy: Double,
    val minable: Boolean,
    val description: String
) extends Enum[Building] {

  def inputsMap: Map[Resource, Double] =
    inputs.map(cr => cr.resource -> cr.qty).toMap

  def outputsMap: Map[Resource, Double] =
    outputs.map(cr => cr.resource -> cr.qty).toMap

  case testBuilding
      extends Building(
        displayName = "Test Building",
        category = Basics,
        cost = Set(stone * 0),
        inputs = Set.empty,
        outputs = Set(
          stone * 10,
          coal * 10,
          ironOre * 10,
          copperOre * 10,
          oilBarrel * 10,
          bricks * 10,
          ironPlates * 10,
          copperPlates * 10,
          gears * 10,
          // copperCables * 10,
          // steel * 10,
          // petroleumBarrel * 1,
          // plastics * 1,
          // sulfur * 1,
          gen1Chip * 100000,
          gen2Chip * 100000,
          gen3Chip * 100000,
          gen4Chip * 100000,
          gen5Chip * 100000,
          // researchPoints * 1,
          // expansionPoints * 1,
          // alienArtefacts * 1,
          // standardAmmunition * 1,
          // armorPenetratingAmmunition * 1,
          // piercingAmmunition * 1,
          // rail * 1,
          // battery * 1,
          // fuelCell * 1,
          // lightDensityStructures * 1,
          // rocketControlUnit * 1,
          // turret * 1,
          // wall * 1,
          redScience * 10,
          // darkScience * 1,
          greenScience * 10,
          blueScience * 10,
          purpleScience * 10,
          yellowScience * 10,
          whiteScience * 10,
          drone * 10
          // rocketParts * 1,
          // satelite * 1
          // gen1Chip * 10,
          // gen2Chip * 10,
          // gen3Chip * 10,
          // gen4Chip * 10,
          // gen5Chip * 10,
        ),
        netEnergy = 0,
        minable = false,
        description =
          "Receives a 50% output boost when no other building is present in its parcel."
      )
  case kiln
      extends Building(
        displayName = "Kiln",
        category = Basics,
        cost = Set(stone * 10),
        inputs = Set(stone * 2, coal * 0.2),
        outputs = Set(bricks * 1),
        netEnergy = 0,
        minable = false,
        description = ""
      )
  case ironSmelter
      extends Building(
        displayName = "Iron Smelter",
        category = Basics,
        cost = Set(bricks * 10),
        inputs = Set(ironOre * 2, coal * 0.2),
        outputs = Set(ironPlates * 1),
        netEnergy = 0,
        minable = false,
        description = ""
      )
  case coalPowerPlant
      extends Building(
        displayName = "Coal Power Plant",
        category = Energy,
        cost = Set(ironPlates * 15, bricks * 15),
        inputs = Set(coal * 0.5),
        outputs = Set(coalAsh * 0.1),
        netEnergy = 16,
        minable = false,
        description = "Produces 16 energy."
      )
  case coalMiner
      extends Building(
        displayName = "Coal Miner",
        category = Basics,
        cost = Set(ironPlates * 10, bricks * 10),
        inputs = Set.empty,
        outputs = Set(coal * 1),
        netEnergy = -1,
        minable = true,
        description = ""
      )
  case ironMiner
      extends Building(
        displayName = "Iron Miner",
        category = Basics,
        cost = Set(ironPlates * 10, bricks * 10),
        inputs = Set.empty,
        outputs = Set(ironOre * 1),
        netEnergy = -1,
        minable = true,
        description = ""
      )
  case stoneMiner
      extends Building(
        displayName = "Stone Miner",
        category = Basics,
        cost = Set(ironPlates * 10, bricks * 10),
        inputs = Set.empty,
        outputs = Set(stone * 1),
        netEnergy = -1,
        minable = true,
        description = ""
      )
  case copperMiner
      extends Building(
        displayName = "Copper Miner",
        category = Basics,
        cost = Set(ironPlates * 10, bricks * 10),
        inputs = Set.empty,
        outputs = Set(copperOre * 1),
        netEnergy = -1,
        minable = true,
        description = ""
      )
  case copperSmelter
      extends Building(
        displayName = "Copper Smelter",
        category = Basics,
        cost = Set(bricks * 15),
        inputs = Set(copperOre * 2, coal * 0.2),
        outputs = Set(copperPlates * 1),
        netEnergy = 0,
        minable = false,
        description = ""
      )
  case gearPress
      extends Building(
        displayName = "Gear Press",
        category = Intermediates,
        cost = Set(ironPlates * 30, bricks * 25),
        inputs = Set(ironPlates * 1),
        outputs = Set(gears * 0.5),
        netEnergy = -2,
        minable = false,
        description = ""
      )
  case cableExtruder
      extends Building(
        displayName = "Cable Extruder",
        category = Intermediates,
        cost = Set(gears * 20, bricks * 30),
        inputs = Set(copperPlates * 1),
        outputs = Set(copperCables * 2),
        netEnergy = -2,
        minable = false,
        description = ""
      )
  case researchCenter
      extends Building(
        displayName = "Research Center",
        category = `Progress & Expansion`,
        cost = Set(gears * 35, bricks * 50, copperCables * 50),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -4,
        minable = false,
        description = "Makes science from this parcel available for research."
      )
  // {
  // 	id= "beltBus",
  // 	name= "Belt Bus",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(expansionPoints * 1 },
  // 	inputs = Set.empty,
  // 	outputs = Set.empty,
  // 	netEnergy = -2,
  // 	rate = 1,
  // 	minable = false,
  // 	description= "Move resources between parcels"
  // },
  // {
  // 	id= "expansionCenter",
  // 	name= "Expansion Center",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(ironPlates: 100 },
  // 	// inputs = Set(redScience: 4, bricks * 10, gears * 10 },
  // 	// outputs = Set(expansionPoints * 1 },
  // 	inputs = Set.empty,
  // 	outputs = Set.empty,
  // 	netEnergy = -3,
  // 	rate = 1,
  // 	minable = false,
  // 	description= "Unlocks the ability to add more parcels."
  // },
  case steelMill
      extends Building(
        displayName = "Steel Mill",
        category = Basics,
        cost = Set(ironPlates * 100, bricks * 400),
        inputs = Set(ironPlates * 5),
        outputs = Set(steel * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  case oilWell
      extends Building(
        displayName = "Oil Well",
        category = Basics,
        cost = Set(steel * 250, gears * 250, gen1Chip * 75),
        inputs = Set.empty,
        outputs = Set(oilBarrel * 1),
        netEnergy = -2,
        minable = false,
        description = ""
      )
  case oilRefinery
      extends Building(
        displayName = "Oil Refinery",
        category = Basics,
        cost = Set(steel * 250, gears * 250, gen1Chip * 75),
        inputs = Set(oilBarrel * 10),
        outputs = Set(petroleumBarrel * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  case plasticsPlant
      extends Building(
        displayName = "Plastics Plant",
        category = Intermediates,
        cost = Set(steel * 500, gears * 500, gen1Chip * 200),
        inputs = Set(petroleumBarrel * 1, coal * 0.5),
        outputs = Set(plastics * 3),
        netEnergy = -2,
        minable = false,
        description = ""
      )
  case sulfurPlant
      extends Building(
        displayName = "Sulfur Plant",
        category = Intermediates,
        cost = Set(steel * 500, gears * 500, gen1Chip * 200),
        inputs = Set(petroleumBarrel * 1),
        outputs = Set(sulfur * 2),
        netEnergy = -2,
        minable = false,
        description = ""
      )
  case remoteConstructionFacility
      extends Building(
        displayName = "Construction Hub",
        category = `Progress & Expansion`,
        cost = Set(gears * 250, gen1Chip * 100),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -6,
        minable = false,
        description =
          "Allows global use of resources from this parcel. -30% Production Rate. +30% Consumption Rate."
      )
  case speedBeaconT1
      extends Building(
        displayName = "Speed Beacon T1",
        category = Beacons,
        cost = Set(steel * 100, gen2Chip * 100),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -3,
        minable = false,
        description = "+2% Production Rate. +2% Consumption Rate."
      )
  case productivityBeaconT1
      extends Building(
        displayName = "Productivity Beacon T1",
        category = Beacons,
        cost = Set(steel * 100, gen2Chip * 100),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -3,
        minable = false,
        description = "+1% Production Rate."
      )
  case speedBeaconT2
      extends Building(
        displayName = "Speed Beacon T2",
        category = Beacons,
        cost = Set(steel * 350, gen3Chip * 350),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -6,
        minable = false,
        description = "+4% Production Rate. +4% Consumption Rate."
      )
  case productivityBeaconT2
      extends Building(
        displayName = "Productivity Beacon T2",
        category = Beacons,
        cost = Set(steel * 350, gen3Chip * 350),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -6,
        minable = false,
        description = "+2% Production Rate."
      )
  case speedBeaconT3
      extends Building(
        displayName = "Speed Beacon T3",
        category = Beacons,
        cost = Set(steel * 1000, gen4Chip * 1000),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -9,
        minable = false,
        description = "+8% Production Rate. +8% Consumption Rate."
      )
  case productivityBeaconT3
      extends Building(
        displayName = "Productivity Beacon T3",
        category = Beacons,
        cost = Set(steel * 1000, gen4Chip * 1000),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -9,
        minable = false,
        description = "+6% Production Rate."
      )
  // {
  // 	id= "blueprintLibrary",
  // 	name= "Blueprint Library",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(steel * 500, gears * 1500, gen2Chip * 1000 },
  // 	inputs = Set.empty,
  // 	outputs = Set.empty,
  // 	netEnergy = -6,
  // 	rate = 1,
  // 	minable = false,
  // 	description= "Unlocks "Copy & Paste" in the parcel context menu"
  // },
  // {
  // 	id= "militaryHQ",
  // 	name= "Military HQ",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(ironPlates: 1000, copperCables * 500 },
  // 	inputs = Set.empty,
  // 	outputs = Set.empty,
  // 	netEnergy = -6,
  // 	rate = 1,
  // 	minable = false,
  // 	description= "Makes ammunition from this parcel available for Military Operations"
  // },
  // {
  // 	id= "standardAmmunitionFactory",
  // 	name= "Ammunition Factory (Standard)",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(ironPlates: 1000, gears * 500 },
  // 	inputs = Set(ironPlates: 1, copperPlates: 3 },
  // 	outputs = Set(standardAmmunition: 5 },
  // 	netEnergy = -6,
  // 	rate = 1,
  // 	minable = false,
  // },
  // {
  // 	id= "armorPenetratingAmmunitionFactory",
  // 	name= "Ammunition Factory (Armor Pen)",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(steel * 1000, gears * 500, gen1Chip * 500 },
  // 	inputs = Set(standardAmmunition: 25, steel * 1, copperPlates: 2 },
  // 	outputs = Set(armorPenetratingAmmunition: 5 },
  // 	netEnergy = -12,
  // 	rate = 1,
  // 	minable = false,
  // },
  // {
  // 	id= "piercingAmmunitionFactory",
  // 	name= "Ammunition Factory (Piercing)",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(steel * 10000, gears * 10000, gen3Chip * 5000 },
  // 	inputs = Set(armorPenetratingAmmunition: 20, steel * 1, copperPlates: 1 },
  // 	outputs = Set(piercingAmmunition: 5 },
  // 	netEnergy = -18,
  // 	rate = 1,
  // 	minable = false,
  // },
  // {
  // 	id= "trainStation",
  // 	name= "Train Station",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(steel * 100, copperCables * 100, gen1Chip * 50 },
  // 	inputs = Set.empty,
  // 	outputs = Set.empty,
  // 	netEnergy = -8,
  // 	rate = 1,
  // 	minable = false,
  // },
  // {
  // 	id= "railFactory",
  // 	name= "Rail Factory",
  // 	category = Intermediates,
  // 	cost = Set(steel * 150, bricks * 150 },
  // 	inputs = Set(stone: 1, steel * 1 },
  // 	outputs = Set(rail: 5 },
  // 	netEnergy = -4,
  // 	rate = 1,
  // 	minable = false,
  // },
  // {
  // 	id= "turretFactory",
  // 	name= "Turret Factory",
  // 	category = Intermediates,
  // 	cost = Set(steel * 150, bricks * 150 },
  // 	inputs = Set(gears * 75, steel * 25, bricks * 25 },
  // 	outputs = Set(turret: 1 },
  // 	netEnergy = -6,
  // 	rate = 1,
  // 	minable = false,
  // },
  // {
  // 	id= "wallFactory",
  // 	name= "Wall Factory",
  // 	category = Intermediates,
  // 	cost = Set(steel * 150, bricks * 150 },
  // 	inputs = Set(ironPlates: 5, bricks * 50 },
  // 	outputs = Set(wall: 1 },
  // 	netEnergy = -6,
  // 	rate = 1,
  // 	minable = false,
  // },
  case redScienceLab
      extends Building(
        displayName = "Red Science Laboratory",
        category = `Progress & Expansion`,
        cost = Set(gears * 50, bricks * 50),
        inputs = Set(copperCables * 5, gears * 2),
        outputs = Set(redScience * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  case greenScienceLab
      extends Building(
        displayName = "Green Science Laboratory",
        category = `Progress & Expansion`,
        cost = Set(gen1Chip * 80, steel * 180, bricks * 160),
        inputs = Set(copperCables * 12, gen1Chip * 4),
        outputs = Set(greenScience * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  // {
  // 	id= "darkScienceLab",
  // 	name= "Dark Science Laboratory",
  // 	category = `Progress & Expansion`,
  // 	cost = Set(gen2Chip * 250, copperCables * 250, gears * 250, bricks * 250 },
  // 	inputs = Set(standardAmmunition: 80, turret: 1, wall: 3 },
  // 	outputs = Set(darkScience: 1 },
  // 	netEnergy = -3,
  // 	rate = 1,
  // 	minable = false,
  // },
  case blueScienceLab
      extends Building(
        displayName = "Blue Science Laboratory",
        category = `Progress & Expansion`,
        cost = Set(gen2Chip * 250, steel * 250, bricks * 250),
        inputs = Set(sulfur * 8, gen2Chip * 4),
        outputs = Set(blueScience * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  case purpleScienceLab
      extends Building(
        displayName = "Purple Science Laboratory",
        category = `Progress & Expansion`,
        cost = Set(gen3Chip * 250, steel * 250, bricks * 250),
        inputs = Set(steel * 3, gen3Chip * 2, gen1Chip * 4),
        outputs = Set(purpleScience * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  case yellowScienceLab
      extends Building(
        displayName = "Yellow Science Laboratory",
        category = `Progress & Expansion`,
        cost = Set(gen4Chip * 250, steel * 250, bricks * 250),
        inputs = Set(gen4Chip * 2, gen2Chip * 4),
        outputs = Set(yellowScience * 1),
        netEnergy = -3,
        minable = false,
        description = ""
      )
  case batteryFactory
      extends Building(
        displayName = "Battery Factory",
        category = Intermediates,
        cost = Set(steel * 500, bricks * 500, gen1Chip * 500),
        inputs = Set(ironPlates * 1, copperPlates * 1, sulfur * 1),
        outputs = Set(battery * 1),
        netEnergy = -6,
        minable = false,
        description = ""
      )
  case solarBatteryArray
      extends Building(
        displayName = "Solar & Battery Array",
        category = Energy,
        cost =
          Set(steel * 100, copperCables * 100, gen1Chip * 50, battery * 50),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = 16,
        minable = false,
        description = "Produces 16 energy."
      )
  case fuelProcessingFacility
      extends Building(
        displayName = "Fuel Processing Facility",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen5Chip * 500),
        inputs = Set(petroleumBarrel * 10, coal * 10),
        outputs = Set(fuelCell * 1),
        netEnergy = -6,
        minable = false,
        description = ""
      )
  case LDSFactory
      extends Building(
        displayName = "LDS Factory",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen5Chip * 500),
        inputs = Set(copperPlates * 20, plastics * 5, steel * 2),
        outputs = Set(lightDensityStructures * 1),
        netEnergy = -6,
        minable = false,
        description = ""
      )
  case rocketControlUnitFactory
      extends Building(
        displayName = "Rocket Control Unit Factory",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen5Chip * 500),
        inputs = Set(
          gen5Chip * 1,
          gen4Chip * 5,
          gen3Chip * 10,
          gen2Chip * 25,
          gen1Chip * 50
        ),
        outputs = Set(rocketControlUnit * 1),
        netEnergy = -6,
        minable = false,
        description = ""
      )
  case rocketLaunchPad
      extends Building(
        displayName = "Rocket Launch Pad",
        category = `Progress & Expansion`,
        cost = Set(steel * 5000, bricks * 5000, gen5Chip * 500),
        inputs = Set(satelite * 1, rocketParts * 100),
        outputs = Set(whiteScience * 1000),
        netEnergy = -16,
        minable = false,
        description = ""
      )
  case rocketPartAssembly
      extends Building(
        displayName = "Rocket Part Assembly",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen5Chip * 500),
        inputs =
          Set(fuelCell * 4, lightDensityStructures * 4, rocketControlUnit * 4),
        outputs = Set(rocketParts * 1),
        netEnergy = -20,
        minable = false,
        description = ""
      )
  case sateliteAssembly
      extends Building(
        displayName = "Satellite Assembly",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen5Chip * 500),
        inputs = Set(
          copperPlates * 300,
          ironPlates * 180,
          plastics * 300,
          gen5Chip * 100,
          fuelCell * 50,
          steel * 300
        ),
        outputs = Set(satelite * 1),
        netEnergy = -20,
        minable = false,
        description = ""
      )
  case productionChallengePlant
      extends Building(
        displayName = "Production Challenge Plant",
        category = `Progress & Expansion`,
        cost = Set(steel * 1000, bricks * 250),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -12,
        minable = false,
        description = ""
      )
  case quartzRefinery
      extends Building(
        displayName = "Quartz Refinery",
        category = Intermediates,
        cost = Set(steel * 150, bricks * 250),
        inputs = Set(stone * 3),
        outputs = Set(quartz * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case siliconFurnace
      extends Building(
        displayName = "Silicon Furnace",
        category = Intermediates,
        cost = Set(steel * 150, bricks * 250),
        inputs = Set(quartz * 1, coal * 0.25),
        outputs = Set(silicon * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case bauxiteMiner
      extends Building(
        displayName = "Bauxite Miner",
        category = Intermediates,
        cost = Set(steel * 150, bricks * 250),
        inputs = Set.empty,
        outputs = Set(bauxiteOre * 10),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case aluminiumSmelter
      extends Building(
        displayName = "Aluminium Smelter",
        category = Intermediates,
        cost = Set(steel * 250, bricks * 150),
        inputs = Set(bauxiteOre * 5, coal * 1),
        outputs = Set(aluminium * 0.5),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case gen1ChipPlant
      extends Building(
        displayName = "Gen 1 Chip Plant",
        category = Intermediates,
        cost = Set(steel * 350, bricks * 350),
        inputs = Set(silicon * 2, copperCables * 3, aluminium * 0.5),
        outputs = Set(gen1Chip * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case bioreactor
      extends Building(
        displayName = "Bioreactor",
        category = Intermediates,
        cost = Set(
          steel * 150,
          bricks * 150,
          gen1Chip * 50
        ), // TODO: Add Biomatter as a cost --> Biomatter is acquired by killing aliens.
        inputs = Set.empty,
        outputs = Set(oxygen * 0.1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case oxidationFurnace
      extends Building(
        displayName = "Oxidation Furnace",
        category = Intermediates,
        cost = Set(steel * 1000, bricks * 1000, gen1Chip * 500),
        inputs = Set(silicon * 1, oxygen * 2),
        outputs = Set(siliconDioxide * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case gen2ChipPlant
      extends Building(
        displayName = "Gen 2 Chip Plant",
        category = Intermediates,
        cost = Set(steel * 500, bricks * 500, gen1Chip * 500),
        inputs = Set(siliconDioxide * 1, plastics * 1),
        outputs = Set(gen2Chip * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case phenolicResinPlant
      extends Building(
        displayName = "Phenolic Resin Plant",
        category = Intermediates,
        cost = Set(steel * 1000, bricks * 1000, gen1Chip * 500),
        inputs = Set(petroleumBarrel * 1, coal * 4, coalAsh * 2),
        outputs = Set(phenolicResin * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case lowDepositionChamber
      extends Building(
        displayName = "Low Deposition Chamber",
        category = Intermediates,
        cost = Set(steel * 1000, bricks * 1000, gen2Chip * 1000),
        inputs = Set(silicon * 1, oxygen * 1),
        outputs = Set(lowkDielectric * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case germaniumRefinery
      extends Building(
        displayName = "Germanium Refinery",
        category = Intermediates,
        cost = Set(steel * 500, bricks * 500, gen2Chip * 250),
        inputs = Set(coalAsh * 0.6),
        outputs = Set(germanium * 0.1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case alloyFurnace
      extends Building(
        displayName = "SiGe Furnace",
        category = Intermediates,
        cost = Set(steel * 500, bricks * 500, gen2Chip * 250),
        inputs = Set(silicon * 1, germanium * 1),
        outputs = Set(siliconGermanium * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case highGradeSmelter
      extends Building(
        displayName = "High-Grade Smelter",
        category = Intermediates,
        cost = Set(steel * 1500, bricks * 1500, gen2Chip * 500),
        // inputs = Set(hgCopperOre: 1 ),
        inputs = Set(copperOre * 20, coal * 4),
        outputs = Set(hgCopperPlate * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case gen3ChipPlant
      extends Building(
        displayName = "Gen 3 Chip Plant",
        category = Intermediates,
        cost = Set(steel * 1500, bricks * 1500, gen2Chip * 1000),
        inputs = Set(
          lowkDielectric * 1,
          siliconGermanium * 0.5,
          hgCopperPlate * 0.5,
          phenolicResin * 1
        ),
        outputs = Set(gen3Chip * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case hafniumMine
      extends Building(
        displayName = "Hafnium Mine",
        category = Intermediates,
        cost = Set(steel * 500, bricks * 500, gen3Chip * 150),
        inputs = Set.empty,
        outputs = Set(hafnium * 0.1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case advancedDepositionChamber
      extends Building(
        displayName = "Advanced Deposition Chamber",
        category = Intermediates,
        cost = Set(steel * 2500, bricks * 2500, gen3Chip * 1000),
        inputs = Set(hafnium * 1, siliconDioxide * 1, silicon * 2),
        outputs = Set(highkDielectric * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case metalGatePlant
      extends Building(
        displayName = "Metal Gate Plant",
        category = Intermediates,
        cost = Set(steel * 2500, bricks * 2500, gen3Chip * 1000),
        inputs = Set(hafnium * 2, silicon * 4),
        outputs = Set(metalGate * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case gen4ChipPlant
      extends Building(
        displayName = "Gen 4 Chip Plant",
        category = Intermediates,
        cost = Set(steel * 2500, bricks * 2500, gen3Chip * 1000),
        inputs = Set(
          siliconGermanium * 1,
          highkDielectric * 1,
          metalGate * 1,
          phenolicResin * 1
        ),
        outputs = Set(gen4Chip * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case grapheneLab
      extends Building(
        displayName = "Graphene Lab",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen4Chip * 800),
        inputs = Set(silicon * 1, coal * 1, coalAsh * 0.1),
        outputs = Set(graphene * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case hafniumDisulfidePlant
      extends Building(
        displayName = "Hafnium-Disulfide Plant",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen4Chip * 800),
        inputs = Set(hafnium * 1, sulfur * 2),
        outputs = Set(hafniumDisulfide * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case nanowireLab
      extends Building(
        displayName = "Nanowire Lab",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen4Chip * 800),
        inputs = Set(hafnium * 1, siliconGermanium * 1, oxygen * 1),
        outputs = Set(nanowires * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case apMateriallLab
      extends Building(
        displayName = "AP-Material Lab",
        category = Intermediates,
        cost = Set(steel * 5000, bricks * 5000, gen4Chip * 800),
        inputs = Set(hgCopperPlate * 1, phenolicResin * 1, quartz * 1),
        outputs = Set(apMaterial * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case gen5ChipPlant
      extends Building(
        displayName = "Gen 5 Chip Plant",
        category = Intermediates,
        cost = Set(steel * 150, bricks * 150),
        inputs = Set(
          graphene * 3,
          hafniumDisulfide * 2,
          nanowires * 2,
          apMaterial * 2
        ),
        outputs = Set(gen5Chip * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case droneFactory
      extends Building(
        displayName = "Drone Factory",
        category = Intermediates,
        cost = Set(steel * 150, bricks * 150),
        inputs = Set(gen2Chip * 1, gen1Chip * 1, gears * 2, copperCables * 8),
        outputs = Set(drone * 1),
        netEnergy = -8,
        minable = false,
        description = ""
      )
  case dronePort
      extends Building(
        displayName = "Drone Port",
        category = `Progress & Expansion`,
        cost = Set(steel * 1500, bricks * 1500, drone * 100),
        inputs = Set.empty,
        outputs = Set.empty,
        netEnergy = -8,
        minable = false,
        description = "Increases Task Execution Speed by +2 Tasks / Minute."
      )
}
