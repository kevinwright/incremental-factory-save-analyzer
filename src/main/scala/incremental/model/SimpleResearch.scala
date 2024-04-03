package incremental.model

import Item.*

object SimpleResearch {
  def ordered: Seq[SimpleResearch] =
    SimpleResearch.values.toSeq.sortBy(_.ordinal)
}

enum SimpleResearch(
    val displayName: String,
    val cost: Set[CountedItem],
    val unlocks: Unlock
) {
  def wikiTitle: String = s"Research $displayName"

  case expansionTech
      extends SimpleResearch(
        displayName = "Expansion Tech",
        cost = Set(redScience * 40),
        unlocks = UnlockAbility("The Map & the ability to build more Parcels")
      )

  case basicElectronics
      extends SimpleResearch(
        displayName = "Basic Electronics",
        cost = Set(redScience * 180),
        unlocks = UnlockBuildings(
          Building.gen1ChipPlant,
          Building.quartzRefinery,
          Building.siliconFurnace,
          Building.bauxiteMiner,
          Building.aluminiumSmelter
        )
      )

  case steelMaking
      extends SimpleResearch(
        displayName = "Steel Making",
        cost = Set(redScience * 60),
        unlocks = UnlockBuildings(Building.steelMill)
      )

  case advancedParcel
      extends SimpleResearch(
        displayName = "Advanced Parcel",
        cost = Set(redScience * 240),
        unlocks = UnlockParcelType(ParcelType.advanced)
      )

  case remoteConstruction
      extends SimpleResearch(
        displayName = "Remote Construction",
        cost = Set(redScience * 200, greenScience * 60),
        unlocks = UnlockBuildings(Building.remoteConstructionFacility)
      )

  case improvedElectronics
      extends SimpleResearch(
        displayName = "Improved Electronics",
        cost = Set(redScience * 1500, greenScience * 360),
        unlocks = UnlockBuildings(
          Building.gen2ChipPlant,
          Building.oxidationFurnace,
          Building.bioreactor
        )
      )

  case oilProcessing
      extends SimpleResearch(
        displayName = "Oil Processing",
        cost = Set(redScience * 240, greenScience * 60),
        unlocks = UnlockBuildings(
          Building.oilRefinery,
          Building.oilWell,
          Building.plasticsPlant,
          Building.sulfurPlant
        )
      )

  case challengeTech
      extends SimpleResearch(
        displayName = "Production Challenges",
        cost = Set(redScience * 600, greenScience * 120),
        unlocks = UnlockBuildings(Building.productionChallengePlant)
      )

  case largeParcel
      extends SimpleResearch(
        displayName = "Large Parcel",
        cost = Set(redScience * 550, greenScience * 100),
        unlocks = UnlockParcelType(ParcelType.large)
      )

  case nodeParcel
      extends SimpleResearch(
        displayName = "Node Parcel",
        cost = Set(redScience * 600, greenScience * 110),
        unlocks = UnlockParcelType(ParcelType.node)
      )

  case blueprintTech
      extends SimpleResearch(
        displayName = "Blueprint Tech",
        cost = Set(redScience * 400, greenScience * 120, blueScience * 40),
        unlocks = UnlockAbility("Blueprint Library")
      )

  case advancedElectronics
      extends SimpleResearch(
        displayName = "Advanced Electronics",
        cost = Set(redScience * 2500, greenScience * 800, blueScience * 360),
        unlocks = UnlockBuildings(
          Building.gen3ChipPlant,
          Building.phenolicResinPlant,
          Building.highGradeSmelter,
          Building.alloyFurnace,
          Building.germaniumRefinery,
          Building.lowDepositionChamber
        )
      )

  case HiTechParcel
      extends SimpleResearch(
        displayName = "HiTech Parcel",
        cost = Set(redScience * 700, greenScience * 160, blueScience * 80),
        unlocks = UnlockParcelType(ParcelType.hitech)
      )

  case droneTech
      extends SimpleResearch(
        displayName = "Construction Drones",
        cost = Set(
          redScience * 400,
          greenScience * 200,
          blueScience * 100,
          purpleScience * 50
        ),
        unlocks = UnlockBuildings(
          Building.droneFactory,
          Building.dronePort
        )
      )

  case beaconTech
      extends SimpleResearch(
        displayName = "Beacon Tech",
        cost = Set(
          redScience * 1000,
          greenScience * 170,
          blueScience * 90,
          purpleScience * 40
        ),
        unlocks = UnlockBuildings(
          Building.productivityBeaconT1,
          Building.speedBeaconT1
        )
      )

  case hiTechElectronics
      extends SimpleResearch(
        displayName = "Hi-Tech Electronics",
        cost = Set(
          redScience * 3500,
          greenScience * 1500,
          blueScience * 800,
          purpleScience * 360
        ),
        unlocks = UnlockBuildings(
          Building.gen4ChipPlant,
          Building.hafniumMine,
          Building.hafniumDisulfidePlant,
          Building.advancedDepositionChamber,
          Building.metalGatePlant
        )
      )

  case beaconTech2
      extends SimpleResearch(
        displayName = "Beacon Tech 2",
        cost = Set(
          redScience * 3000,
          greenScience * 1500,
          blueScience * 500,
          purpleScience * 250,
          yellowScience * 50
        ),
        unlocks = UnlockBuildings(
          Building.productivityBeaconT2,
          Building.speedBeaconT2
        )
      )

  case nanoelectronics
      extends SimpleResearch(
        displayName = "Nanoelectronics",
        cost = Set(
          redScience * 5000,
          greenScience * 3500,
          blueScience * 1500,
          purpleScience * 800,
          yellowScience * 360
        ),
        unlocks = UnlockBuildings(
          Building.gen5ChipPlant,
          Building.grapheneLab,
          Building.nanowireLab,
          Building.apMateriallLab
        )
      )

  case rocketTech
      extends SimpleResearch(
        displayName = "Rocket Tech",
        cost = Set(
          redScience * 5000,
          greenScience * 3500,
          blueScience * 1500,
          purpleScience * 800,
          yellowScience * 360
        ),
        unlocks = UnlockBuildings(
          Building.rocketLaunchPad,
          Building.rocketPartAssembly,
          Building.rocketControlUnitFactory,
          Building.LDSFactory,
          Building.fuelProcessingFacility
        )
      )

  case beaconTech3
      extends SimpleResearch(
        displayName = "Beacon Tech 3",
        cost = Set(
          redScience * 9000,
          greenScience * 6000,
          blueScience * 3000,
          purpleScience * 2000,
          yellowScience * 1000,
          whiteScience * 1000
        ),
        unlocks = UnlockBuildings(
          Building.productivityBeaconT3,
          Building.speedBeaconT3
        )
      )
}
