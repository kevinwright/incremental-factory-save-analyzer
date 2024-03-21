package gameanalyzer.model

import scala.collection.immutable.ListMap

import gameanalyzer.CollectionUtils.*

object Parcel {
  case class Beacons(
      t1p: Int,
      t1s: Int,
      t2p: Int,
      t2s: Int,
      t3p: Int,
      t3s: Int
  ) {
    lazy val prodMult: Double =
      math.pow(1.01, t1p) *
        math.pow(1.02, t1s) *
        math.pow(1.02, t2p) *
        math.pow(1.04, t2s) *
        math.pow(1.06, t3p) *
        math.pow(1.08, t3s)

    lazy val consumptionMult: Double =
      math.pow(1.02, t1s) *
        math.pow(1.04, t2s) *
        math.pow(1.08, t3s)
  }
}
case class Parcel(
    id: String,
    x: Double,
    y: Double,
    nodeConnectionCount: Int,
    orientation: Int,
    cluster: Int,
    continent: Int,
    planet: Int,
    maxBuildings: Int,
    maxConnections: Int,
    parcelType: String,
    maxResources: Double,
    HQ: Boolean,
    buildings: Map[Building, Int],
    activeBuildings: Map[Building, Int],
    resources: Map[Resource, Double],
    // upgrades: Upgrades,
    // productionRateModifier: Int,
    // consumptionRateModifier: Int,
    // buildingProductionRateModifiers,
    // buildingConsumptionRateModifiers,
    outputValues: Map[String, OutputValue],
    name: Option[String]
) {
  def displayName: String = name.getOrElse(id)

  lazy val beacons: Parcel.Beacons = Parcel.Beacons(
    t1p = buildings.getOrElse(Building.productivityBeaconT1, 0),
    t1s = buildings.getOrElse(Building.speedBeaconT1, 0),
    t2p = buildings.getOrElse(Building.productivityBeaconT2, 0),
    t2s = buildings.getOrElse(Building.speedBeaconT2, 0),
    t3p = buildings.getOrElse(Building.productivityBeaconT3, 0),
    t3s = buildings.getOrElse(Building.speedBeaconT2, 0)
  )

  def consumptionMap: ListMap[Resource, Double] = (
    for {
      (building, numBuildings) <- buildings.toSeq.filter(_._2 > 0)
      (resource, resourceQty) <- building.inputsMap.toSeq
    } yield resource -> resourceQty * numBuildings * beacons.consumptionMult
  ).sumValues

  /** @param skillTree
    *   used to determine bonuses
    * @return
    */
  def productionMapForSkills(skillTree: SkillTree): ListMap[Resource, Double] =
    sumValues(
      for {
        (building, numBuildings) <- buildings.toSeq.filter(_._2 > 0)
        (resource, singleBuildingOutput) <- building.outputsMap.toSeq
        singleBonus = skillTree.specializationBoostFor(resource)
        specBoostMultiplier = 1 + (singleBonus * numBuildings)
      } yield {
        val totalProd =
          singleBuildingOutput * numBuildings * specBoostMultiplier * beacons.prodMult
        resource -> totalProd
      }
    )

  def unboostedProductionMap: ListMap[Resource, Double] =
    sumValues(
      for {
        (building, numBuildings) <- buildings.toSeq
        (resource, singleBuildingOutput) <- building.outputsMap.toSeq
      } yield {
        val totalProd =
          singleBuildingOutput * numBuildings
        resource -> totalProd
      }
    )
}
