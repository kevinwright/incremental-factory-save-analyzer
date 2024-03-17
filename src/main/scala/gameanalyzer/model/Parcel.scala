package gameanalyzer.model

import scala.collection.immutable.ListMap

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
    //upgrades: Upgrades,
    //productionRateModifier: Int,
    //consumptionRateModifier: Int,
    // buildingProductionRateModifiers,
    // buildingConsumptionRateModifiers,
    outputValues: Map[String, OutputValue],
    name: Option[String]
) {
  def displayName: String = name.getOrElse(id)

  private def sumValues[K, V: Numeric](seq: Seq[(K, V)]): ListMap[K, V] = {
    seq
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sum)
      .to(ListMap)
  }

  def consumptionMap: ListMap[Resource, Double] = sumValues(
    for {
      (building, numBuildings) <- buildings.toSeq
      (resource, resourceQty) <- building.inputsMap.toSeq
    } yield resource -> resourceQty * numBuildings
  )

  /**
   *
   * @param skillTree used to determine bonuses
   * @return
   */
  def productionMap(skillTree: SkillTree): ListMap[Resource, Double] = sumValues(
    for {
      (building, numBuildings) <- buildings.toSeq
      (resource, resourceQty) <- building.outputsMap.toSeq
    } yield resource -> resourceQty * numBuildings
  )

}
