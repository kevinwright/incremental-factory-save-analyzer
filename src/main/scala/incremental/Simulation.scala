package incremental

import incremental.model.*
import incremental.CollectionUtils.*
import incremental.model.Item.nullItem

import scala.annotation.tailrec

object Simulation {
  case class CalculatedParcel(
      underlying: ParcelInstance,
      itemImports: Seq[(Item, Double)],
      unboostedProduction: Map[Item, Double],
      production: Map[Item, Double],
      consumption: Map[Item, Double],
      availability: Map[Item, Double],
      deficit: Map[Item, Double],
      excess: Map[Item, Double]
  )

  case class CalculatedConnection(
      underlying: NodeConnection,
      item: Item,
      maxThroughput: Double,
      actualThroughput: Double,
      source: CalculatedParcel,
      targetId: String
  )

  case class SimulationState(
      parcels: Seq[CalculatedParcel],
      connections: Seq[CalculatedConnection]
  ) {
    def unresolvedParcelIds: Seq[String] =
      connections
        .map(_.targetId)
        .filterNot(tid => parcels.exists(_.underlying.id == tid))

    def importsTo(
        c: CalculatedParcel,
        r: Item = nullItem
    ): Seq[CalculatedConnection] = {
      if r == Item.nullItem then
        connections.filter(_.targetId == c.underlying.id)
      else connections.filter(c => c.targetId == c.underlying.id && c.item == r)
    }

    def exportsFrom(
        c: CalculatedParcel,
        r: Item = nullItem
    ): Seq[CalculatedConnection] = {
      if r == Item.nullItem then connections.filter(_.source == c)
      else connections.filter(c => c.source == c && c.item == r)
    }
  }
}
class Simulation(gameState: GameState) {
  import Simulation.*

  object Raw {
    val allParcels: Seq[ParcelInstance] = gameState.parcels.instances
    val allConnections = gameState.nodeConnections
    val allImporterIds = allConnections.map(_.targetId).toSet
    val rootParcels = allParcels.filterNot(p => allImporterIds.contains(p.id))

    def parcelById(id: String): ParcelInstance =
      allParcels.find(_.id == id).get

    def outgoingFrom(
        parcel: ParcelInstance
    ): Seq[(NodeConnection, ParcelInstance)] = {
      allConnections
        .filter(_.sourceId == parcel.id)
        .map(conn => conn -> parcelById(conn.targetId))

    }

    def incomingTo(
        parcel: ParcelInstance
    ): Seq[(ParcelInstance, NodeConnection)] = {
      allConnections
        .filter(_.targetId == parcel.id)
        .map(conn => parcelById(conn.sourceId) -> conn)
    }
  }

  private def calculateParcel(
      parcel: ParcelInstance,
      imports: Seq[(Item, Double)]
  ): CalculatedParcel = {
    val consumption = parcel.consumptionMap
    val production = parcel.productionMapForSkills(gameState.skilltree)

    val availability: Map[Item, Double] =
      Item.values.toSeq.flatMap { r =>
        val imported = imports.sumValues.getOrElse(r, 0.0d)
        val produced = production.getOrElse(r, 0.0d)
        val sum = imported + produced
        if sum > 0 then Some(r -> sum) else None
      }.toMap

    val deficit: Map[Item, Double] =
      consumption.flatMap { case (r, n) =>
        val delta = availability.getOrElse(r, 0.0d) - n
        if delta < 0 then Some(r -> delta) else None
      }

    val excess: Map[Item, Double] =
      availability.flatMap { case (r, n) =>
        val delta = n - consumption.getOrElse(r, 0.0d)
        if delta > 0 then Some(r -> delta) else None
      }

    CalculatedParcel(
      underlying = parcel,
      itemImports = imports,
      consumption = consumption,
      availability = availability,
      unboostedProduction = parcel.unboostedProductionMap,
      production = production,
      deficit = deficit,
      excess = excess
    )
  }

  private def calculateOutboundConnections(
      parcel: CalculatedParcel
  ): Seq[CalculatedConnection] = {
    val underlying = Raw.allConnections
      .filter(_.sourceId == parcel.underlying.id)

    underlying.map { c =>
      val item = c.sourceHandle
        .map(Item.valueOf)
        .getOrElse(Item.nullItem)

      val shareCount = underlying.count(_.sourceHandle.contains(item))

      val maxThroughput = gameState.skilltree.maxThroughputFor(item)
      val available =
        parcel.excess.getOrElse(item, 0.0d) / shareCount.doubleValue
      CalculatedConnection(
        underlying = c,
        item = item,
        maxThroughput = maxThroughput,
        actualThroughput = maxThroughput min available,
        source = parcel,
        targetId = c.targetId
      )
    }
  }

  def calculateRoots: SimulationState = {
    val calculatedParcels =
      Raw.rootParcels.map(p => calculateParcel(p, Nil))

    val calculatedConnections =
      calculatedParcels.flatMap(calculateOutboundConnections)
    SimulationState(calculatedParcels, calculatedConnections)
  }

  @tailrec
  private def iterate(state: SimulationState): SimulationState = {
    val nextTranche =
      Raw.allParcels.filter(p => {
        val rawConns = Raw.allConnections.filter(_.targetId == p.id)
        state.unresolvedParcelIds.contains(p.id)
        && rawConns.forall(nc => state.connections.exists(_.underlying == nc))
      })

    if (nextTranche.isEmpty) {
      state
    } else {
      val newCalculatedParcels = nextTranche.map { p =>
        val inbounds = state.connections.filter(_.targetId == p.id)
        val imports: Seq[(Item, Double)] =
          inbounds.map(cc => cc.item -> cc.actualThroughput)
        calculateParcel(p, imports)
      }
      val newCalculatedConnections =
        newCalculatedParcels.flatMap(calculateOutboundConnections)

      iterate(
        state.copy(
          parcels = state.parcels ++ newCalculatedParcels,
          connections = state.connections ++ newCalculatedConnections
        )
      )
    }
  }

  def run(): SimulationState = {
    iterate(calculateRoots)
  }
}
