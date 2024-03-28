package gameanalyzer

import gameanalyzer.model.*
import gameanalyzer.CollectionUtils.*
import gameanalyzer.model.Resource.nullResource

import scala.annotation.tailrec

object Simulation {
  case class CalculatedParcel(
                               underlying: ParcelInstance,
                               resourceImports: Seq[(Resource, Double)],
                               unboostedProduction: Map[Resource, Double],
                               production: Map[Resource, Double],
                               consumption: Map[Resource, Double],
                               availability: Map[Resource, Double],
                               deficit: Map[Resource, Double],
                               excess: Map[Resource, Double]
  )

  case class CalculatedConnection(
      underlying: NodeConnection,
      resource: Resource,
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
        r: Resource = nullResource
    ): Seq[CalculatedConnection] = {
      if r == Resource.nullResource then
        connections.filter(_.targetId == c.underlying.id)
      else
        connections.filter(c =>
          c.targetId == c.underlying.id && c.resource == r
        )
    }

    def exportsFrom(
        c: CalculatedParcel,
        r: Resource = nullResource
    ): Seq[CalculatedConnection] = {
      if r == Resource.nullResource then connections.filter(_.source == c)
      else connections.filter(c => c.source == c && c.resource == r)
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

    def outgoingFrom(parcel: ParcelInstance): Seq[(NodeConnection, ParcelInstance)] = {
      allConnections
        .filter(_.sourceId == parcel.id)
        .map(conn => conn -> parcelById(conn.targetId))

    }

    def incomingTo(parcel: ParcelInstance): Seq[(ParcelInstance, NodeConnection)] = {
      allConnections
        .filter(_.targetId == parcel.id)
        .map(conn => parcelById(conn.sourceId) -> conn)
    }
  }

  private def calculateParcel(
                               parcel: ParcelInstance,
                               imports: Seq[(Resource, Double)]
  ): CalculatedParcel = {
    val consumption = parcel.consumptionMap
    val production = parcel.productionMapForSkills(gameState.skilltree)

    val availability: Map[Resource, Double] =
      Resource.values.toSeq.flatMap { r =>
        val imported = imports.sumValues.getOrElse(r, 0.0d)
        val produced = production.getOrElse(r, 0.0d)
        val sum = imported + produced
        if sum > 0 then Some(r -> sum) else None
      }.toMap

    val deficit: Map[Resource, Double] =
      consumption.flatMap { case (r, n) =>
        val delta = availability.getOrElse(r, 0.0d) - n
        if delta < 0 then Some(r -> delta) else None
      }

    val excess: Map[Resource, Double] =
      availability.flatMap { case (r, n) =>
        val delta = n - consumption.getOrElse(r, 0.0d)
        if delta > 0 then Some(r -> delta) else None
      }

    CalculatedParcel(
      underlying = parcel,
      resourceImports = imports,
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
      val resource = c.sourceHandle
        .map(Resource.valueOf)
        .getOrElse(Resource.nullResource)

      val shareCount = underlying.count(_.sourceHandle.contains(resource))

      val maxThroughput = gameState.skilltree.maxThroughputFor(resource)
      val available =
        parcel.excess.getOrElse(resource, 0.0d) / shareCount.doubleValue
      CalculatedConnection(
        underlying = c,
        resource = resource,
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
        val imports: Seq[(Resource, Double)] =
          inbounds.map(cc => cc.resource -> cc.actualThroughput)
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
