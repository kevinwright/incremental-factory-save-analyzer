package gameanalyzer

import gameanalyzer.model.*

import scala.annotation.tailrec

class Simulation(gameState: GameState) {

  case class CalculatedParcel(
      underlying: Parcel,
      resourceImports: Map[Resource, Double],
      consumption: Map[Resource, Double],
      production: Map[Resource, Double],
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
        .filter(tid => parcels.exists(_.underlying.id == tid))

    def starvedParcels = parcels.filter(_.deficit.nonEmpty)
  }

  object Raw {
    val allParcels: Seq[Parcel] = gameState.parcels.parcelList
    val allConnections = gameState.nodeConnections
    val allImporterIds = allConnections.map(_.targetId).toSet
    val rootParcels = allParcels.filterNot(p => allImporterIds.contains(p.id))

    def parcelById(id: String): Parcel =
      allParcels.find(_.id == id).get

    def outgoingFrom(parcel: Parcel): Seq[(NodeConnection, Parcel)] = {
      allConnections
        .filter(_.sourceId == parcel.id)
        .map(conn => conn -> parcelById(conn.targetId))

    }

    def incomingTo(parcel: Parcel): Seq[(Parcel, NodeConnection)] = {
      allConnections
        .filter(_.targetId == parcel.id)
        .map(conn => parcelById(conn.sourceId) -> conn)
    }
  }

  private def calculateParcel(
      parcel: Parcel,
      imports: Map[Resource, Double]
  ): CalculatedParcel = {
    val buildings = parcel.buildings

    val consumption = parcel.consumptionMap
    val production = parcel.productionMap

    val netConsumption: Map[Resource, Double] =
      consumption.map { case (r, n) =>
        val available =
          imports.getOrElse(r, 0.0d) + production.getOrElse(r, 0.0d)
        (r, available - n)
      }

    CalculatedParcel(
      underlying = parcel,
      resourceImports = imports,
      consumption = consumption,
      production = production,
      deficit = netConsumption.filter(_._2 < 0),
      excess = netConsumption.filter(_._2 > 0)
    )
  }

  private def calculateConnections(
      parcel: CalculatedParcel
  ): Seq[CalculatedConnection] = {
    Raw.allConnections
      .filter(_.sourceId == parcel.underlying.id)
      .map { c =>
        val resource = c.sourceHandle
          .map(Resource.valueOf)
          .getOrElse(Resource.nullResource)

        val maxThroughput = gameState.skilltree.maxThroughputFor(resource)
        val available = parcel.excess.getOrElse(resource, 0.0d)
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
      Raw.rootParcels.map(p => calculateParcel(p, Map.empty))

    val calculatedConnections =
      calculatedParcels.flatMap(calculateConnections)
    SimulationState(calculatedParcels, calculatedConnections)
  }

  @tailrec
  private def iterate(state: SimulationState): SimulationState = {
    val nextTranche =
      Raw.allParcels.filter(p => state.unresolvedParcelIds.contains(p.id))

    if (nextTranche.isEmpty) {
      state
    } else {
      val newCalculatedParcels = nextTranche.map { p =>
        val inbounds = state.connections.filter(_.targetId == p.id)
        val imports = inbounds
          .groupMap(_.resource)(_.actualThroughput)
          .view
          .mapValues(_.sum)
          .toMap
        calculateParcel(p, imports)
      }
      val newCalculatedConnections =
        newCalculatedParcels.flatMap(calculateConnections)
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
