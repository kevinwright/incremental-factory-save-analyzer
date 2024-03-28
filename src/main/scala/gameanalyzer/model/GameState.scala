package gameanalyzer.model

import scala.collection.immutable.ListMap

final case class GameState(
                            totalRewards: Int,
                            skilltree: SkillTree,
                            viewPort: Option[ViewPort],
                            maxParcels: Int,
                            maxClusters: Int,
                            taskQueueTickRate: Int,
                            tasks: Tasks,
                            parcels: ParcelInstanceCollection,
                            nodeConnections: Seq[NodeConnection],
                            // mapItems,
                            blueprint: Seq[(String, Blueprint)],
                            selectedCell: Option[String],
                            // resourcesInFocus,
                            // analysisMode,
                            // buyParcelCost,
                            selectedCluster: Int,
                            // clusterBuyParcelCosts
                            research: Map[String, Boolean],
                            unlockedBuildings: Seq[(String, Boolean)],
                            // sectionVisibility
                            // battle
                            // pollution
                            // scheduleList
                            // trainList
                            // maxTrains
                            maxChallenges: Int,
                            activeChallenges: Seq[ActiveChallenge],
                            currentChallengeCards: Seq[String],
                            selectedParcelName: Option[String],
                            autoSave: Boolean,
                            droneBlueprints: Seq[DroneBlueprint],
                            energyEfficiency: Option[Int],
                            // savedProjects
                            researchData: Seq[(String, Boolean)]
) {
  def throughputLimits: ListMap[Resource, Double] = {
    Resource.values
      .map(r => r -> skilltree.maxThroughputFor(r))
      .to(ListMap)
  }

  def prodBoosts: ListMap[Resource, Double] = {
    Resource.values
      .map(r => r -> skilltree.specializationBoostFor(r))
      .to(ListMap)
  }
}
