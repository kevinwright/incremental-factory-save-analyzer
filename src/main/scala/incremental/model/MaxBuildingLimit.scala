package incremental.model

case class MaxBuildingLimit(
    level: Int,
    cost: UpgradeCost,
    maxBuildingLimit: Int,
    inputAnchors: Int
)
