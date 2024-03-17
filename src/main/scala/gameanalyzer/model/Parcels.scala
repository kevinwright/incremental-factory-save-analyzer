package gameanalyzer.model

case class Parcels(
    parcelList: Seq[Parcel],
    currentParcelId: String,
    maxBuildingsPerParcel: Int,
    upgradeCosts: UpgradeCosts,
    globalProductionRateModifiers: RateModifiers,
    globalConsumptionRateModifiers: RateModifiers
)
