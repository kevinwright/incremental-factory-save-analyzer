package incremental.model

import com.github.plokhotnyuk.jsoniter_scala.macros.named

case class ParcelInstanceCollection(
    @named("parcelList") instances: Seq[ParcelInstance],
    currentParcelId: String,
    maxBuildingsPerParcel: Int,
    upgradeCosts: UpgradeCosts,
    globalProductionRateModifiers: RateModifiers,
    globalConsumptionRateModifiers: RateModifiers
)
