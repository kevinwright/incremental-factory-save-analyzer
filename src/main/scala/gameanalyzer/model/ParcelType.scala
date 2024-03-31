package gameanalyzer.model

enum ParcelType(
    val displayName: String,
    baseConnections: Int,
    baseBuildings: Int
) extends Enum[ParcelType] {
  val baseLimits: ParcelLimits = ParcelLimits(baseConnections, baseBuildings)

  case HQ extends ParcelType("HQ Parcel", 8, 64)
  case advanced extends ParcelType("Advanced Parcel", 4, 48)
  case hitech extends ParcelType("Hi-Tech Parcel", 6, 32)
  case basic extends ParcelType("Basic Parcel", 2, 16)
  case large extends ParcelType("Large Parcel", 2, 64)
  case node extends ParcelType("Node Parcel", 8, 1)
}
