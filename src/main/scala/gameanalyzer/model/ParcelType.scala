package gameanalyzer.model

enum ParcelType(
    baseConnections: Int,
    baseBuildings: Int
) extends Enum[ParcelType] {
  val baseLimits: ParcelLimits = ParcelLimits(baseConnections, baseBuildings)

  case HQ extends ParcelType(8, 64)
  case advanced extends ParcelType(4, 48)
  case hitech extends ParcelType(6, 32)
  case basic extends ParcelType(2, 16)
  case large extends ParcelType(2, 64)
  case node extends ParcelType(8, 1)
}
