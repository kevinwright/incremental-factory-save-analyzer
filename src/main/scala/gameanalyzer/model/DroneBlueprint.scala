package gameanalyzer.model

case class DroneBlueprint(
    name: String,
    details: Seq[Parcel],
    connections: Seq[NodeConnection]
)
