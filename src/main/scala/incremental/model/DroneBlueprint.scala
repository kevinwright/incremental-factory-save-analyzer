package incremental.model

case class DroneBlueprint(
                           name: String,
                           details: Seq[ParcelInstance],
                           connections: Seq[NodeConnection]
)
