package incremental.model

case class Blueprint(
    name: String,
    buildings: Seq[BlueprintBuilding]
)
