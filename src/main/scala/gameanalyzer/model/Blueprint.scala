package gameanalyzer.model

case class Blueprint(
    name: String,
    buildings: Seq[BlueprintBuilding]
)
