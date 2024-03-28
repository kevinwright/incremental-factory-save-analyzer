package gameanalyzer.model

enum TechTier(
    keyResourceId: String
) {
  lazy val keyResource: Resource =
    Resource.valueOf(keyResourceId)

  case Foundation extends TechTier("nullResource")
  case Steel extends TechTier("steel")
  case Gen1 extends TechTier("gen1Chip")
  case Gen2 extends TechTier("gen2Chip")
  case Gen3 extends TechTier("gen3Chip")
  case Gen4 extends TechTier("gen4Chip")
  case Gen5 extends TechTier("gen5Chip")
}
