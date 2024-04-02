package incremental.model

enum TechTier(
    keyItemId: String
) {
  lazy val keyItem: Item =
    Item.valueOf(keyItemId)

  case Foundation extends TechTier("nullItem")
  case Steel extends TechTier("steel")
  case Gen1 extends TechTier("gen1Chip")
  case Gen2 extends TechTier("gen2Chip")
  case Gen3 extends TechTier("gen3Chip")
  case Gen4 extends TechTier("gen4Chip")
  case Gen5 extends TechTier("gen5Chip")
}
