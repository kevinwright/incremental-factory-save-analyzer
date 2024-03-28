package gameanalyzer.wiki

import gameanalyzer.model.{Building, ParcelType, Resource}

object WikiTables {

  def mkTable(
      headers: Seq[String],
      cellAlignments: Seq[String],
      rows: Seq[Seq[Any]]
  ): String = {
    val startMarkup = "{| class=\"sortable wikitable\"\n"
    val headerMarkup = headers.mkString("! ", "\n! ", "\n")

    rows
      .map(row =>
        (row zip cellAlignments)
          .map((c, a) => s"| style=\"text-align: $a;\" | $c")
          .mkString("", "\n", "\n")
      )
      .mkString(s"$startMarkup$headerMarkup|-\n", "|-\n", "|}\n")
  }

  def itemsTable: String = {
    mkTable(
      Seq("Item", "Category", "Order", "Unlocks<br>With"),
      Seq("left", "left", "right", "left"),
      Resource.values
        .filterNot(_ == Resource.nullResource)
        .sortBy(_.ordinal)
        .map(r =>
          Seq(
            r.wikiLinkWithIcon(24),
            r.category,
            r.order,
            r.techTier.keyResource.wikiLinkWithIcon(24)
          )
        )
    )
  }

  def buildingsTable: String = {
    mkTable(
      Seq("Building", "Output", "Category", "Energy", "Unlocks<br>With"),
      Seq("left", "left", "left", "right", "left"),
      Building.values
        .filterNot(_ == Building.testBuilding)
        .sortBy(b => (b.techTier.ordinal, b.ordinal))
        .map(b =>
          Seq(
            b.wikiLinkWithIcon(24),
            b.mainOutput.resource.wikiLink,
            b.category,
            b.netEnergy,
            b.techTier.keyResource.wikiLinkWithIcon(24)
          )
        )
    )
  }

  def parcelTypesTable: String = {
    val preamble =
      """
          |{| class="sortable wikitable" style="border: none; background: none;"
          |! style="border: none; background: none;" |
          |! style="text-align: centre;" colspan=3 | Base
          ||-
          |! style="text-align: left;" | Name
          |! style="text-align: centre;" | Connections
          |! style="text-align: centre;" | Buildings
          |! style="text-align: centre;" | Storage
          |""".stripMargin

    val rows = ParcelType.values.map { pt =>
      s"""
          ||-
          || style="text-align: left;" | [parcel${pt.name()}|${pt.name()}]
          || style="text-align: right;" | ${pt.baseLimits.connections}
          || style="text-align: right;" | ${pt.baseLimits.buildings}
          || style="text-align: right;" | ${pt.baseLimits.storage}
          |""".stripMargin
    }

    val endTable = "|}\n"
    rows.mkString(preamble, "", endTable)
  }
}
