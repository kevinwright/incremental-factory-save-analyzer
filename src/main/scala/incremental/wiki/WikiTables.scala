package incremental.wiki

import incremental.WikiFormatter
import incremental.model.*

object WikiTables {

  def defaultFormatter = WikiFormatter(
    iconSize = 24,
    showItemIcons = true,
    showBuildingIcons = true,
    showItemText = true
  )

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
      Item.ordered
        .filterNot(_ == Item.nullItem)
        .map(r =>
          Seq(
            defaultFormatter.formatItem(r),
            r.category,
            r.order,
            defaultFormatter.formatItem(r.techTier.keyItem)
          )
        )
    )
  }

  def buildingsTable: String = {
    mkTable(
      Seq("Building", "Output", "Category", "Energy", "Unlocks<br>With"),
      Seq("left", "left", "left", "right", "left"),
      Buildings.ordered
        .filterNot(_ == Building.testBuilding)
        .sortBy(b => (b.techTier.ordinal, b.ordinal))
        .map(b =>
          Seq(
            defaultFormatter.formatBuilding(b),
            defaultFormatter.formatItem(b.mainOutput.item),
            b.category,
            b.netEnergy,
            defaultFormatter.formatItem(b.techTier.keyItem)
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

    val rows = ParcelTypes.ordered.map { pt =>
      s"""
          ||-
          || style="text-align: left;" | [[${pt.displayName}]]
          || style="text-align: right;" | ${pt.baseLimits.connections}
          || style="text-align: right;" | ${pt.baseLimits.buildings}
          || style="text-align: right;" | ${pt.baseLimits.storage}
          |""".stripMargin
    }

    val endTable = "|}\n"
    rows.mkString(preamble, "", endTable)
  }

  def simpleResearchesTable: String =
    mkTable(
      Seq("Name", "Unlocks"),
      Seq("left", "left"),
      SimpleResearch.ordered
        .map(x =>
          Seq(
            s"[[${x.wikiTitle}]]",
            defaultFormatter.formatUnlocks(x.unlocks)
          )
        )
    )
}
