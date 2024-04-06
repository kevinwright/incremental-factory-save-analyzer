package incremental.wiki

import incremental.WikiFormatter
import incremental.model.*
import incremental.model.Item.{
  blueScience,
  greenScience,
  purpleScience,
  redScience,
  whiteScience,
  yellowScience
}

object WikiTables {

  def defaultFormatter = WikiFormatter()
  def iconOnlyFormatter = WikiFormatter(showItemText = false)
  def textOnlyFormatter =
    WikiFormatter(showItemIcons = false, showBuildingIcons = false)

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

  private def costQty(costs: Iterable[CountedItem], item: Item): String =
    costs
      .find(_.item == item)
      .map(_.qty)
      .map(textOnlyFormatter.formatNumber)
      .getOrElse("")

  def simpleResearchesTable: String =
    mkTable(
      Seq(
        "Name",
        iconOnlyFormatter.formatItem(redScience),
        iconOnlyFormatter.formatItem(greenScience),
        iconOnlyFormatter.formatItem(blueScience),
        iconOnlyFormatter.formatItem(purpleScience),
        iconOnlyFormatter.formatItem(yellowScience),
        iconOnlyFormatter.formatItem(whiteScience),
        "Unlocks"
      ),
      Seq("left", "right", "right", "right", "right", "right", "right", "left"),
      SimpleResearch.ordered
        .map(x =>
          Seq(
            s"[[${x.wikiTitle}]]",
            costQty(x.cost, redScience),
            costQty(x.cost, greenScience),
            costQty(x.cost, blueScience),
            costQty(x.cost, purpleScience),
            costQty(x.cost, yellowScience),
            costQty(x.cost, whiteScience),
            textOnlyFormatter.formatUnlocks(x.unlocks)
          )
        )
    )
}
