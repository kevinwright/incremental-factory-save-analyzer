package incremental

import incremental.model.*

sealed trait Formatter {
  def formatBuilding(b: Building): String
  def formatItem(i: Item): String
  def formatCountedItem(cr: CountedItem): String
  def formatNumber(d: Double): String =
    if math.abs(d - d.floor) < 0.00001d then d.intValue.toString
    else d.toString

  def formatCommaList(
      seq: Seq[String],
      conjunction: String = "and"
  ): String = {
    val result = seq.length match {
      case 0 => ""
      case 1 => seq.head
      case 2 => s"${seq.head} $conjunction ${seq.last}"
      case _ => seq.init.mkString("", ", ", s", $conjunction ${seq.last}")
    }
    result
  }
}

case class WikiFormatter(
    iconSize: Int = 24,
    showItemIcons: Boolean = true,
    showItemText: Boolean = true,
    showBuildingIcons: Boolean = true
) extends Formatter {

  private def itemIconLink(r: Item): String =
    if showItemIcons
    then
      s"[[File:${r.icon}|link=${r.displayName}|alt=${r.displayName}|${iconSize}px]]"
    else ""

  private def itemTextLink(r: Item): String =
    if showItemText
    then s"[[${r.displayName}]]"
    else ""

  private def itemComboLink(r: Item): String = {
    (itemIconLink(r) + " " + itemTextLink(r)).trim
  }

  private def buildingIconLink(b: Building): String =
    if showBuildingIcons
    then {
      val r = b.mainOutput.item
      s"[[File:${r.icon}|link=${r.displayName}|alt=${r.displayName}|${iconSize}px]]"
    } else ""

  private def buildingTextLink(b: Building): String =
    s"[[${b.displayName}]]"

  private def buildingComboLink(b: Building): String =
    (buildingIconLink(b) + " " + buildingTextLink(b)).trim

  def formatItem(i: Item): String =
    if i == Item.nullItem then ""
    else {
      val link = itemComboLink(i)
      s"{{Nowrap|$link}}"
    }

  def formatBuilding(b: Building): String =
    s"{{Nowrap|${buildingComboLink(b)}}}"

  def formatCountedItem(ci: CountedItem): String = {
    val qty = formatNumber(ci.qty)
    val link = itemComboLink(ci.item)
    s"{{Nowrap|$qty  × $link}}"
  }
}
