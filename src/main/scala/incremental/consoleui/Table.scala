package incremental.consoleui

import UiUtils.*
import Justification.*
import fansi.{Attrs, Color}

import scala.collection.immutable.Seq

object Table {
  case class ColMeta(
      header: Any,
      width: Int
  )

  val rainbow = List(
    Color.Magenta,
    Color.Red,
    Color.Yellow,
    Color.Green,
    Color.Blue,
    Color.Cyan
  )

  def buildTreeTable[A](
      title: Option[Any],
      headerRow: Seq[Any],
      roots: Seq[A],
      walkDown: A => Seq[A],
      mkRow: A => Seq[Any]
  ): Table = {
    def recurse(
        nodes: Seq[A],
        inheritedPrefix: fansi.Str = fansi.Str(""),
        nestLevel: Int = 0
    ): Seq[Seq[Any]] = {
      def doOne(node: A, isLast: Boolean = false): Seq[Seq[Any]] = {
        val rawRow = mkRow(node)

        val prefixColor = Attrs(rainbow(nestLevel % 6))

        val prefix: fansi.Str =
          if nestLevel == 0 then fansi.Str("")
          else {
            if isLast then {
              inheritedPrefix ++ fansi.Str("╰ ").overlay(prefixColor)
            } else {
              inheritedPrefix ++ fansi.Str("├ ").overlay(prefixColor)
            }
          }

        val thisRow =
          (prefix ++ UiUtils.stringify(rawRow.head)) +: rawRow.tail

        val nestedPrefix =
          if nestLevel == 0 then fansi.Str("")
          else {
            if isLast then {
              fansi.Str("  ")
            } else {
              fansi.Str("│ ").overlay(prefixColor)
            }
          }

        val nestedRows = recurse(
          walkDown(node),
          inheritedPrefix ++ nestedPrefix,
          nestLevel + 1
        )
        thisRow +: nestedRows
      }

      if nodes.length > 1 then {
        nodes.init.flatMap(doOne(_)) :++
          doOne(nodes.last, isLast = true)
      } else {
        nodes.flatMap(n => doOne(n, isLast = true)) // 1 or 0 elems
      }
    }

    val bodyRows = recurse(roots)
    Table(title, headerRow, bodyRows)
  }
}

import Table.*

case class Table(
    title: Option[Any],
    headerRow: Seq[Any],
    bodyRows: Seq[Seq[Any]],
    padding: Int = 1,
    justifier: Justification.determinant = Justification.defaultDeterminant
) {

  lazy val colDefs: Seq[ColMeta] = {
    bodyRows match {
      case Seq() => Seq.empty
      case _ =>
        val bodyRowSizes =
          for (row <- bodyRows) yield {
            assert(
              row.length == headerRow.length,
              "Row length doesn't match header"
            )
            for (cell <- row) yield {
              if cell == null then 0
              else textLength(cell)
            }
          }

        val headerSizes = headerRow.map(textLength)

        val colSizes =
          (headerSizes +: bodyRowSizes).transpose.map(_.max)

        val result = headerRow.zip(colSizes).map(ColMeta(_, _))
        result
    }
  }

  def alignedHeader: Seq[fansi.Str] =
    alignRow(colDefs.map(_.copy(header = "")), headerRow, padding, justifier)

  def alignedBody: Seq[Seq[fansi.Str]] =
    for (row <- bodyRows) yield alignRow(colDefs, row, padding, justifier)

  private def alignRow(
      colDefs: Seq[ColMeta],
      row: Seq[Any],
      padding: Int,
      justifier: Justification.determinant
  ): Seq[fansi.Str] = {
    for (item, colMeta) <- row zip colDefs
    yield
      if colMeta.width == 0 then fansi.Str("")
      else {
        justifier(colMeta.header, item).justify(
          stringify(item),
          colMeta.width,
          padding
        )
      }
  }

  def withBorders(
      borderStyle: LineStyle = LineStyle.Heavy,
      headerSeperatorStyle: LineStyle = LineStyle.Light,
      horizStyle: LineStyle = LineStyle.NoLine,
      vertStyle: LineStyle = LineStyle.Light
  ): BorderedTable = BorderedTable(
    this,
    borderStyle,
    headerSeperatorStyle,
    horizStyle,
    vertStyle
  )

}
