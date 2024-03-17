package gameanalyzer.consoleui

import gameanalyzer.consoleui.LineStyle.*
import Table.*

class BorderedTable(
    underlying: Table,
    borderStyle: LineStyle = Heavy,
    headerSeperatorStyle: LineStyle = Light,
    horizStyle: LineStyle = NoLine,
    vertStyle: LineStyle = Light
) {
  lazy val colWidths = underlying.colDefs.map(_.width + 2 * underlying.padding)

  private val topBorder: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, borderStyle))
      .mkString(
        BoxArt.tlCorner(borderStyle).toChar.toString,
        BoxArt.tDown(borderStyle, vertStyle).toChar.toString,
        BoxArt.trCorner(borderStyle).toChar.toString
      )
  }

  private val bottomBorder: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, borderStyle))
      .mkString(
        BoxArt.blCorner(borderStyle).toChar.toString,
        BoxArt.tUp(borderStyle, vertStyle).toChar.toString,
        BoxArt.brCorner(borderStyle).toChar.toString
      )
  }

  private val headerSeparator: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, headerSeperatorStyle))
      .mkString(
        BoxArt.tRight(borderStyle, headerSeperatorStyle).toChar.toString,
        BoxArt.cross(headerSeperatorStyle, vertStyle).toChar.toString,
        BoxArt.tLeft(borderStyle, headerSeperatorStyle).toChar.toString
      )
  }

  private val rowSeparator: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, horizStyle))
      .mkString(
        BoxArt.tRight(borderStyle, horizStyle).toChar.toString,
        BoxArt.cross(horizStyle, vertStyle).toChar.toString,
        BoxArt.tLeft(borderStyle, horizStyle).toChar.toString
      )
  }

  private val vertBorder = BoxArt.vert(borderStyle).toChar.toString
  private val vertSep = BoxArt.vert(vertStyle).toChar.toString

  def toStringBlock: String = {
    topBorder +
      "\n" +
      underlying.alignedHeader.mkString(vertBorder, vertSep, vertBorder) +
      "\n" +
      headerSeparator +
      underlying.alignedBody
        .map(row => row.mkString(vertBorder, vertSep, vertBorder))
        .mkString("\n", "\n", "\n") +
      bottomBorder
  }
}
