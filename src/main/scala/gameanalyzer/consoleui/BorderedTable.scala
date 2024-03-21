package gameanalyzer.consoleui

import gameanalyzer.consoleui.LineStyle.*
import Table.*
import BoxArt.given
import gameanalyzer.consoleui.UiUtils.stringify

class BorderedTable(
    underlying: Table,
    borderStyle: LineStyle = Heavy,
    headerSeperatorStyle: LineStyle = Light,
    horizStyle: LineStyle = NoLine,
    vertStyle: LineStyle = Light
) {
  lazy val colWidths = underlying.colDefs.map(_.width + 2 * underlying.padding)
  lazy val colWidthTotal = colWidths.sum + colWidths.size - 1

  private val horzBorder = BoxArt.vert(borderStyle).toCharStr
  private val vertBorder = BoxArt.vert(borderStyle).toCharStr
  private val vertSep = BoxArt.vert(vertStyle).toCharStr
  private val tlBorder = BoxArt.tlCorner(borderStyle).toCharStr
  private val trBorder = BoxArt.trCorner(borderStyle).toCharStr
  private val blBorder = BoxArt.blCorner(borderStyle).toCharStr
  private val brBorder = BoxArt.brCorner(borderStyle).toCharStr
  private val teeRightBorder = BoxArt.tRight(borderStyle, borderStyle).toCharStr
  private val teeLeftBorder = BoxArt.tLeft(borderStyle, borderStyle).toCharStr
  private val teeDownBorder = BoxArt.tDown(borderStyle, vertStyle).toCharStr
  private val teeUpBorder = BoxArt.tUp(borderStyle, vertStyle).toCharStr

  private val titleBlock: String = underlying.title match {
    case Some(str) =>
      val titleStr = stringify(str).overlay(fansi.Bold.On)
      tlBorder
        + BoxArt.horzLine(colWidthTotal, borderStyle)
        + trBorder + "\n"
        + vertBorder
        + Justification.Centred.justify(
          titleStr,
          colWidthTotal - 2
        ) + vertBorder
        + "\n"
    case None => ""
  }
  private val topBorder: String = {
    val (l, r) =
      if underlying.title.isDefined then (teeRightBorder, teeLeftBorder)
      else (tlBorder, trBorder)
    colWidths
      .map(w => BoxArt.horzLine(w, borderStyle))
      .mkString(l, teeDownBorder, r)
  }

  private val bottomBorder: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, borderStyle))
      .mkString(
        blBorder,
        teeUpBorder,
        brBorder
      )
  }

  private val headerSeparator: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, headerSeperatorStyle))
      .mkString(
        BoxArt.tRight(borderStyle, headerSeperatorStyle).toCharStr,
        BoxArt.cross(headerSeperatorStyle, vertStyle).toCharStr,
        BoxArt.tLeft(borderStyle, headerSeperatorStyle).toCharStr
      )
  }

  private val rowSeparator: String = {
    colWidths
      .map(w => BoxArt.horzLine(w, horizStyle))
      .mkString(
        BoxArt.tRight(borderStyle, horizStyle).toCharStr,
        BoxArt.cross(horizStyle, vertStyle).toCharStr,
        BoxArt.tLeft(borderStyle, horizStyle).toCharStr
      )
  }

  def toStringBlock: String = {
    titleBlock +
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
