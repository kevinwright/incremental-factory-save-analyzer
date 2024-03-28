package gameanalyzer.consoleui

import gameanalyzer.consoleui.LineStyle.*
import gameanalyzer.consoleui.UiUtils.stringify
import UiUtils.mkFansiStr

class BorderedTable(
    underlying: Table,
    borderStyle: LineStyle = Heavy,
    headerSeperatorStyle: LineStyle = Light,
    horizStyle: LineStyle = NoLine,
    vertStyle: LineStyle = Light
) {
  lazy val colWidths = underlying.colDefs.map(_.width + 2 * underlying.padding)
  lazy val colWidthTotal = colWidths.sum + colWidths.size - 1

  private val horzBorder = BoxArt.vert(borderStyle).toFansi
  private val vertBorder = BoxArt.vert(borderStyle).toFansi
  private val vertSep = BoxArt.vert(vertStyle).toFansi
  private val tlBorder = BoxArt.tlCorner(borderStyle).toFansi
  private val trBorder = BoxArt.trCorner(borderStyle).toFansi
  private val blBorder = BoxArt.blCorner(borderStyle).toFansi
  private val brBorder = BoxArt.brCorner(borderStyle).toFansi
  private val teeRightBorder = BoxArt.tRight(borderStyle, borderStyle).toFansi
  private val teeLeftBorder = BoxArt.tLeft(borderStyle, borderStyle).toFansi
  private val teeDownBorder = BoxArt.tDown(borderStyle, vertStyle).toFansi
  private val teeUpBorder = BoxArt.tUp(borderStyle, vertStyle).toFansi

  private val titleBlock: fansi.Str = underlying.title match {
    case Some(str) =>
      val titleStr = stringify(str).overlay(fansi.Bold.On)
      tlBorder
        ++ BoxArt.horzLine(colWidthTotal, borderStyle)
        ++ trBorder ++ "\n"
        ++ vertBorder
        ++ Justification.Centred.justify(
          titleStr,
          colWidthTotal - 2
        ) ++ vertBorder
        ++ "\n"
    case None => ""
  }
  private val topBorder: fansi.Str = {
    val (l, r) =
      if underlying.title.isDefined then (teeRightBorder, teeLeftBorder)
      else (tlBorder, trBorder)
    colWidths
      .map(w => BoxArt.horzLine(w, borderStyle))
      .mkFansiStr(l, teeDownBorder, r)
  }

  private val bottomBorder: fansi.Str = {
    colWidths
      .map(w => BoxArt.horzLine(w, borderStyle))
      .mkFansiStr(
        blBorder,
        teeUpBorder,
        brBorder
      )
  }

  private val headerSeparator: fansi.Str = {
    colWidths
      .map(w => BoxArt.horzLine(w, headerSeperatorStyle))
      .mkFansiStr(
        BoxArt.tRight(borderStyle, headerSeperatorStyle).toCharStr,
        BoxArt.cross(headerSeperatorStyle, vertStyle).toCharStr,
        BoxArt.tLeft(borderStyle, headerSeperatorStyle).toCharStr
      )
  }

  private val rowSeparator: fansi.Str = {
    colWidths
      .map(w => BoxArt.horzLine(w, horizStyle))
      .mkFansiStr(
        BoxArt.tRight(borderStyle, horizStyle).toCharStr,
        BoxArt.cross(horizStyle, vertStyle).toCharStr,
        BoxArt.tLeft(borderStyle, horizStyle).toCharStr
      )
  }

  def toStringBlock: fansi.Str = {
    titleBlock ++
      topBorder ++
      "\n" ++
      underlying.alignedHeader.mkFansiStr(vertBorder, vertSep, vertBorder) ++
      "\n" ++
      headerSeparator ++
      underlying.alignedBody
        .map(row => row.mkFansiStr(vertBorder, vertSep, vertBorder))
        .mkFansiStr("\n", "\n", "\n") ++
      bottomBorder
  }
}
