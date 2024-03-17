package gameanalyzer.consoleui

import LineStyle.*

object BoxArt {
  final val diagUpRight = '╱'
  final val diagUpLeft = '╲'
  final val diagCross = '╳'

  def horz(style: LineStyle) = BoxArt(left = style, right = style)
  def vert(style: LineStyle) = BoxArt(top = style, bottom = style)

  def tlCorner(style: LineStyle) = BoxArt(right = style, bottom = style)
  def trCorner(style: LineStyle) = BoxArt(left = style, bottom = style)
  def brCorner(style: LineStyle) = BoxArt(left = style, top = style)
  def blCorner(style: LineStyle) = BoxArt(right = style, top = style)

  def tRight(longStyle: LineStyle, shortStyle: LineStyle) =
    BoxArt(top = longStyle, bottom = longStyle, right = shortStyle)

  def tLeft(longStyle: LineStyle, shortStyle: LineStyle) =
    BoxArt(top = longStyle, bottom = longStyle, left = shortStyle)

  def tDown(longStyle: LineStyle, shortStyle: LineStyle) =
    BoxArt(left = longStyle, right = longStyle, bottom = shortStyle)

  def tUp(longStyle: LineStyle, shortStyle: LineStyle) =
    BoxArt(left = longStyle, right = longStyle, top = shortStyle)

  def cross(horzStyle: LineStyle, vertStyle: LineStyle) =
    BoxArt(
      top = vertStyle,
      right = horzStyle,
      bottom = vertStyle,
      left = horzStyle
    )

  def horzLine(len: Int, style: LineStyle): String =
    horz(style).toChar.toString * len

}

given boxart2char: Conversion[BoxArt, Char] with
  def apply(ba: BoxArt): Char = ba.toChar

case class BoxArt(
    top: LineStyle = NoLine,
    right: LineStyle = NoLine,
    bottom: LineStyle = NoLine,
    left: LineStyle = NoLine
) {

  def toChar: Char = (top, right, bottom, left) match {
    // Empty
    case (NoLine, NoLine, NoLine, NoLine) => ' '

    // Light-Arc
    case (NoLine, LightArc, NoLine, LightArc) => '─'
    case (LightArc, NoLine, LightArc, NoLine) => '│'
    case (NoLine, LightArc, LightArc, NoLine) => '╭'
    case (NoLine, NoLine, LightArc, LightArc) => '╮'
    case (LightArc, NoLine, NoLine, LightArc) => '╯'
    case (LightArc, LightArc, NoLine, NoLine) => '╰'

    // Dashed Orthogonal
    case (NoLine, LightDoubleDash, NoLine, LightDoubleDash) => '╌'
    case (NoLine, LightTripleDash, NoLine, LightTripleDash) => '┄'
    case (NoLine, LightQuadDash, NoLine, LightQuadDash)     => '┈'
    case (NoLine, Heavy, NoLine, Heavy)                     => '━'
    case (NoLine, HeavyDoubleDash, NoLine, HeavyDoubleDash) => '╍'
    case (NoLine, HeavyTripleDash, NoLine, HeavyTripleDash) => '┅'
    case (NoLine, HeavyQuadDash, NoLine, HeavyQuadDash)     => '┉'
    case (LightDoubleDash, NoLine, LightDoubleDash, NoLine) => '╎'
    case (LightTripleDash, NoLine, LightTripleDash, NoLine) => '┆'
    case (LightQuadDash, NoLine, LightQuadDash, NoLine)     => '┊'
    case (HeavyDoubleDash, NoLine, HeavyDoubleDash, NoLine) => '╏'
    case (HeavyTripleDash, NoLine, HeavyTripleDash, NoLine) => '┇'
    case (HeavyQuadDash, NoLine, HeavyQuadDash, NoLine)     => '┋'

    // Light
    case (NoLine, Light, NoLine, Light)                   => '─'
    case (Light, NoLine, Light, NoLine)                   => '│'
    case (AnyLight(), NoLine, NoLine, NoLine)             => '╵'
    case (NoLine, AnyLight(), NoLine, NoLine)             => '╶'
    case (NoLine, NoLine, AnyLight(), NoLine)             => '╷'
    case (NoLine, NoLine, NoLine, AnyLight())             => '╴'
    case (NoLine, AnyLight(), AnyLight(), AnyLight())     => '┬'
    case (AnyLight(), NoLine, AnyLight(), AnyLight())     => '┤'
    case (AnyLight(), AnyLight(), NoLine, AnyLight())     => '┴'
    case (AnyLight(), AnyLight(), AnyLight(), NoLine)     => '├'
    case (AnyLight(), AnyLight(), AnyLight(), AnyLight()) => '┼'
    case (NoLine, AnyLight(), AnyLight(), NoLine)         => '┌'
    case (NoLine, NoLine, AnyLight(), AnyLight())         => '┐'
    case (AnyLight(), NoLine, NoLine, AnyLight())         => '┘'
    case (AnyLight(), AnyLight(), NoLine, NoLine)         => '└'

    // Heavy
    case (Heavy, NoLine, Heavy, NoLine)                   => '┃'
    case (AnyHeavy(), NoLine, NoLine, NoLine)             => '╹'
    case (NoLine, AnyHeavy(), NoLine, NoLine)             => '╺'
    case (NoLine, NoLine, AnyHeavy(), NoLine)             => '╻'
    case (NoLine, NoLine, NoLine, AnyHeavy())             => '╸'
    case (AnyHeavy(), AnyHeavy(), NoLine, AnyHeavy())     => '┻'
    case (AnyHeavy(), AnyHeavy(), AnyHeavy(), NoLine)     => '┣'
    case (NoLine, AnyHeavy(), AnyHeavy(), AnyHeavy())     => '┳'
    case (AnyHeavy(), NoLine, AnyHeavy(), AnyHeavy())     => '┫'
    case (AnyHeavy(), AnyHeavy(), AnyHeavy(), AnyHeavy()) => '╋'
    case (NoLine, AnyHeavy(), AnyHeavy(), NoLine)         => '┏'
    case (NoLine, NoLine, AnyHeavy(), AnyHeavy())         => '┓'
    case (AnyHeavy(), NoLine, NoLine, AnyHeavy())         => '┛'
    case (AnyHeavy(), AnyHeavy(), NoLine, NoLine)         => '┗'

    // Doubled
    case (Doubled, NoLine, Doubled, NoLine)   => '║'
    case (NoLine, Doubled, NoLine, Doubled)   => '═'
    case (Doubled, Doubled, NoLine, Doubled)  => '╩'
    case (Doubled, Doubled, Doubled, NoLine)  => '╠'
    case (NoLine, Doubled, Doubled, Doubled)  => '╦'
    case (Doubled, NoLine, Doubled, Doubled)  => '╣'
    case (Doubled, Doubled, Doubled, Doubled) => '╬'
    case (NoLine, Doubled, Doubled, NoLine)   => '╔'
    case (NoLine, NoLine, Doubled, Doubled)   => '╗'
    case (Doubled, NoLine, NoLine, Doubled)   => '╝'
    case (Doubled, Doubled, NoLine, NoLine)   => '╚'

    // Light to Double
    case (AnyLight(), Doubled, AnyLight(), Doubled) => '╪'
    case (Doubled, AnyLight(), Doubled, AnyLight()) => '╫'
    case (NoLine, AnyLight(), Doubled, NoLine)      => '╓'
    case (NoLine, AnyLight(), Doubled, AnyLight())  => '╥'
    case (NoLine, NoLine, Doubled, AnyLight())      => '╖'
    case (Doubled, AnyLight(), Doubled, NoLine)     => '╟'
    case (Doubled, NoLine, Doubled, AnyLight())     => '╢'
    case (Doubled, AnyLight(), NoLine, NoLine)      => '╙'
    case (Doubled, AnyLight(), NoLine, AnyLight())  => '╨'
    case (Doubled, NoLine, NoLine, AnyLight())      => '╜'
    case (NoLine, Doubled, AnyLight(), NoLine)      => '╒'
    case (NoLine, Doubled, AnyLight(), Doubled)     => '╤'
    case (NoLine, NoLine, AnyLight(), Doubled)      => '╕'
    case (AnyLight(), Doubled, AnyLight(), NoLine)  => '╞'
    case (AnyLight(), NoLine, AnyLight(), Doubled)  => '╡'
    case (AnyLight(), Doubled, NoLine, NoLine)      => '╘'
    case (AnyLight(), Doubled, NoLine, Doubled)     => '╧'
    case (AnyLight(), NoLine, NoLine, Doubled)      => '╛'

    // Light to Heavy
    case (NoLine, AnyHeavy(), NoLine, AnyLight())         => '╼'
    case (NoLine, AnyLight(), NoLine, AnyHeavy())         => '╾'
    case (AnyLight(), NoLine, AnyHeavy(), NoLine)         => '╽'
    case (AnyHeavy(), NoLine, AnyLight(), NoLine)         => '╿'
    case (NoLine, AnyLight(), AnyHeavy(), NoLine)         => '┎'
    case (NoLine, NoLine, AnyHeavy(), AnyLight())         => '┒'
    case (AnyHeavy(), NoLine, NoLine, AnyLight())         => '┚'
    case (AnyHeavy(), AnyLight(), NoLine, NoLine)         => '┖'
    case (NoLine, AnyLight(), AnyHeavy(), AnyLight())     => '┰'
    case (AnyHeavy(), AnyLight(), AnyHeavy(), NoLine)     => '┠'
    case (AnyHeavy(), NoLine, AnyHeavy(), AnyLight())     => '┨'
    case (AnyHeavy(), AnyLight(), NoLine, AnyLight())     => '┸'
    case (AnyLight(), AnyHeavy(), AnyLight(), AnyHeavy()) => '┿'
    case (AnyHeavy(), AnyLight(), AnyHeavy(), AnyLight()) => '╂'
    case (AnyHeavy(), AnyLight(), AnyLight(), AnyLight()) => '╀'
    case (AnyLight(), AnyHeavy(), AnyLight(), AnyLight()) => '┾'
    case (AnyLight(), AnyLight(), AnyHeavy(), AnyLight()) => '╁'
    case (AnyLight(), AnyLight(), AnyLight(), AnyHeavy()) => '┽'
    case (AnyHeavy(), AnyHeavy(), AnyLight(), AnyLight()) => '╄'
    case (AnyLight(), AnyHeavy(), AnyHeavy(), AnyLight()) => '╆'
    case (AnyLight(), AnyLight(), AnyHeavy(), AnyHeavy()) => '╅'
    case (AnyHeavy(), AnyLight(), AnyLight(), AnyHeavy()) => '╃'
    case (AnyLight(), AnyHeavy(), AnyHeavy(), AnyHeavy()) => '╈'
    case (AnyHeavy(), AnyLight(), AnyHeavy(), AnyHeavy()) => '╉'
    case (AnyHeavy(), AnyHeavy(), AnyLight(), AnyHeavy()) => '╇'
    case (AnyHeavy(), AnyHeavy(), AnyHeavy(), AnyLight()) => '╊'
    case (NoLine, AnyHeavy(), AnyLight(), AnyLight())     => '┮'
    case (NoLine, AnyLight(), AnyHeavy(), AnyHeavy())     => '┱'
    case (NoLine, AnyHeavy(), AnyHeavy(), AnyLight())     => '┲'
    case (NoLine, AnyLight(), AnyLight(), AnyHeavy())     => '┭'
    case (AnyHeavy(), NoLine, AnyLight(), AnyHeavy())     => '┩'
    case (AnyLight(), NoLine, AnyHeavy(), AnyLight())     => '┧'
    case (AnyHeavy(), NoLine, AnyLight(), AnyLight())     => '┦'
    case (AnyLight(), NoLine, AnyHeavy(), AnyHeavy())     => '┪'
    case (AnyLight(), AnyLight(), NoLine, AnyHeavy())     => '┵'
    case (AnyHeavy(), AnyHeavy(), NoLine, AnyLight())     => '┺'
    case (AnyHeavy(), AnyLight(), NoLine, AnyHeavy())     => '┹'
    case (AnyLight(), AnyHeavy(), NoLine, AnyLight())     => '┶'
    case (AnyHeavy(), AnyHeavy(), AnyLight(), NoLine)     => '┡'
    case (AnyLight(), AnyLight(), AnyHeavy(), NoLine)     => '┟'
    case (AnyHeavy(), AnyLight(), AnyLight(), NoLine)     => '┞'
    case (AnyLight(), AnyHeavy(), AnyHeavy(), NoLine)     => '┢'
    case (NoLine, AnyHeavy(), AnyLight(), NoLine)         => '┍'
    case (NoLine, AnyHeavy(), AnyLight(), AnyHeavy())     => '┯'
    case (NoLine, NoLine, AnyLight(), AnyHeavy())         => '┑'
    case (AnyLight(), AnyHeavy(), AnyLight(), NoLine)     => '┝'
    case (AnyLight(), NoLine, AnyLight(), AnyHeavy())     => '┥'
    case (AnyLight(), AnyHeavy(), NoLine, NoLine)         => '┕'
    case (AnyLight(), AnyHeavy(), NoLine, AnyHeavy())     => '┷'
    case (AnyLight(), NoLine, NoLine, AnyHeavy())         => '┙'

    case _ => '●'

  }

}
