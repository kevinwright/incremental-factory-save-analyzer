package incremental.consoleui

enum Justification {
  case Left, Centred, Right
}

import Justification.*

extension (j: Justification)
  private def spaces(n: Int): fansi.Str = fansi.Str(" ".repeat(n))

  def justify(
      text: fansi.Str,
      width: Int,
      padding: Int = 1
  ): fansi.Str = {
    val space: Int = width - text.length

    val (prefix, suffix) = j match {
      case Justification.Left  => (padding, space + padding)
      case Justification.Right => (space + padding, padding)
      case Justification.Centred =>
        val pre = space / 2
        val post = space - pre
        (pre + padding, post + padding)
    }

    val prefixStr = spaces(prefix max 0)
    val suffixStr = spaces(suffix max 0)
    prefixStr ++ text ++ suffixStr
  }

object Justification {
  type determinant = (Any, Any) => Justification

  def defaultDeterminant(context: Any, value: Any): Justification =
    value match {
      case x: Int    => Justification.Right
      case x: Float  => Justification.Right
      case x: Double => Justification.Right
      case x         => Justification.Left
    }
}
