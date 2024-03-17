package gameanalyzer.consoleui

enum LineStyle {
  case Light
  case LightArc
  case LightDoubleDash
  case LightTripleDash
  case LightQuadDash
  case Heavy
  case HeavyDoubleDash
  case HeavyTripleDash
  case HeavyQuadDash
  case Doubled
  case NoLine
}

object LineStyle {
  object AnyLight {
    def unapply(s: LineStyle): Boolean = s match {
      case Light | LightArc | LightDoubleDash | LightTripleDash |
          LightQuadDash =>
        true
      case _ => false
    }
  }

  object AnyHeavy {
    def unapply(s: LineStyle): Boolean = s match {
      case Heavy | HeavyDoubleDash | HeavyTripleDash | HeavyQuadDash => true
      case _                                                         => false
    }
  }
}
