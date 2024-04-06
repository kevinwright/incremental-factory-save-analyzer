package incremental.layout


sealed trait Content {
  def format(formatFn: [T] => T => String): String
}
sealed trait SimpleContent extends Content {
  def format(formatFn: [T] => T => String): String = stringValue
  def stringValue: String
}

case class TextContent(text: CharSequence) extends SimpleContent {
  def stringValue: String = text.toString
}

case class NumberContent(number: Int | Float | Double) extends SimpleContent {
  def stringValue: String = number.toString
}

case class ComplexContent(elems: Seq[Content]) extends Content {
  def format(formatFn: [T] => T => String): String = stringValue
  def stringValue: String = elems.mkString(" ")
}

case class CustomContent[C](contained: C) extends Content {
  def format(formatFn: [T] => T => String): String = formatFn[C](contained)
}

given Conversion[None.type, Content] with
  def apply(dummy: None.type): Content = TextContent("")

given [T](using conv: Conversion[T, Content]): Conversion[Option[T], Content]
  with
  def apply(x: Option[T]): Content = x match {
    case Some(value) => conv.convert(value)
    case None        => TextContent("")
  }

given Conversion[String, TextContent] with
  def apply(x: String): TextContent = TextContent(x)

given Conversion[Int, NumberContent] with
  def apply(x: Int): NumberContent = NumberContent(x)

given Conversion[Double, NumberContent] with
  def apply(x: Double): NumberContent = NumberContent(x)
