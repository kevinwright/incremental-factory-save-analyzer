package incremental.layout

sealed trait Content:
  def format(customFormatFn: CustomContent => String = x => x.toString): String

sealed trait SimpleContent extends Content:
  def format(customFormatFn: CustomContent => String): String = stringValue
  def stringValue: String

case class TextContent(text: CharSequence) extends SimpleContent:
  def stringValue: String = text.toString

case class NumberContent(number: Byte | Int | Float | Double)
    extends SimpleContent:
  def stringValue: String = number.toString

case class ComplexContent(elems: Seq[Content]) extends Content:
  def format(customFormatFn: CustomContent => String): String = stringValue
  def stringValue: String = elems.mkString(" ")

trait CustomContent extends Content

case class WrappedCustomContent[C](contained: C) extends CustomContent:
  def format(customFormatFn: CustomContent => String): String =
    customFormatFn(this)

//////////////////////
// CONVERSIONS

trait ConvertsToContent[A]:
  def convert(a: A): Content
  extension (a: A) inline def toContent: Content = convert(a)

given [C <: Content]: ConvertsToContent[C] with
  inline def convert(x: C): Content = x

given ConvertsToContent[None.type] with
  inline def convert(dummy: None.type): Content = TextContent("")

given [A](using base: ConvertsToContent[A]): ConvertsToContent[Option[A]] with
  inline def convert(x: Option[A]): Content = x match {
    case Some(value) => base.convert(value)
    case None        => TextContent("")
  }

given ConvertsToContent[String] with
  inline def convert(x: String): Content = TextContent(x)

given ConvertsToContent[Byte] with
  inline def convert(x: Byte): Content = NumberContent(x)

given ConvertsToContent[Int] with
  inline def convert(x: Int): Content = NumberContent(x)

given ConvertsToContent[Double] with
  inline def convert(x: Double): Content = NumberContent(x)

given ConvertsToContent[Float] with
  inline def convert(x: Float): Content = NumberContent(x)
