package gameanalyzer.consoleui

object UiUtils {
  def stringify(value: Any): fansi.Str = value match {
    case s: fansi.Str => s
    case x            => fansi.Str(x.toString)
  }

  def textLength(value: Any): Int = stringify(value).length

  
}
