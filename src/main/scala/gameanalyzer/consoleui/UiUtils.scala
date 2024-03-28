package gameanalyzer.consoleui

object UiUtils {
  def stringify(value: Any): fansi.Str = value match {
    case s: fansi.Str => s
    case x            => fansi.Str(x.toString)
  }

  def textLength(value: Any): Int = stringify(value).length

  extension [A](it: IterableOnce[A])
    def mkFansiStr(
        start: fansi.Str,
        sep: fansi.Str,
        end: fansi.Str
    ): fansi.Str =
      if (it.knownSize == 0) start ++ end
      else {
        start ++ it.iterator.map(stringify).reduce(_ ++ sep ++ _) ++ end
      }

    @inline def mkFansiStr(sep: fansi.Str): fansi.Str =
      mkFansiStr(fansi.Str(""), sep, fansi.Str(""))

}
