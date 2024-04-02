package incremental

import scala.util.matching.Regex

class StringTemplate(str: String) {
  def substitute(subs: (String, String)*): String =
    subs.foldLeft(str) { case (acc, (seek, sub)) =>
      acc.replaceAll(Regex.quote(seek), Regex.quoteReplacement(sub))
    }
}
