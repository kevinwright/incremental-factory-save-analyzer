package gameanalyzer.wiki

import java.nio.file.{Files, Path}
import scala.util.{Try, Using}

case class WikiCredentials(
    basicUser: Option[String],
    basicPass: Option[String],
    wikiUser: String,
    wikiPass: String
)

object WikiCredentials {
  def load(path: Path): Try[WikiCredentials] =
    Using(
      io.Source.fromInputStream(
        Files.newInputStream(path)
      )
    ) { src =>
      val map = src
        .getLines()
        .toSeq
        .map(line => line.takeWhile(_ != '=') -> line.dropWhile(_ != '=').tail)
        .toMap

      WikiCredentials(
        map.get("basic-user"),
        map.get("basic-pass"),
        map("wiki-user"),
        map("wiki-pass")
      )
    }

}
