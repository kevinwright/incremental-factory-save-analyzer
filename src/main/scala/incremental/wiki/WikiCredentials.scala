package incremental.wiki

import cats.effect.{IO, Resource}

import java.nio.file.{Files, Path}
import scala.util.{Try, Using}
import scala.io.Source

case class WikiCredentials(
    basicUser: Option[String],
    basicPass: Option[String],
    wikiUser: String,
    wikiPass: String,
    urlBase: String,
    restPath: String,
    apiPath: String
)

object WikiCredentials {
  def load(path: Path): IO[WikiCredentials] =
    Resource
      .fromAutoCloseable(
        IO(
          Source.fromInputStream(
            Files.newInputStream(path)
          )
        )
      )
      .use { src =>
        val map = src
          .getLines()
          .toSeq
          .map(line =>
            line.takeWhile(_ != '=') -> line.dropWhile(_ != '=').tail
          )
          .toMap

        IO.pure(
          WikiCredentials(
            map.get("basic-user"),
            map.get("basic-pass"),
            map("wiki-user"),
            map("wiki-pass"),
            map.getOrElse("url-base", "https://incrementalfactory.wiki.gg"),
            map.getOrElse("rest-path", "rest.php/v1"),
            map.getOrElse("api-path", "api.php")
          )
        )
      }

}
