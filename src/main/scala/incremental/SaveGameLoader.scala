package incremental

import incremental.model.GameStateRoot
import scodec.bits.BitVector

import scala.util.{Try, Using}
import com.github.plokhotnyuk.jsoniter_scala.core.*

import java.io.InputStream
import java.nio.file.{Files, Path}
import scala.io.{Source, BufferedSource}
import model.ModelCodecs.given
import cats.effect.{IO, Resource}
import java.io.{File, FileInputStream}

object SaveGameLoader {
  def load(pathStr: String): IO[GameStateRoot] =
    sourceForPath(pathStr).use { src =>
      val text = src.mkString
      if text.trim.startsWith("{") then {
        IO(readFromString[GameStateRoot](text))
      } else {
        BitVector.fromBase64Descriptive(text) match {
          case Left(err) => sys.error(err)
          case Right(vec) =>
            IO(readFromByteBuffer[GameStateRoot](vec.toByteBuffer))
        }
      }
    }

  def sourceForPath(pathStr: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable(
      IO(
        Source.fromInputStream(
          inputStreamForPath(pathStr: String)
        )
      )
    )

  def inputStreamForPath(pathStr: String): InputStream =
    if pathStr.startsWith("classpath:")
    then getClass.getResourceAsStream(pathStr.drop(10))
    else Files.newInputStream(Path.of(pathStr))

}
