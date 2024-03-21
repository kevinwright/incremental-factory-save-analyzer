package gameanalyzer

import gameanalyzer.model.GameStateRoot
import scodec.bits.BitVector

import scala.util.{Try, Using}
import com.github.plokhotnyuk.jsoniter_scala.core.*

import java.io.InputStream
import java.nio.file.{Files, Path}
import scala.io.BufferedSource
import model.ModelCodecs.given

object SaveGameLoader {
  def load(pathStr: String): Try[GameStateRoot] =
    Using(sourceForPath(pathStr)) { src =>
      val text = src.mkString
      if text.trim.startsWith("{") then {
        readFromString[GameStateRoot](text)
      } else {
        BitVector.fromBase64Descriptive(text) match {
          case Left(err) => sys.error(err)
          case Right(vec) =>
            readFromByteBuffer[GameStateRoot](vec.toByteBuffer)
        }
      }
    }

  def sourceForPath(pathStr: String): BufferedSource =
    io.Source.fromInputStream(
      inputStreamForPath(pathStr: String)
    )

  def inputStreamForPath(pathStr: String): InputStream =
    if pathStr.startsWith("classpath:")
    then getClass.getResourceAsStream(pathStr.drop(10))
    else Files.newInputStream(Path.of(pathStr))

}
