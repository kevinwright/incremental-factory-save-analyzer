package incremental.commands

import cats.data.{Validated, ValidatedNel}
import com.monovore.decline.*
import cats.implicits.*

object AnalyseSaveFile {
  case class ParsedArgs(
      intentions: Set[Intent],
      blueprint: Option[String],
      saveFile: String
  ) {
    def includeAll = intentions.contains(Intent.all)
    def includeBuildings = includeAll || intentions.contains(Intent.buildings)
    def includeResources = includeAll || intentions.contains(Intent.resources)
    def includeSkills = includeAll || intentions.contains(Intent.skills)
  }

  enum Intent {
    case buildings, resources, skills, all
  }

  given Argument[Intent] with {
    def read(string: String): ValidatedNel[String, Intent] = {
      Intent.values.find(sso =>
        string == sso.toString || string == sso.toString.head.toString
      ) match {
        case Some(flag) => Validated.valid(flag)
        case _ => Validated.invalidNel(s"Invalid analyse intention: $string")
      }
    }

    def defaultMetavar = Intent.values.mkString("|")
  }

  private val intentOpt = Opts
    .options[Intent](
      long = "intent",
      short = "i",
      help = "What to summarise"
    )
    .orEmpty
    .map(_.toSet)

  private val blueprintOpt: Opts[Option[String]] = Opts
    .option[String](
      long = "blueprint",
      metavar = "string",
      help = "A blueprint to analyze."
    )
    .orNone

  private val saveFileOpt: Opts[String] = Opts
    .argument[String](
      metavar = "filename"
    )

  val command: Command[ParsedArgs] = Command(
    name = "analyse-save-file",
    header = "Analyse the factory in the provided save file."
  ) {
    (
      intentOpt,
      blueprintOpt,
      saveFileOpt
    ).mapN(ParsedArgs.apply)
  }

  val subcommand: Opts[ParsedArgs] = Opts.subcommand(command)
}
