package incremental.commands

import cats.data.{
  NonEmptyList,
  NonEmptySeq,
  NonEmptySet,
  Validated,
  ValidatedNel
}
import com.monovore.decline.*
import cats.implicits.*
import cats.kernel.Order

import java.nio.file.Path

object PublishWiki {
  enum Intent {
    case main, items, buildings, parceltypes, skills, research, all
  }

  given Order[Intent] = Order.by((_: Intent).ordinal)

  case class ParsedArgs(
      intentions: NonEmptySet[Intent],
      credentialsPath: Path
  ) {
    def publishAll = intentions.contains(Intent.all)
    def publishMain = publishAll || intentions.contains(Intent.main)
    def publishItems = publishAll || intentions.contains(Intent.items)
    def publishBuildings = publishAll || intentions.contains(Intent.buildings)
    def publishParcelTypes =
      publishAll || intentions.contains(Intent.parceltypes)
    def publishResearch =
      publishAll || intentions.contains(Intent.research)
    def publishSkills = publishAll || intentions.contains(Intent.skills)
  }

  given Argument[Intent] with {
    def read(string: String): ValidatedNel[String, Intent] = {
      Intent.values.find(sso =>
        string == sso.toString || string == sso.toString.head.toString
      ) match {
        case Some(flag) => Validated.valid(flag)
        case _ => Validated.invalidNel(s"Invalid wiki intention: $string")
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
    .withDefault(NonEmptyList.of(Intent.main))
    .map(_.toNes)

  private val credentialsFileOpt: Opts[Path] = Opts
    .option[Path](
      long = "credentials",
      metavar = "string",
      help = "Wiki credentials."
    )
    .withDefault(
      Path.of(
        System.getProperty("user.home"),
        "incremental-factory-wiki.credentials"
      )
    )

  val command: Command[ParsedArgs] = Command(
    name = "publish-wiki",
    header = "Publishes the model to the wiki."
  ) {
    (
      intentOpt,
      credentialsFileOpt
    ).mapN(ParsedArgs.apply)
  }

  val subcommand: Opts[ParsedArgs] = Opts.subcommand(command)

}
