package gameanalyzer

import com.monovore.decline.*
import cats.implicits.*

import java.nio.file.Path

object CommandLineParser {
  case class SummaryFlags(
      buildings: Boolean,
      resources: Boolean,
      skills: Boolean
  )

  object SummaryFlags {
    def parse(s: String): SummaryFlags = {
      val parts = s.split(",").toSet
      val all = parts("a") || parts("all")
      SummaryFlags(
        buildings = all || parts("b") || parts("buildings"),
        resources = all || parts("r") || parts("resources"),
        skills = all || parts("s") || parts("skills")
      )
    }

    val opt: Opts[SummaryFlags] = Opts
      .option(
        long = "summarise",
        short = "s",
        metavar = "b|buldings|r|resources|s|skills|a|all",
        help = "Comma-Separated list of things to summarise"
      )
      .withDefault("")
      .map(parse)
  }

  case class Args(
      summaryFlags: SummaryFlags,
      customCredentialsPath: Option[Path],
      blueprint: Option[String],
      saveFile: String
  ) {
    def credentialsPath: Path = customCredentialsPath.getOrElse(
      Path.of(
        System.getProperty("user.home"),
        "incremental-factory-wiki.credentials"
      )
    )
  }

  private val credentialsFileOpt: Opts[Option[Path]] = Opts
    .option[Path](
      long = "credentials",
      metavar = "string",
      help = "A blueprint to analyze."
    )
    .orNone

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

  private val command: Command[Args] = Command(
    name = "game-analyser",
    header = "Print the last few lines of one or more files."
  ) {
    (
      SummaryFlags.opt,
      credentialsFileOpt,
      blueprintOpt,
      saveFileOpt
    ).mapN(Args.apply)
  }

  def process[T](rawArgs: Array[String])(runFn: Args => T): T =
    command.parse(rawArgs, sys.env) match {
      case Left(help) if help.errors.isEmpty =>
        println(help)
        sys.exit(0)

      case Left(help) =>
        System.err.println(help)
        sys.exit(1)

      case Right(parsedArgs) =>
        runFn(parsedArgs)
    }
}
