package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class CommandLineSpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  import CommandLine.*

  private val line = CommandLine("Probe", "measures a probe",
    Flag("apply", "write what it finds"),
    Value("report", "file", "where the report goes"),
    Value("threads", "n", "workers", default = Some("6")),
    Value("relearn", "signal", "a signal relearned", repeatable = true))
    .withPositionals(Positional("country", "the country measured"))

  private def parsed(args: String*): Arguments = line.parse(args) match {
    case Right(arguments) => arguments
    case Left(stop)       => fail(s"stopped: $stop")
  }
  private def invalid(args: String*): String = line.parse(args) match {
    case Left(Invalid(message)) => message
    case other                  => fail(s"not refused: $other")
  }

  "a CommandLine" should "refuse a flag it does not declare, rather than drop it" in {
    invalid("--repport", "out.md") should include("--repport")
  }

  it should "refuse a valued option given no value, at the end or before another option" in {
    invalid("--report") should include("--report")
    invalid("--report", "--apply") should include("--report")
  }

  it should "read a bare boolean before a valued option without shifting the pairs after it" in {
    // args.grouped(2) read this as ("--apply","--report") and ("out.md","--threads"), losing both values
    val arguments = parsed("--apply", "--report", "out.md", "--threads", "3")
    arguments.flag("apply") shouldBe true
    arguments.get("report") shouldBe Some("out.md")
    arguments.int("threads") shouldBe Some(3)
  }

  it should "read --name=value as the option's value" in {
    parsed("--report=out.md").get("report") shouldBe Some("out.md")
  }

  it should "give an absent option its declared default, and an absent flag false" in {
    val arguments = parsed()
    arguments.get("threads") shouldBe Some("6")
    arguments.get("report") shouldBe None
    arguments.flag("apply") shouldBe false
  }

  it should "collect a repeatable option's every value in order, and refuse a single option given twice" in {
    parsed("--relearn", "year", "--relearn", "runtime").all("relearn") shouldBe Seq("year", "runtime")
    invalid("--report", "a.md", "--report", "b.md") should include("twice")
  }

  it should "read positionals around the options, and refuse one more than it declares" in {
    parsed("pl", "--apply").positionals shouldBe Seq("pl")
    parsed("--apply", "pl").positional("country") shouldBe Some("pl")
    invalid("pl", "de") should include("de")
  }

  it should "read everything after -- as positionals" in {
    parsed("--", "--apply").positionals shouldBe Seq("--apply")
  }

  it should "refuse a value its reader cannot convert, and a required option left out" in {
    an[InvalidArguments] should be thrownBy parsed("--threads", "many").int("threads")
    an[InvalidArguments] should be thrownBy parsed().required("report")
    parsed("--report", "r.md").path("report") shouldBe Some(Paths.get("r.md"))
    parsed("--report", "r.md").requiredPath("report") shouldBe Paths.get("r.md")
    parsed().requiredInt("threads") shouldBe 6
  }

  it should "treat a read of an option it never declared as the tool's own bug" in {
    an[IllegalArgumentException] should be thrownBy parsed().get("reprot")
    an[IllegalArgumentException] should be thrownBy parsed().flag("report")
  }

  it should "answer --help with help text built from what it declares" in {
    line.parse(Seq("--report", "x", "--help")) match {
      case Left(Help(text)) =>
        text should include("usage: Probe [options] [country]")
        text should include("measures a probe")
        text should include("--report <file>")
        text should include("(default: 6)")
        text should include("(repeatable)")
        text should include("--apply")
        text should include("country")
      case other => fail(s"no help: $other")
    }
  }

  it should "read a positional as a number or a country, and stop on one that is neither" in {
    val countries = CommandLine("Census", "counts").withPositionals(Positional("country", "measured"), Positional("ticks", "how many"))
    def read(args: String*) = countries.parse(args).toOption.get
    read("UK", "3").country("country") shouldBe Some(models.Country.UnitedKingdom)
    read("uk", "3").positionalInt("ticks") shouldBe Some(3)
    read().country("country") shouldBe None
    // args.headOption.flatMap(Country.byCode) read a mistyped code as none given, and measured the default country
    an[InvalidArguments] should be thrownBy read("gb").country("country")
    an[InvalidArguments] should be thrownBy read("uk", "many").positionalInt("ticks")
    an[InvalidArguments] should be thrownBy read().requiredPositional("ticks")
    val several = CommandLine("Seed", "seeds").withPositionals(Positional("countries", "seeded", repeatable = true))
    several.parse(Seq("es", "pl")).toOption.get.countries("countries") shouldBe Seq(models.Country.Spain, models.Country.Poland)
    several.parse(Nil).toOption.get.countries("countries") shouldBe empty
    an[InvalidArguments] should be thrownBy several.parse(Seq("es", "xx")).toOption.get.countries("countries")
  }

  "Commands" should "read each command's arguments by that command's own line, and refuse no command or an unknown one" in {
    val tool = Commands("Clusters", "builds clusters",
      CommandLine("select", "picks clusters").withPositionals(Positional("dir", "the corpora"), Positional("budget", "listings")),
      CommandLine("extend", "grows clusters", Flag("dry", "write nothing")).withPositionals(Positional("dir", "the corpora")))
    val (command, arguments) = tool.parse(Seq("select", "d", "40")).toOption.get
    command shouldBe "select"
    arguments.positionalInt("budget") shouldBe Some(40)
    tool.parse(Seq("extend", "--dry", "d")).toOption.get._2.flag("dry") shouldBe true
    tool.parse(Seq("select", "--dry", "d")) shouldBe Left(Invalid("unknown option --dry"))
    tool.parse(Seq("extend", "d", "e")).isLeft shouldBe true
    tool.parse(Nil) match { case Left(Invalid(why)) => why should include("select, extend"); case other => fail(s"$other") }
    tool.parse(Seq("prune")) match { case Left(Invalid(why)) => why should include("prune"); case other => fail(s"$other") }
    tool.accepts(Seq("extend", "d")) shouldBe Right(())
    tool.parse(Seq("--help")) match {
      case Left(Help(text)) => text should (include("usage: Clusters <command>") and include("select  picks clusters") and include("grows clusters"))
      case other            => fail(s"no help: $other")
    }
    tool.parse(Seq("extend", "--help")) match {
      case Left(Help(text)) => text should include("usage: Clusters extend [options] [dir]")
      case other            => fail(s"no help: $other")
    }
  }

  "parseOrExit" should "exit 2 with the usage on stderr for a bad command line, and 0 on --help" in {
    val jvm = ChildJvm(configuration)
    val (refused, refusal) = jvm.run("tools.CommandLineProbe", Seq("--bogus"))
    refused shouldBe 2
    refusal should include("unknown option --bogus")
    refusal should include("usage: CommandLineProbe")
    val (helped, help) = jvm.run("tools.CommandLineProbe", Seq("--help"))
    helped shouldBe 0
    help should include("--name <text>")
    jvm.run("tools.CommandLineProbe", Seq("--name", "x")) shouldBe (0, "hello x\n")
    val (unknown, why) = jvm.run("tools.CommandsProbe", Seq("wave"))
    unknown shouldBe 2
    why should include("unknown command wave")
    jvm.run("tools.CommandsProbe", Seq("greet", "--bogus"))._1 shouldBe 2
    jvm.run("tools.CommandsProbe", Seq("greet", "--name", "y")) shouldBe (0, "greet y\n")
  }
}

/** A tool with one option, run in a child JVM by [[CommandLineSpec]] for its exit codes. */
object CommandLineProbe {
  def main(args: Array[String]): Unit = {
    val arguments = CommandLine("CommandLineProbe", "greets", CommandLine.Value("name", "text", "who")).parseOrExit(args)
    println(s"hello ${arguments.required("name")}")
  }
}

/** A tool with one command, run in a child JVM by [[CommandLineSpec]] for its exit codes. */
object CommandsProbe {
  def main(args: Array[String]): Unit = {
    val (command, arguments) = CommandLine.Commands("CommandsProbe", "greets",
      CommandLine("greet", "greets", CommandLine.Value("name", "text", "who"))).parseOrExit(args)
    println(s"$command ${arguments.required("name")}")
  }
}
