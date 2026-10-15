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
  }
}

/** A tool with one option, run in a child JVM by [[CommandLineSpec]] for its exit codes. */
object CommandLineProbe {
  def main(args: Array[String]): Unit = {
    val arguments = CommandLine("CommandLineProbe", "greets", CommandLine.Value("name", "text", "who")).parseOrExit(args)
    println(s"hello ${arguments.required("name")}")
  }
}
