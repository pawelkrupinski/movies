package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The case of [[CommandLineSpec]] that runs its `CommandLineProbe` and `CommandsProbe` in child JVMs for their exit codes. */
class CommandLineIntegrationSpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  "parseOrExit" should "exit 2 with the usage on stderr for a bad command line, and 0 on --help" in {
    val jvm = ChildJvm(harness)
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

/** A tool with one option, run in a child JVM by [[CommandLineIntegrationSpec]] for its exit codes. */
object CommandLineProbe {
  def main(args: Array[String]): Unit = {
    val arguments = CommandLine("CommandLineProbe", "greets", CommandLine.Value("name", "text", "who")).parseOrExit(args)
    println(s"hello ${arguments.required("name")}")
  }
}

/** A tool with one command, run in a child JVM by [[CommandLineIntegrationSpec]] for its exit codes. */
object CommandsProbe {
  def main(args: Array[String]): Unit = {
    val (command, arguments) = CommandLine.Commands("CommandsProbe", "greets",
      CommandLine("greet", "greets", CommandLine.Value("name", "text", "who"))).parseOrExit(args)
    println(s"$command ${arguments.required("name")}")
  }
}
