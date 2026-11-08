package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The case of [[SharedUsersMigrationSpec]] that runs the migration's `main` in a child JVM for its exit code. */
class SharedUsersMigrationIntegrationSpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  "The migration's command line" should "dry-run with no flag, write with --write, and exit 2 on a flag it does not know" in {
    SharedUsersMigration.commandLine.parse(Nil).toOption.get.flag("write") shouldBe false
    SharedUsersMigration.commandLine.parse(Seq("--write")).toOption.get.flag("write") shouldBe true
    // args.contains("--write") read a mistyped --wirte as no flag: the operator's write ran as a silent dry run
    val (code, output) = ChildJvm(harness).run("tools.SharedUsersMigration", Seq("--wirte"))
    code shouldBe 2
    output should include("unknown option --wirte")
  }
}
