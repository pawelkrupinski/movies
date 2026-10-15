package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ToolCallers

class IntegrationToolCommandLinesSpec extends AnyFlatSpec with Matchers {

  "every runMain of an integration tool in the scripts, workflows and skills" should "parse against the tool's command line" in {
    val checked = ToolCallers.calls("integration").flatMap(call => ToolCallers.commandLineOf(call.tool).map(call -> _))
    checked.map(_._1.tool) should contain("integration.IdentityUnifiedDataset")
    checked.foreach { case (call, line) =>
      withClue(s"${call.file}: ${call.tool} ${call.args.mkString(" ")}") { line.parse(call.args).isRight shouldBe true }
    }
  }

  "IdentityUnifiedDataset" should "default its threads and require its out" in {
    val arguments = IdentityUnifiedDataset.commandLine.parse(Seq("--out", "o")).toOption.get
    arguments.requiredInt("threads") shouldBe 6
    an[tools.CommandLine.InvalidArguments] should be thrownBy
      IdentityUnifiedDataset.commandLine.parse(Nil).toOption.get.requiredPath("out")
    ModelTakeCorrectionCost.commandLine.parse(Seq("--answer", "d")).isLeft shouldBe true
  }
}
