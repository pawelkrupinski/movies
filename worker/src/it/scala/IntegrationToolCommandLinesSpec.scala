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

  "the decoration and take-answer tools" should "read their options and defaults, and refuse a flag left without its value" in {
    val candidates = IdentityDecorationCandidates.commandLine.parse(Seq("--out", "o", "--withhold", "3")).toOption.get
    candidates.requiredInt("rounds") shouldBe 4
    candidates.requiredInt("withhold") shouldBe 3
    candidates.required("version") shouldBe "unversioned"
    // grouped(2) read this as out = "--write" and dropped "a.json"
    IdentityDecorationCandidates.commandLine.parse(Seq("--out", "--write", "a.json")).isLeft shouldBe true
    val fill = ModelTakeAnswersFill.commandLine.parse(Seq("--out", "o", "--posters", "false")).toOption.get
    (fill.requiredInt("rounds"), fill.requiredInt("threads"), fill.get("posters"), fill.int("sample")) shouldBe (8, 6, Some("false"), None)
    SuspectTakesDump.commandLine.parse(Seq("--out", "o")).toOption.get.requiredPath("out") shouldBe java.nio.file.Paths.get("o")
    IdentityDecorationTokens.commandLine.parse(Seq("--outt", "o")).isLeft shouldBe true
  }
}
