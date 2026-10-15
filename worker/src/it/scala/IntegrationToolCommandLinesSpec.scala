package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ToolCallers

class IntegrationToolCommandLinesSpec extends AnyFlatSpec with Matchers {

  "every runMain of an integration tool in the scripts, workflows and skills" should "parse against the tool's command line" in {
    val checked = ToolCallers.calls("integration").flatMap(call => ToolCallers.commandLineOf(call.tool).map(call -> _))
    checked.map(_._1.tool) should contain allOf ("integration.IdentityUnifiedDataset", "integration.IdentityResolveDump")
    checked.foreach { case (call, line) =>
      withClue(s"${call.file}: ${call.tool} ${call.args.mkString(" ")}") { line.accepts(call.args).isRight shouldBe true }
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

  "IdentityResolveDump" should "require its out, countries and corpus directory, the fixture root its own option" in {
    val arguments = IdentityResolveDump.commandLine.parse(Seq("--out", "o", "--full", "pl,uk", "--corpus-dir", "c")).toOption.get
    arguments.required("full") shouldBe "pl,uk"
    arguments.path("fixtures") shouldBe None
    an[tools.CommandLine.InvalidArguments] should be thrownBy IdentityResolveDump.commandLine.parse(Seq("--out", "o")).toOption.get.required("full")
    // KINOWO_IDENTITY_DUMP named the directory once; a flag left without its value is refused, never read as the next
    IdentityResolveDump.commandLine.parse(Seq("--out", "--full", "pl")).isLeft shouldBe true
  }

  "the unmatched-cluster capture and fill" should "read the command lines scripts.IdentityCapture builds for them" in {
    import scripts.IdentityCapture._
    val layout = Layout(java.nio.file.Path.of("/repo"), java.nio.file.Path.of("/repo/target/identity-capture"))
    val capture = UnmatchedClustersCapture.commandLine.parse(captureArguments("uk", Map.empty, layout)).toOption.get
    UnmatchedClustersCapture.countryOf(capture) shouldBe models.Country.all.find(_.code == "uk").get
    capture.requiredPath("out") shouldBe layout.fixtures
    capture.path("family-seed") shouldBe Some(layout.families)
    val fill = UnmatchedClustersFill.commandLine.parse(fillArguments("de", Map.empty, layout)).toOption.get
    fill.requiredPath("capture") shouldBe layout.fixtures
    an[tools.CommandLine.InvalidArguments] should be thrownBy
      UnmatchedClustersCapture.countryOf(UnmatchedClustersCapture.commandLine.parse(Seq("gb", "--out", "o")).toOption.get)
    UnmatchedClustersFill.commandLine.parse(Seq("de", "pl")).isLeft shouldBe true
  }
}
