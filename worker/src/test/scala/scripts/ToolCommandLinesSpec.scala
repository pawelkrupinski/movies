package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.CommandLine

import java.nio.file.Paths

class ToolCommandLinesSpec extends AnyFlatSpec with Matchers {

  private def parsed(line: CommandLine, args: String*): CommandLine.Arguments =
    line.parse(args).fold(stop => fail(s"${line.tool} ${args.mkString(" ")}: $stop"), identity)
  private def refused(line: CommandLine, args: String*): Boolean = line.parse(args).isLeft

  "every runMain of a scripts tool in the scripts, workflows and skills" should "parse against the tool's command line" in {
    val calls = ToolCallers.calls("scripts")
    calls.map(_.tool) should contain allOf ("scripts.IdentityRefit", "scripts.DecorationDiscovery", "scripts.IdentityUnifiedFit")
    val checked = calls.flatMap(call => ToolCallers.commandLineOf(call.tool).map(call -> _))
    checked.map(_._1.tool).toSet should contain allOf ("scripts.IdentityRefit", "scripts.DecorationDiscovery", "scripts.IdentityUnifiedFit",
      "scripts.ReapOrphanedFilmRows", "scripts.IdentityCalibrate", "scripts.IdentityDecorationsLearn",
      "scripts.RecordCorpusFixture")
    checked.foreach { case (call, line) =>
      withClue(s"${call.file}: ${call.tool} ${call.args.mkString(" ")}") { line.accepts(call.args).isRight shouldBe true }
    }
  }

  "IdentityCalibrate and IdentityDecorationsLearn" should "read the recorded corpora alike, the hard clusters defaulted" in {
    Seq(IdentityCalibrate.commandLine, IdentityDecorationsLearn.commandLine).foreach { line =>
      RecordedCorpora.of(parsed(line, "--corpora", "c", "--fixtures", "f")) shouldBe
        RecordedCorpora(Paths.get("c"), Paths.get("f"), Paths.get("test/resources/fixtures/corpus"))
      an[CommandLine.InvalidArguments] should be thrownBy RecordedCorpora.of(parsed(line, "--fixtures", "f"))
    }
  }

  "IdentityCalibrate" should "read --monotone-only with its artefact anywhere, and the epsilon as a number or certified" in {
    val monotone = parsed(IdentityCalibrate.commandLine, "--monotone-only", "w.json")
    monotone.flag("monotone-only") shouldBe true
    monotone.positional("artefact") shouldBe Some("w.json")
    parsed(IdentityCalibrate.commandLine, "--corpora", "c", "--epsilon", "0.01").double("epsilon") shouldBe Some(0.01)
    parsed(IdentityCalibrate.commandLine).get("epsilon") shouldBe Some("certified")
    parsed(IdentityDecorationsLearn.commandLine).requiredPath("out") shouldBe IdentityDecorationsLearn.Artefact
  }

  "IdentityRefit" should "read the weekly workflow's command line, a bare --apply first" in {
    val arguments = parsed(IdentityRefit.commandLine, "--apply", "--report", "r.md", "--relearn", "year.delta,runtime.delta")
    arguments.flag("apply") shouldBe true
    arguments.path("report") shouldBe Some(Paths.get("r.md"))
    arguments.get("relearn") shouldBe Some("year.delta,runtime.delta")
    arguments.requiredInt("max-changes") shouldBe 5
    arguments.requiredPath("weights") shouldBe IdentityRefit.Artefact
    refused(IdentityRefit.commandLine, "--relern", "year.delta") shouldBe true
  }

  "DecorationDiscovery" should "read its options and their defaults" in {
    val arguments = parsed(DecorationDiscovery.commandLine, "--apply", "--only", "prefix:kino-konesera", "--limit", "10")
    arguments.flag("apply") shouldBe true
    arguments.get("only") shouldBe Some("prefix:kino-konesera")
    arguments.requiredInt("limit") shouldBe 10
    parsed(DecorationDiscovery.commandLine).requiredInt("min-remainders") shouldBe services.identity.TitleDecorations.MinCandidateRemainders
  }

  "the training-row fits" should "refuse a valued option left without a value rather than read the next flag as it" in {
    // grouped(2) read this as report = "--version" and dropped "v1"
    refused(IdentityUnifiedFit.commandLine, "--report", "--version", "v1") shouldBe true
    val fit = parsed(IdentityUnifiedFit.commandLine, "--version", "v1")
    fit.requiredPath("training") shouldBe IdentityUnifiedFit.Training
    fit.get("version") shouldBe Some("v1")
    parsed(IdentityDecorationScoreFit.commandLine).requiredPath("out") shouldBe IdentityDecorationScoreFit.Artefact
    parsed(IdentityUnifiedStrategies.commandLine).requiredPath("rules") shouldBe IdentityUnifiedStrategies.RulesArtefact
    parsed(IdentityUnifiedExperiments.commandLine, "--report", "e.md").path("report") shouldBe Some(Paths.get("e.md"))
  }

  "SuspectTakesMeasure" should "read its dumps, year and outputs" in {
    val arguments = parsed(SuspectTakesMeasure.commandLine, "--why", "Lalka~Michael", "--out", "q.tsv")
    arguments.requiredPath("dumps") shouldBe Paths.get("target/suspect-takes")
    arguments.requiredInt("year") shouldBe 2026
    arguments.get("why") shouldBe Some("Lalka~Michael")
    an[CommandLine.InvalidArguments] should be thrownBy parsed(SuspectTakesMeasure.commandLine, "--year", "next").requiredInt("year")
  }

  "IdentityEvidenceClasses" should "require its fixtures" in {
    an[CommandLine.InvalidArguments] should be thrownBy parsed(IdentityEvidenceClasses.commandLine).requiredPath("fixtures")
    parsed(IdentityEvidenceClasses.commandLine, "--fixtures", "f").requiredPath("weights") shouldBe IdentityCalibrate.ResolverArtefact
  }

  "IdentityGateImpact" should "read its row exports as positionals around the options" in {
    val arguments = parsed(IdentityGateImpact.commandLine, "rows-pl.jsonl", "--trees", "t", "rows-uk.jsonl")
    arguments.positionals shouldBe Seq("rows-pl.jsonl", "rows-uk.jsonl")
    arguments.path("trees") shouldBe Some(Paths.get("t"))
  }

  "RecordCorpusFixture" should "record the country named, the configured one when none is, and stop on a code no country has" in {
    def country(args: String*) = parsed(RecordCorpusFixture.commandLine, args*).country("country")
    country("uk").map(_.code) shouldBe Some("uk")
    country() shouldBe None
    // args.headOption read a mistyped code as no code, and recorded the configured country under it
    an[CommandLine.InvalidArguments] should be thrownBy country("gb")
    refused(RecordCorpusFixture.commandLine, "pl", "uk") shouldBe true
  }

  "ReapOrphanedFilmRows" should "read its cap" in {
    parsed(ReapOrphanedFilmRows.commandLine).requiredInt("max") shouldBe 5000
    parsed(ReapOrphanedFilmRows.commandLine, "--apply", "--max", "10").requiredInt("max") shouldBe 10
  }
}
