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
      "scripts.RecordCorpusFixture", "scripts.BuildHardClusters", "scripts.FillMissingFixtures", "scripts.UnmatchedClustersRatchet")
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

  "the country benches and audits" should "measure the country named, their default when none is, and stop on a code no country has" in {
    // each read args.headOption.flatMap(Country.byCode): a mistyped `gb` ran on the US (or Germany) instead
    CorpusCensusBench.runOf(parsed(CorpusCensusBench.commandLine, "uk", "3")) shouldBe CorpusCensusBench.Run(models.Country.UnitedKingdom, 3)
    CorpusCensusBench.runOf(parsed(CorpusCensusBench.commandLine)) shouldBe CorpusCensusBench.Run(models.Country.UnitedStates, 5)
    an[CommandLine.InvalidArguments] should be thrownBy CorpusCensusBench.runOf(parsed(CorpusCensusBench.commandLine, "gb"))
    an[CommandLine.InvalidArguments] should be thrownBy CorpusCensusBench.runOf(parsed(CorpusCensusBench.commandLine, "us", "five"))
    GenreLanguageAudit.runOf(parsed(GenreLanguageAudit.commandLine, "pl", "Lalka")) shouldBe GenreLanguageAudit.Run(models.Country.Poland, Some("Lalka"))
    GenreLanguageAudit.runOf(parsed(GenreLanguageAudit.commandLine)) shouldBe GenreLanguageAudit.Run(models.Country.Germany, None)
    an[CommandLine.InvalidArguments] should be thrownBy GenreLanguageAudit.runOf(parsed(GenreLanguageAudit.commandLine, "deu"))
    IdentityListingsBench.runOf(parsed(IdentityListingsBench.commandLine, "bench_db", "es")) shouldBe
      IdentityListingsBench.Run("bench_db", models.Country.Spain, passes = 6, churn = 70)
    an[CommandLine.InvalidArguments] should be thrownBy IdentityListingsBench.runOf(parsed(IdentityListingsBench.commandLine, "bench_db", "sp"))
    an[CommandLine.InvalidArguments] should be thrownBy IdentityListingsBench.runOf(parsed(IdentityListingsBench.commandLine))
  }

  "FilmIdCounterSeed" should "seed the countries named, every country when none is, and refuse a flag it does not know" in {
    FilmIdCounterSeed.runOf(parsed(FilmIdCounterSeed.commandLine, "--apply", "es")) shouldBe FilmIdCounterSeed.Run(apply = true, Seq(models.Country.Spain))
    FilmIdCounterSeed.runOf(parsed(FilmIdCounterSeed.commandLine)) shouldBe FilmIdCounterSeed.Run(apply = false, models.Country.all)
    // args.filterNot(_.startsWith("--")) dropped a mistyped --aply: the run went ahead, every country, dry
    refused(FilmIdCounterSeed.commandLine, "--aply", "es") shouldBe true
    an[CommandLine.InvalidArguments] should be thrownBy FilmIdCounterSeed.runOf(parsed(FilmIdCounterSeed.commandLine, "es", "gb"))
  }

  "IdentityConjunctionProbe" should "write the contenders where named, measure allocation over --alloc runs, and refuse a count that is none" in {
    IdentityConjunctionProbe.outOf(parsed(IdentityConjunctionProbe.commandLine)) shouldBe Paths.get("target/probe/contenders.tsv")
    IdentityConjunctionProbe.outOf(parsed(IdentityConjunctionProbe.commandLine, "p.tsv")) shouldBe Paths.get("p.tsv")
    parsed(IdentityConjunctionProbe.commandLine, "--alloc", "5").int("alloc") shouldBe Some(5)
    // `--alloc five` died on NumberFormatException; `p.tsv extra` wrote p.tsv and ignored the rest
    an[CommandLine.InvalidArguments] should be thrownBy parsed(IdentityConjunctionProbe.commandLine, "--alloc", "five").int("alloc")
    refused(IdentityConjunctionProbe.commandLine, "p.tsv", "extra") shouldBe true
  }

  "IdentityMlScorerBench" should "require its model, take the expected scores beside it, and nothing more" in {
    val arguments = parsed(IdentityMlScorerBench.commandLine, "target/identity-ml/gbm-d2.json", "target/identity-ml/scores-d2.txt")
    (arguments.requiredPositional("model"), arguments.positional("expected")) shouldBe ("target/identity-ml/gbm-d2.json", Some("target/identity-ml/scores-d2.txt"))
    // args(0) threw ArrayIndexOutOfBounds with no model named
    an[CommandLine.InvalidArguments] should be thrownBy parsed(IdentityMlScorerBench.commandLine).requiredPositional("model")
    refused(IdentityMlScorerBench.commandLine, "m.json", "s.txt", "extra") shouldBe true
  }

  "RecordUsChains" should "pin the day named, and refuse one that is no date or a second argument" in {
    import clients.tools.RecordUsChains
    RecordUsChains.dayOf(parsed(RecordUsChains.commandLine, "2026-10-12")) shouldBe Some(java.time.LocalDate.of(2026, 10, 12))
    RecordUsChains.dayOf(parsed(RecordUsChains.commandLine)) shouldBe None
    an[CommandLine.InvalidArguments] should be thrownBy RecordUsChains.dayOf(parsed(RecordUsChains.commandLine, "12.10.2026"))
    refused(RecordUsChains.commandLine, "2026-10-12", "extra") shouldBe true
  }

  "FillMissingFixtures" should "read the convergence fill's six arguments, and refuse a seventh, a bad code or a deadline that is no number" in {
    val run = FillMissingFixtures.runOf(parsed(FillMissingFixtures.commandLine, "de", "refetch.tsv", "held", "out", "1760000000", "refused.tsv"))
    run shouldBe FillMissingFixtures.Run(models.Country.Germany, Paths.get("refetch.tsv"), Paths.get("held"), Paths.get("out"), 1760000000L,
      Some(Paths.get("refused.tsv")))
    def refusedRun(args: String*) = an[CommandLine.InvalidArguments] should be thrownBy FillMissingFixtures.runOf(parsed(FillMissingFixtures.commandLine, args*))
    // `rest*` ignored a seventh argument; `until.toLong` died on a NumberFormatException
    refused(FillMissingFixtures.commandLine, "de", "l", "h", "o", "1", "r", "extra") shouldBe true
    refusedRun("de", "l", "h", "o", "tomorrow")
    refusedRun("ger", "l", "h", "o", "1")
    refusedRun("de", "l", "h", "o")
  }

  "every program under worker's test tree" should "have a main the JVM can start" in {
    // `def main(args)(using LatestTitleYear)` compiles to main(String[], LatestTitleYear): `runMain` finds no entry point
    val Package = """(?m)^package ([\w.]+)""".r
    val Object  = """(?m)^object (\w+)""".r
    val programs = tools.ScalaSourceScan.scalaFiles(Seq("worker/src/test")).filter(tools.ScalaSourceScan.isProgram)
      .filter(path => """"(?:[^"\\\n]|\\.)*"""".r.replaceAllIn(tools.ScalaSourceScan.codeOf(path), "").contains("def main("))
    programs.size should be > 20
    val unstartable = programs.filterNot { path =>
      val source = tools.ScalaSourceScan.read(path)
      val prefix = Package.findFirstMatchIn(source).fold("")(_.group(1) + ".")
      Object.findAllMatchIn(source).exists(o => scala.util.Try(Class.forName(prefix + o.group(1)).getMethod("main", classOf[Array[String]])).isSuccess)
    }
    unstartable shouldBe empty
  }

  "ReapOrphanedFilmRows" should "read its cap" in {
    parsed(ReapOrphanedFilmRows.commandLine).requiredInt("max") shouldBe 5000
    parsed(ReapOrphanedFilmRows.commandLine, "--apply", "--max", "10").requiredInt("max") shouldBe 10
  }

  "UnmatchedClustersRatchet" should "read the checked-in captures and judgements by default, and the takes check's --measure" in {
    val defaults = parsed(UnmatchedClustersRatchet.commandLine)
    defaults.flag("measure") shouldBe false
    defaults.requiredPath("fixtures") shouldBe tools.UnmatchedClusters.Directory
    defaults.requiredPath("judgements") shouldBe tools.IdentityJudgements.CheckedIn
    defaults.requiredPath("out") shouldBe tools.UnmatchedClusters.FindingsPath.getParent
    parsed(UnmatchedClustersRatchet.commandLine, "--measure", "--out", "o").flag("measure") shouldBe true
    refused(UnmatchedClustersRatchet.commandLine, "--labels", "l.tsv") shouldBe true
  }
}
