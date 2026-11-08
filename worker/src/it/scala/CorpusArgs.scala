package integration

import models.Country
import tools.{CommandLine, IntegrationMongoTarget}

import java.nio.file.Path

/** What a tool over recorded full corpora reads ([[IdentityResolveDump]], [[LeanWholeGuard]]): the `--full`
 *  countries, `--out`, `--corpus-dir` and `--fixtures` it was given, the process's configuration, the throwaway
 *  Mongo `MONGODB_URI` names (refused unless it is one), and the corpora recorded for those countries. */
final case class CorpusArgs(countries: Seq[Country], out: Path, corpusDir: Path, configuration: settings.ProcessConfiguration,
                            root: settings.FixtureRoot, target: IntegrationMongoTarget, corpora: Seq[IdentityShadow.Corpus]) {
  /** The countries named whose corpus `--corpus-dir` does not hold. */
  def missing: Seq[Country] = countries.filterNot(c => corpora.exists(_.country == c))
}

object CorpusArgs {
  /** The options every corpus tool declares; `--full` says what the tool does with the corpora. */
  def options(fullHelp: String): Seq[CommandLine.Declared] = Seq(
    CommandLine.Value("full", "codes", s"$fullHelp, comma-separated (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"))

  /** `arguments` read; a country `--corpus-dir` holds no corpus of is named on stderr under `tool`. */
  def of(arguments: CommandLine.Arguments, tool: String): CorpusArgs = {
    val countries = arguments.required("full").split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"--full: no country $code (one of ${Country.all.map(_.code).mkString(", ")})")))
    val out           = arguments.requiredPath("out")
    val corpusDir     = arguments.requiredPath("corpus-dir")
    val configuration = settings.ProcessConfiguration.resolve()
    val harness       = new settings.HarnessConfiguration(configuration.env)
    val root          = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(harness.fixtureRoot)
    val target        = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo to seed the corpora into"))
    target.requireThrowaway()
    val read = CorpusArgs(countries, out, corpusDir, configuration, root, target,
      IdentityShadow.full(countries.toSet, corpusDir, root, harness.identityLiveGaps))
    read.missing.foreach(c => System.err.println(s"[$tool] no cinema-scrapes-${c.code}.json.gz in $corpusDir"))
    read
  }
}
