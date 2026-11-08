package settings

import java.util.Locale

import models.Country
import tools.Env

import java.nio.file.Path

/**
 * The resolver of the configuration only a test, replay or measuring HARNESS reads: the
 * convergence legs' replay switches and corpora (`.github/workflows/country-convergence-leg.yml`
 * sets them, so every key keeps its name), the identity measuring tools' caches and corpora, the
 * page tests' browser, the share-card generator, the JVM a spec launches a child of. No deployed
 * process reads any of these, so they live beside the other test support rather than in
 * [[ProcessConfiguration]], which keeps only what a deployed `main` resolves.
 *
 * It reads through an [[Env]] the same way, and every value is a type of its own
 * (`HarnessValues`; `ProcessConfigurationSpec` checks both resolvers' sources). A suite gets one
 * from `tools.SuiteConfiguration` over the same Env as its `configuration`; a tool `main`
 * resolves one with [[HarnessConfiguration.resolve]]; a spec builds one over `Env.of(...)`.
 */
final class HarnessConfiguration(val env: Env) {

  private def text(key: String): Option[String] = env.get(key).map(_.trim).filter(_.nonEmpty)
  private def fact(key: String): Option[String] = env.currentValue(key).map(_.trim).filter(_.nonEmpty)
  private def count(key: String, default: Int): Int = env.positiveInt(key, default)

  // ── Fixtures and specs ──────────────────────────────────────────────────────
  /** `KINOWO_FIXTURE_DIR` — the fixture tree the recorder writes into. */
  def recordingFixtureDirectory: Option[RecordingFixtureDirectory] = text("KINOWO_FIXTURE_DIR").map(RecordingFixtureDirectory(_))

  /** `KINOWO_FIXTURE_ROOT`, else the repository's own tree. */
  def fixtureRoot: FixtureRoot = text("KINOWO_FIXTURE_ROOT").map(root => FixtureRoot(Path.of(root))).getOrElse(FixtureRoot.RepositoryRelative)

  /** `KINOWO_ALLOW_REMOTE_IT` (`1` or `true`). */
  def remoteIntegrationAllowed: RemoteIntegrationAllowed = RemoteIntegrationAllowed(env.flag("KINOWO_ALLOW_REMOTE_IT"))

  /** `KINOWO_SPEC_TIME_SCALE` — a whole-number multiplier on every test wait bound (`tools.SpecTimeouts`),
   *  for a runner slower than the defaults allow for; 1 when unset. */
  def specTimeScale: SpecTimeScale = SpecTimeScale(count("KINOWO_SPEC_TIME_SCALE", 1))

  /** `KINOWO_RACE_SEED`, else `default`. */
  def raceSeed(default: RaceSeed): RaceSeed = text("KINOWO_RACE_SEED").flatMap(_.toLongOption).map(RaceSeed(_)).getOrElse(default)

  // ── Convergence legs ────────────────────────────────────────────────────────
  /** `KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES`, when a run points at a particular tree. */
  def enrichmentFixtureTree: Option[EnrichmentFixtureTree] = text("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES").map(EnrichmentFixtureTree(_))

  /** `KINOWO_CONVERGENCE_HERMETIC` (`1` or `true`). */
  def hermeticReplay: HermeticReplay = HermeticReplay(env.flag("KINOWO_CONVERGENCE_HERMETIC"))
  /** `KINOWO_CONVERGENCE_FILL_ONLY` (`1` or `true`). */
  def gapFill: GapFill = GapFill(env.flag("KINOWO_CONVERGENCE_FILL_ONLY"))

  /** `KINOWO_CONVERGENCE_ORDER_PASSES` (`0,1`): a split order row's passes. An entry that is not a pass
   *  number is refused, not skipped -- the row would otherwise compare fewer passes than it says. */
  def orderPasses: Option[OrderPasses] = text("KINOWO_CONVERGENCE_ORDER_PASSES").map { list =>
    OrderPasses(list.split(',').toSeq.map(_.trim).map(pass =>
      pass.toIntOption.getOrElse(throw new IllegalArgumentException(s"KINOWO_CONVERGENCE_ORDER_PASSES: '$pass' is not a pass number in '$list'"))))
  }
  /** `KINOWO_CONVERGENCE_ORDER_PASS_OUT`: a one-pass order row writes its pass's corpus here. */
  def orderPassOut: Option[OrderPassOut] = text("KINOWO_CONVERGENCE_ORDER_PASS_OUT").map(dir => OrderPassOut(Path.of(dir)))
  /** `KINOWO_CONVERGENCE_ORDER_PASS_IN`: a comparison job reads the passes' corpora here instead of replaying them. */
  def orderPassIn: Option[OrderPassIn] = text("KINOWO_CONVERGENCE_ORDER_PASS_IN").map(dir => OrderPassIn(Path.of(dir)))

  /** `KINOWO_IDENTITY_LOOKUPS` (`1` or `true`). */
  def identityLookupSweep: IdentityLookupSweepEnabled = IdentityLookupSweepEnabled(env.flag("KINOWO_IDENTITY_LOOKUPS"))

  def corpusRunId: Option[CorpusRunId]                     = text("KINOWO_CONVERGENCE_CORPUS_RUN").map(CorpusRunId(_))
  def corpusRecordedAt: Option[CorpusRecordedAt]           = text("KINOWO_CONVERGENCE_CORPUS_RECORDED_AT").map(CorpusRecordedAt(_))
  def greenCorpusRunId: Option[GreenCorpusRunId]           = text("KINOWO_CONVERGENCE_GREEN_CORPUS_RUN").map(GreenCorpusRunId(_))
  def greenCorpusRecordedAt: Option[GreenCorpusRecordedAt] = text("KINOWO_CONVERGENCE_GREEN_CORPUS_RECORDED_AT").map(GreenCorpusRecordedAt(_))
  def greenCorpusDirectory: Option[GreenCorpusDirectory]   = text("KINOWO_CONVERGENCE_GREEN_CORPUS_DIR").map(dir => GreenCorpusDirectory(Path.of(dir)))

  /** `KINOWO_CONVERGENCE_SCRAPES_URI` / `KINOWO_CONVERGENCE_SCRAPES_DB` — a production dump a
   *  convergence leg (or the corpus recorder) reads its real scrapes from, read-only. */
  def convergenceScrapesUri: Option[ConvergenceScrapesUri] = text("KINOWO_CONVERGENCE_SCRAPES_URI").map(ConvergenceScrapesUri(_))
  def convergenceScrapesDatabase: Option[ConvergenceScrapesDatabaseName] =
    text("KINOWO_CONVERGENCE_SCRAPES_DB").map(ConvergenceScrapesDatabaseName(_))

  /** `GITHUB_STEP_SUMMARY` — the file a CI step appends its markdown summary to. */
  def stepSummaryFile: Option[StepSummaryFile] = fact("GITHUB_STEP_SUMMARY").map(file => StepSummaryFile(Path.of(file)))

  // ── Identity measuring tools ────────────────────────────────────────────────
  /** `KINOWO_HARD_CLUSTERS_COUNTRIES` — comma-separated codes narrowing a hard-cluster run; codes
   *  naming no country are dropped. */
  def hardClusterCountries: Option[HardClusterCountries] =
    text("KINOWO_HARD_CLUSTERS_COUNTRIES").map(codes => HardClusterCountries(codes.split(",").iterator.map(_.trim).flatMap(Country.byCode).toSet))
  /** `KINOWO_HARD_CLUSTERS_DUMP` (`1` or `true`) — print every pass's films. */
  def hardClusterDump: HardClusterDump = HardClusterDump(env.flag("KINOWO_HARD_CLUSTERS_DUMP"))

  /** `KINOWO_IDENTITY_CORPUS_DIR` — recorded corpora the listing-key spec widens its sweep to. */
  def identityCorpusDirectory: Option[IdentityCorpusDirectory] =
    text("KINOWO_IDENTITY_CORPUS_DIR").map(dir => IdentityCorpusDirectory(Path.of(dir)))
  /** `KINOWO_IDENTITY_CACHE` — where the identity tools keep the recordings they fetched; `~/.cache/kinowo-identity` by
   *  default, as the scripts read it. */
  def identityCache: Option[IdentityCache] =
    text("KINOWO_IDENTITY_CACHE").orElse(text("HOME").map(_ + "/.cache/kinowo-identity")).map(dir => IdentityCache(Path.of(dir)))
  /** `KINOWO_IDENTITY_GATE=strict` — the identity query-coverage gate fails on any gap, not just reports it. */
  def identityGateStrict: IdentityGateStrict = IdentityGateStrict(text("KINOWO_IDENTITY_GATE").contains("strict"))
  /** `KINOWO_IDENTITY_FULL` — comma-separated codes of the full recorded corpora the identity gate
   *  measures besides the hard clusters; codes naming no country are dropped. */
  def identityFullCorpora: IdentityFullCorpora =
    IdentityFullCorpora(text("KINOWO_IDENTITY_FULL").toSeq.flatMap(_.split(",")).map(_.trim.toLowerCase(Locale.ROOT)).flatMap(Country.byCode).toSet)
  /** `KINOWO_IDENTITY_OUT` — where the identity shadow run writes its reports and calibration
   *  dataset, else `target/identity-shadow`. */
  def identityShadowOutput: IdentityShadowOutput =
    IdentityShadowOutput(Path.of(text("KINOWO_IDENTITY_OUT").getOrElse("target/identity-shadow")))
  /** `KINOWO_IDENTITY_AGREEMENT_CACHE` — with it, the resolver-only replay also runs the agreement stage over its no-matches
   *  (`agreement.AgreementStage`), the families answered from this directory (`<host>/<sha256 of "METHOD url body">`, the
   *  signal-combination experiment's cache), else live — a measuring loop, never in CI. */
  def identityAgreementCache: Option[IdentityAgreementCache] = text("KINOWO_IDENTITY_AGREEMENT_CACHE").map(dir => IdentityAgreementCache(Path.of(dir)))
  /** `KINOWO_IDENTITY_POSTER_CACHE` — posters and TMDB poster lists the unmatched-cluster capture hashes before fetching
   *  any live (and files what it fetched into), for the agreement's poster evidence. Never in CI. */
  def identityPosterCache: Option[IdentityPosterCache] = text("KINOWO_IDENTITY_POSTER_CACHE").map(dir => IdentityPosterCache(Path.of(dir)))
  /** `KINOWO_IDENTITY_FAMILY_SEED` — a directory of `<db>.jsonl` exports of prod's `identity_family_answers` (one EJSON
   *  document a line) the unmatched-cluster capture files before asking the families anything. */
  def identityFamilySeed: Option[IdentityFamilySeed] = text("KINOWO_IDENTITY_FAMILY_SEED").map(dir => IdentityFamilySeed(Path.of(dir)))
  /** `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY` — a TMDB key with which the resolver-only replay
   *  (`integration.IdentityResolveDump`) answers what its recording cannot from TMDB and IMDb LIVE,
   *  instead of as gaps: the local loop for a change that asks new questions. Never in CI. */
  def identityLiveGaps: Option[IdentityLiveGaps] = text("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY").map(IdentityLiveGaps(_))
  /** `KINOWO_IDENTITY_LIVE_PER_HOST` — a capture's live reads at once per host. */
  def identityLivePerHost: IdentityLivePerHost = IdentityLivePerHost(count("KINOWO_IDENTITY_LIVE_PER_HOST", 4))
  /** `KINOWO_IDENTITY_SEED_FILMS` — a directory of `films-<cc>.json`, today's films as sets of listing
   *  keys (`scripts.ListingKeyBackfill --export`), that the ID-seeding review assigns ids from. */
  def identitySeedFilms: Option[IdentitySeedFilms] =
    text("KINOWO_IDENTITY_SEED_FILMS").map(dir => IdentitySeedFilms(Path.of(dir)))
  /** `KINOWO_IDENTITY_FOCUS=pieśni lasu,vincent` — resolve only listings whose titles hold every word
   *  of one of these comma-separated phrases, print their decisions and stop: a fast check. */
  def identityFocus: Option[IdentityFocus] =
    text("KINOWO_IDENTITY_FOCUS").map(v => IdentityFocus(v.split(",").toSeq
      .map(p => services.movies.TitleContainment.tokens(p).toSet).filter(_.nonEmpty), alone = env.flag("KINOWO_IDENTITY_FOCUS_ALONE")))
      .filter(_.phrases.nonEmpty)
  /** `KINOWO_IDENTITY_RECORD_CHECK` — the country whose recording pass the identity gate checks
   *  against a scratch fixture root. */
  def identityRecordCheck: Option[IdentityRecordCheck] =
    text("KINOWO_IDENTITY_RECORD_CHECK").map(_.trim.toLowerCase(Locale.ROOT)).flatMap(Country.byCode).map(IdentityRecordCheck(_))

  // ── Page tests and the share-card generator ─────────────────────────────────
  /** `CDP_BROWSER_BIN` — the Chrome/Edge binary the page tests drive, over the usual install paths. */
  def cdpBrowserBinary: Option[CdpBrowserBinary] = fact("CDP_BROWSER_BIN").map(bin => CdpBrowserBinary(Path.of(bin)))

  /** `KINOWO_OG_BASE` — the origin the share-card generator screenshots, over the country's own. */
  def ogCardBaseUrl: Option[OgCardBaseUrl] = text("KINOWO_OG_BASE").map(OgCardBaseUrl(_))
  /** `KINOWO_OG_OUT`, else the web assets' image directory. */
  def ogCardOutputDirectory: OgCardOutputDirectory =
    OgCardOutputDirectory(Path.of(text("KINOWO_OG_OUT").getOrElse("web/src/main/assets/img")))
  /** `KINOWO_OG_HOME_CITY` — the city slug the landing card screenshots. */
  def ogCardHomeCity: Option[OgCardHomeCity] = text("KINOWO_OG_HOME_CITY").map(OgCardHomeCity(_))
  /** `KINOWO_OG_PROXY_PORT` — one residential proxy port pinned instead of the rotation. */
  def ogCardProxyPort: Option[OgCardProxyPort] = text("KINOWO_OG_PROXY_PORT").flatMap(_.toIntOption).map(OgCardProxyPort(_))
  /** `KINOWO_OG_PROXY_HOST`, else the residential proxy's own host. */
  def ogCardProxyHost: OgCardProxyHost = OgCardProxyHost(text("KINOWO_OG_PROXY_HOST").getOrElse("isp.decodo.com"))

  // ── The JVM itself ──────────────────────────────────────────────────────────
  /** `java.home` — the running JDK, whose `bin/` a spec launches a child `java` / `keytool` from. */
  def javaHome: JavaHome = JavaHome(Path.of(fact("java.home").getOrElse(throw new IllegalStateException("java.home is not set"))))
  /** `java.class.path` — the running JVM's class path, split into its entries. */
  def javaClassPath: JavaClassPath =
    JavaClassPath(fact("java.class.path").toSeq.flatMap(_.split(java.io.File.pathSeparator)).filter(_.nonEmpty).map(Path.of(_)))
}

object HarnessConfiguration {

  /** This process's harness configuration: environment variables → system properties. Call
   *  once, from a suite or a tool `main`. */
  def resolve(): HarnessConfiguration = new HarnessConfiguration(Env.fromProcess())
}
