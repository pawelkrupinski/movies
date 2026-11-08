package settings

import java.nio.file.Path

// One type per configuration value [[HarnessConfiguration]] resolves, as `ConfigurationValues` is
// for [[ProcessConfiguration]]: unwrapped (`.value`) only at the boundary that needs the raw value.

/** `KINOWO_FIXTURE_DIR` — the fixture tree the recorder (`RecordAllDataToFixture`) writes into. */
final case class RecordingFixtureDirectory(value: String) extends AnyVal
/** `KINOWO_ALLOW_REMOTE_IT` — an integration run deliberately allowed onto a credentialed
 *  cluster. */
final case class RemoteIntegrationAllowed(value: Boolean) extends AnyVal
/** `KINOWO_RACE_SEED` — the seed round 1 of a concurrency spec is drawn from. */
final case class RaceSeed(value: Long) extends AnyVal
/** `KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES` — the fixture tree a convergence replay uses. */
final case class EnrichmentFixtureTree(value: String) extends AnyVal
/** `KINOWO_CONVERGENCE_HERMETIC` — a convergence leg refuses every request it cannot replay. */
final case class HermeticReplay(value: Boolean) extends AnyVal
/** `KINOWO_CONVERGENCE_FILL_ONLY` — a convergence leg replays its tree as a hermetic one does (no
 *  expiry, remembered failures replayed) but fetches, live, only what the tree lacks, and records it:
 *  the `Identity model convergence` build's overlay of the model's own enrichment. */
final case class GapFill(value: Boolean) extends AnyVal
/** `KINOWO_CONVERGENCE_ORDER_PASSES` — the order-independence passes (`0,1`) a split order row replays
 *  and compares; every pass when unset. A leg over its time budget splits its passes across rows. */
final case class OrderPasses(indices: Seq[Int]) extends AnyVal
/** `KINOWO_CONVERGENCE_ORDER_PASS_OUT` — where a ONE-PASS order row writes its pass's corpus (`pass<N>.bin.gz`) for the
 *  comparison job, which compares the passes rather than this row. */
final case class OrderPassOut(value: Path) extends AnyVal
/** `KINOWO_CONVERGENCE_ORDER_PASS_IN` — the order passes' corpora a COMPARISON job reads (`pass<N>.bin.gz`, as the
 *  one-pass rows wrote them) and compares, in place of replaying them. */
final case class OrderPassIn(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_LOOKUPS` — the convergence leg sweeps identity lookups. */
final case class IdentityLookupSweepEnabled(value: Boolean) extends AnyVal
/** `KINOWO_CONVERGENCE_CORPUS_RUN` — the CI run that recorded the replayed corpus. */
final case class CorpusRunId(value: String) extends AnyVal
/** `KINOWO_CONVERGENCE_CORPUS_RECORDED_AT`. */
final case class CorpusRecordedAt(value: String) extends AnyVal
/** `KINOWO_CONVERGENCE_GREEN_CORPUS_RUN` — the run that recorded the last green corpus. */
final case class GreenCorpusRunId(value: String) extends AnyVal
/** `KINOWO_CONVERGENCE_GREEN_CORPUS_RECORDED_AT`. */
final case class GreenCorpusRecordedAt(value: String) extends AnyVal
/** `KINOWO_CONVERGENCE_GREEN_CORPUS_DIR` — where that green corpus was restored to. */
final case class GreenCorpusDirectory(value: Path) extends AnyVal
/** `KINOWO_CONVERGENCE_SCRAPES_URI` — a production dump of real scrapes, read-only. */
final case class ConvergenceScrapesUri(value: String) extends AnyVal
/** `KINOWO_CONVERGENCE_SCRAPES_DB` — that dump's database. */
final case class ConvergenceScrapesDatabaseName(value: String) extends AnyVal
/** `GITHUB_STEP_SUMMARY` — the file a CI step's markdown summary is appended to. */
final case class StepSummaryFile(value: Path) extends AnyVal
/** `KINOWO_HARD_CLUSTERS_COUNTRIES` — the countries a hard-cluster run is narrowed to. */
final case class HardClusterCountries(value: Set[models.Country]) extends AnyVal
/** `KINOWO_HARD_CLUSTERS_DUMP` (`1` or `true`) — print every hard-cluster pass's films. */
final case class HardClusterDump(value: Boolean) extends AnyVal
/** `KINOWO_IDENTITY_CORPUS_DIR` — recorded corpora the listing-key spec sweeps. */
final case class IdentityCorpusDirectory(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_CACHE` (default `~/.cache/kinowo-identity`) — the identity tools' cache: the recordings they fetched. */
final case class IdentityCache(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_GATE=strict` — the identity query-coverage gate fails on any gap. */
final case class IdentityGateStrict(value: Boolean) extends AnyVal
/** How many times longer than its default every test wait bound runs (`tools.SpecTimeouts`). */
final case class SpecTimeScale(value: Int) extends AnyVal
/** `KINOWO_IDENTITY_FULL` — the full recorded corpora the identity gate measures. */
final case class IdentityFullCorpora(value: Set[models.Country]) extends AnyVal
/** `KINOWO_IDENTITY_OUT` — where the identity shadow run writes its reports and calibration dataset. */
final case class IdentityShadowOutput(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_SEED_FILMS` — today's films as listing-key sets, the ID-seeding review's previous assignment. */
final case class IdentitySeedFilms(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_AGREEMENT_CACHE` — the signal-combination experiment's answer cache the resolver-only replay reads
 *  other film database families from before asking them live. */
final case class IdentityAgreementCache(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_FAMILY_SEED` — prod's `identity_family_answers`, exported per country database, for the capture to file first. */
final case class IdentityFamilySeed(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_POSTER_CACHE` — posters already downloaded (`img/<sha1 of the URL>`, an `.err` beside a failed one) and
 *  TMDB films' poster lists (`tmdb/<id>.json`), which the unmatched-cluster capture hashes before fetching anything. */
final case class IdentityPosterCache(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY` — a resolver-only replay asks TMDB and IMDb live for what its recording cannot answer. */
final case class IdentityLiveGaps(tmdbKey: String) extends AnyVal
/** `KINOWO_IDENTITY_LIVE_PER_HOST` — how many live reads a capture runs at once per host (default 4); `scripts/identity-capture.sh`
 *  splits one budget between the countries it runs side by side, and each run halves its own on a 429 or 503. */
final case class IdentityLivePerHost(value: Int) extends AnyVal
/** `KINOWO_IDENTITY_FOCUS` — title phrases a measurement run shows old against new, and stops. A
 *  title is in focus when it holds EVERY word of one phrase, so "dark city" is Dark City, not every
 *  dark or city title. The whole corpus is resolved and only the focus shown, so a focused answer is
 *  the measurement's; `alone` (`KINOWO_IDENTITY_FOCUS_ALONE`) resolves the focused listings by
 *  themselves instead — seconds, but a family member outside the focus can change the answer. */
final case class IdentityFocus(phrases: Seq[Set[String]], alone: Boolean = false) {
  def covers(titleWords: Set[String]): Boolean = phrases.exists(_.subsetOf(titleWords))
}
/** `KINOWO_IDENTITY_RECORD_CHECK` — the country whose recording pass the identity gate checks. */
final case class IdentityRecordCheck(value: models.Country) extends AnyVal
/** `CDP_BROWSER_BIN` — the browser the page tests drive over CDP. */
final case class CdpBrowserBinary(value: Path) extends AnyVal
/** `KINOWO_OG_BASE` — the origin the share-card generator screenshots. */
final case class OgCardBaseUrl(value: String) extends AnyVal
/** `KINOWO_OG_OUT` — where the share-card generator writes its images. */
final case class OgCardOutputDirectory(value: Path) extends AnyVal
/** `KINOWO_OG_HOME_CITY` — the city slug the landing share card shows. */
final case class OgCardHomeCity(value: String) extends AnyVal
/** `KINOWO_OG_PROXY_PORT` — a single residential proxy port the generator pins. */
final case class OgCardProxyPort(value: Int) extends AnyVal
/** `KINOWO_OG_PROXY_HOST` — the residential proxy host the generator routes through. */
final case class OgCardProxyHost(value: String) extends AnyVal
/** `java.home` — the running JDK's installation directory. */
final case class JavaHome(value: Path) extends AnyVal {
  /** An executable the JDK ships in its `bin/` (`java`, `keytool`). */
  def binary(name: String): Path = value.resolve("bin").resolve(name)
}
/** `java.class.path` — the running JVM's class path entries. */
final case class JavaClassPath(value: Seq[Path]) extends AnyVal
