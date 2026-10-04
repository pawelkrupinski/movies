package settings

import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration

// One type per configuration value [[ProcessConfiguration]] resolves, so the compiler decides
// what may be passed where: a TMDB key cannot be handed to OMDb, a users database cannot open
// as the corpus, a settle interval cannot pace the enrichment reaper. Each is unwrapped
// (`.value`) only at the boundary that needs the raw value — the HTTP header, the driver call,
// the scheduler, the log line.

// ── Deployment ────────────────────────────────────────────────────────────────
/** `KINOWO_COUNTRIES` — the countries a worker process runs (codes resolved to countries). */
final case class WorkerCountries(value: Seq[models.Country]) extends AnyVal
/** A `KINOWO_COUNTRIES` code that names no country — reported, then skipped. */
final case class UnknownCountryCode(value: String) extends AnyVal
/** A setting an integration or alerter needs and does not have — its key, and why when it is
 *  set but unusable (`KEY (not a number)`). */
final case class MissingSetting(value: String) extends AnyVal
/** `COMMIT_SHA` — the commit the running image was built from. */
final case class CommitSha(value: String) extends AnyVal
/** `GITHUB_SHA` — the commit a CI tool run is reporting on. */
final case class GithubCommitSha(value: String) extends AnyVal
/** `PORT` — the port the worker's /health + /metrics server listens on. */
final case class HealthPort(value: Int) extends AnyVal
/** `PATH` — the directories an executable (vips) is looked for in, in order. */
final case class ExecutableSearchPath(value: Seq[Path]) extends AnyVal
/** `APP_MODE` — an explicit override of the mode Play derived for itself. */
enum ApplicationMode { case Development, Test, Production }

// ── Mongo ─────────────────────────────────────────────────────────────────────
/** `MONGODB_URI` — the cluster this process's corpus lives on. */
final case class MongoUri(value: String) extends AnyVal
// `MONGODB_DB` and `MONGODB_USERS_DB` are `DatabaseNames.scala`'s: `models.Country` names them.
/** `MONGODB_MOVIES_MIRROR_URI` — the local read-mirror /debug reads instead of the tunnel. */
final case class MirrorMongoUri(value: String) extends AnyVal {
  /** The mirror is a Mongo cluster like any other, once chosen. */
  def asMongoUri: MongoUri = MongoUri(value)
}
/** `MONGODB_PROBE_TIMEOUT_SECONDS` — how long a boot waits for Mongo to answer. */
final case class MongoProbeTimeout(value: FiniteDuration) extends AnyVal
/** `KINOWO_MONGO_MAX_POOL_SIZE` — connections per MongoClient. */
final case class MongoMaxPoolSize(value: Int) extends AnyVal
/** `MONGODB_OPTIONAL` — a local dev's opt-out of the required-Mongo boot check. */
final case class MongoOptional(value: Boolean) extends AnyVal

// ── Third-party credentials and ids ─────────────────────────────────────────────
/** `TMDB_API_KEY` (a v3 key or a v4 read token). */
final case class TmdbApiKey(value: String) extends AnyVal
/** `OMDB_API_KEY`. */
final case class OmdbApiKey(value: String) extends AnyVal
/** `ANTHROPIC_API_KEY` — the language model the identity resolver asks about listings no rule took. */
final case class AnthropicApiKey(value: String) extends AnyVal
/** `ZYTE_API_KEY`. */
final case class ZyteApiKey(value: String) extends AnyVal
/** `KINOWO_PROXY_USER` — the residential egress's user. */
final case class ProxyUser(value: String) extends AnyVal
/** `KINOWO_PROXY_PASS` — the residential egress's password. */
final case class ProxyPassword(value: String) extends AnyVal
/** `FACEBOOK_APP_ID` — the Facebook app sign-in and the share-card re-scrape run as. */
final case class FacebookAppId(value: String) extends AnyVal
/** `FACEBOOK_APP_SECRET`. */
final case class FacebookAppSecret(value: String) extends AnyVal
/** A GitHub token allowed to start the retire-venues workflow (`actions: write` on the repository). */
final case class GitHubDispatchToken(value: String) extends AnyVal
/** `FB_APP_ID` — the `fb:app_id` a page's Open Graph block carries. */
final case class FacebookPageAppId(value: String) extends AnyVal
/** `GA_MEASUREMENT_ID` — the GA4 property pages report to. */
final case class GoogleAnalyticsMeasurementId(value: String) extends AnyVal
/** `SENTRY_LOADER_URL` — the Sentry loader script a page embeds. */
final case class SentryLoaderUrl(value: String) extends AnyVal
/** `GOOGLE_CLIENT_ID`. */
final case class GoogleClientId(value: String) extends AnyVal
/** `GOOGLE_CLIENT_SECRET`. */
final case class GoogleClientSecret(value: String) extends AnyVal
/** `APPLE_BUNDLE_ID` — the iOS app Sign in with Apple tokens must be issued for. */
final case class AppleBundleId(value: String) extends AnyVal
/** `ADMIN_ALLOWLIST` — the emails allowed onto the operational pages. */
final case class AdminAllowlist(value: Set[String]) extends AnyVal
/** `TELEGRAM_BOT_TOKEN` — the bot every alerter posts as. */
final case class TelegramBotToken(value: String) extends AnyVal
/** A Telegram chat an alerter posts to (`KINOWO_*_TG_CHAT_ID`). */
final case class TelegramChatId(value: Long) extends AnyVal
/** A forum topic within that chat (`KINOWO_*_TG_TOPIC_ID`). */
final case class TelegramTopicId(value: Long) extends AnyVal

// ── Worker storage ─────────────────────────────────────────────────────────────
/** `KINOWO_SHARE_CARD_DIR` — where rendered share cards are kept; `{cc}` is the country. */
final case class ShareCardDirectoryTemplate(value: String) extends AnyVal {
  def forCountry(country: models.Country): Path = Path.of(value.replace("{cc}", country.code))
}
/** `KINOWO_HEAP_DUMP_DIR` — where an on-demand or watchdog heap dump is written. */
final case class HeapDumpDirectory(value: Path) extends AnyVal
/** `KINOWO_SCRAPE_CITIES` — the city slugs a worker scrapes, overriding every modelled city. */
final case class ScrapeCitySlugs(value: Set[String]) extends AnyVal

// ── Test and replay harness ─────────────────────────────────────────────────────
/** `KINOWO_LOCAL_MONGO_URI` — the local stack's Mongo. */
final case class LocalStackMongoUri(value: String) extends AnyVal
/** `KINOWO_LOCAL_MONGO_DB` — the local stack's database. */
final case class LocalStackDatabaseName(value: String) extends AnyVal
/** `KINOWO_FIXTURE_DIR` — the fixture tree the local stack's worker replays. */
final case class LocalStackFixtureDirectory(value: String) extends AnyVal
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
/** `KINOWO_IDENTITY_RATING_GATE` — the identity phase-3 staged-migration switch: cards below the
 *  calibrated identity confidence are served without ratings. Off by default. */
final case class IdentityRatingGateEnabled(value: Boolean) extends AnyVal
/** `KINOWO_IDENTITY_PROJECTION_SECONDS` — the identity projection's period. */
final case class IdentityProjectionInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_IDENTITY_SHADOW_INTERVAL_SECONDS` — how often the identity model's paced live lookup fill
 *  runs a round (its own claimed window, independent of the settle). */
final case class IdentityShadowInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_IDENTITY_SHADOW_INITIAL_DELAY_SECONDS` — how long after boot the fill's first round waits. */
final case class IdentityShadowInitialDelay(value: FiniteDuration) extends AnyVal
/** `KINOWO_IDENTITY_SHADOW_LOOKUP_RATE` — live asks per minute the lookup fill may make: the cap on
 *  its share of the lookup chain. */
final case class IdentityShadowLookupRate(perMinute: Int) extends AnyVal {
  def pace: FiniteDuration = scala.concurrent.duration.Duration(60000L / perMinute.max(1), "millis")
  /** How many asks fit in `window` at this rate (at least one). */
  def allowanceOver(window: FiniteDuration): Int = (perMinute.toLong * window.toSeconds / 60).toInt.max(1)
  def halved: IdentityShadowLookupRate = IdentityShadowLookupRate((perMinute / 2).max(1))
}
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
/** `KINOWO_HARD_CLUSTERS_RECORD` — the hard-cluster spec re-records its responses. */
final case class HardClusterRecording(value: Boolean) extends AnyVal
/** `KINOWO_HARD_CLUSTERS_COUNTRIES` — the countries a hard-cluster run is narrowed to. */
final case class HardClusterCountries(value: Set[models.Country]) extends AnyVal
/** `KINOWO_HARD_CLUSTERS_DUMP` (`1` or `true`) — print every hard-cluster pass's films. */
final case class HardClusterDump(value: Boolean) extends AnyVal
/** `KINOWO_IDENTITY_CORPUS_DIR` — recorded corpora the listing-key spec sweeps. */
final case class IdentityCorpusDirectory(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_GATE=strict` — the identity query-coverage gate fails on any gap. */
final case class IdentityGateStrict(value: Boolean) extends AnyVal
/** How many times longer than its default every test wait bound runs (`tools.SpecTimeouts`). */
final case class SpecTimeScale(value: Int) extends AnyVal
/** `KINOWO_IDENTITY_FULL` — the full recorded corpora the identity gate measures. */
final case class IdentityFullCorpora(value: Set[models.Country]) extends AnyVal
final case class IdentityShadowOutput(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_SEED_FILMS` — today's films as listing-key sets, the ID-seeding review's previous assignment. */
final case class IdentitySeedFilms(value: Path) extends AnyVal
final case class IdentityDump(value: Path) extends AnyVal
/** `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY` — a resolver-only replay asks TMDB and IMDb live for what its recording cannot answer. */
final case class IdentityLiveGaps(tmdbKey: String) extends AnyVal
final case class IdentityShadowPermutations(value: Int) extends AnyVal
/** `KINOWO_IDENTITY_PIPELINE_CACHE` — where the identity shadow run keeps each corpus's booted
 *  pipeline answers, so resolver variants measure against one boot. */
final case class IdentityPipelineCache(value: Path) extends AnyVal
final case class IdentityPipelineBootOnly(value: Boolean) extends AnyVal
/** `KINOWO_IDENTITY_ROBUSTNESS=off` — the identity shadow run skips its re-resolving robustness
 *  measures (arrival orders, outage, perturbation). */
final case class IdentityShadowRobustness(value: Boolean) extends AnyVal
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

// ── The JVM itself ────────────────────────────────────────────────────────────
/** `java.home` — the running JDK's installation directory. */
final case class JavaHome(value: Path) extends AnyVal {
  /** An executable the JDK ships in its `bin/` (`java`, `keytool`). */
  def binary(name: String): Path = value.resolve("bin").resolve(name)
}
/** `java.class.path` — the running JVM's class path entries. */
final case class JavaClassPath(value: Seq[Path]) extends AnyVal
