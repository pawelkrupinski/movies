package settings

import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration

// One type per configuration value [[ProcessConfiguration]] resolves (a test harness's own are in
// testkit's `HarnessValues`), so the compiler decides what may be passed where: a TMDB key cannot be handed to OMDb, a users database cannot open
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
/** Which kind of process a composition root is — the web tier, a worker, or a one-off tool's
 *  `main` — named where an outside system must tell them apart (mongod's `appName`). */
enum ProcessTier(val label: String) {
  case Web    extends ProcessTier("web")
  case Worker extends ProcessTier("worker")
  case Tool   extends ProcessTier("tool")
}

// ── Mongo ─────────────────────────────────────────────────────────────────────
/** `MONGODB_URI` — the cluster this process's corpus lives on. */
final case class MongoUri(value: String) extends AnyVal
// `MONGODB_DB` and `MONGODB_USERS_DB` are `DatabaseNames.scala`'s: `models.Country` names them.
/** `MONGODB_FLEET_DB` — the database every country's web and worker share for fleet-wide state (`services.FleetDatabase`). */
final case class FleetDatabaseName(value: String) extends AnyVal
/** `MONGODB_MOVIES_MIRROR_URI` — the local read-mirror /debug reads instead of the tunnel. */
final case class MirrorMongoUri(value: String) extends AnyVal {
  /** The mirror is a Mongo cluster like any other, once chosen. */
  def asMongoUri: MongoUri = MongoUri(value)
}
/** `REVIEW_LABELS_FILE` — the `identity-judgements.tsv` the /debug review pages write their answers into. */
final case class ReviewLabelsFile(value: Path) extends AnyVal
/** `MONGODB_PROBE_TIMEOUT_SECONDS` — how long a boot waits for Mongo to answer. */
final case class MongoProbeTimeout(value: FiniteDuration) extends AnyVal
/** `KINOWO_MONGO_MAX_POOL_SIZE` — connections per MongoClient. */
final case class MongoMaxPoolSize(value: Int) extends AnyVal
/** The `appName` a MongoClient sends at its handshake, which mongod's slow-op log lines and
 *  `currentOp` show — derived from the process tier and its countries
 *  ([[ProcessConfiguration.mongoApplicationName]]). Build through [[MongoApplicationName.of]]. */
final case class MongoApplicationName private (value: String) extends AnyVal
object MongoApplicationName {
  /** The driver refuses (and so fails the boot over) an applicationName longer than this, in UTF-8 bytes. */
  val MaxBytes: Int = 128

  /** What a connection opened without a process's own name sends. */
  val Default: MongoApplicationName = of("kinowo")

  /** `raw`, cut to [[MaxBytes]] UTF-8 bytes at a character boundary. */
  def of(raw: String): MongoApplicationName = {
    val characters = raw.codePoints().toArray.toSeq.map(Character.toString)
    val widths     = characters.map(_.getBytes(java.nio.charset.StandardCharsets.UTF_8).length).scanLeft(0)(_ + _).tail
    new MongoApplicationName(characters.zip(widths).takeWhile(_._2 <= MaxBytes).map(_._1).mkString)
  }
}
/** `MONGODB_OPTIONAL` — a local dev's opt-out of the required-Mongo boot check. */
final case class MongoOptional(value: Boolean) extends AnyVal

// ── Third-party credentials and ids ─────────────────────────────────────────────
/** `TMDB_API_KEY` (a v3 key or a v4 read token). */
final case class TmdbApiKey(value: String) extends AnyVal {
  /** The header every TMDB request carries the key in — beside the `api_key` parameter, so a v3 key and a v4 read token
   *  both authenticate. Also how a convergence fill signs a TMDB request it was listed masked (`tools.FillCredentials`). */
  def authorization: Map[String, String] = Map("Authorization" -> s"Bearer $value")
}
/** `OMDB_API_KEY`. */
final case class OmdbApiKey(value: String) extends AnyVal
/** `ZYTE_API_KEY`. */
final case class ZyteApiKey(value: String) extends AnyVal
// (ProxyUser and ProxyPassword live in the http module, beside RealHttpFetch.ProxyConfig.)
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

// ── The identity model ──────────────────────────────────────────────────────────
/** `KINOWO_IDENTITY_RATING_GATE` — forces the identity phase-3 rating gate on (cards below the
 *  calibrated identity confidence served without ratings) for a country the worker wiring does not
 *  already gate (`ReadModelWiring.RatingGateCountries`: Germany, Spain). It cannot turn the gate off. */
final case class IdentityRatingGateEnabled(value: Boolean) extends AnyVal
/** `KINOWO_IDENTITY_RETENTION_DAYS` — how long a departed listing stays an identity input while a billed listing relates to
 *  it by title (`services.identity.ListingRetention`), counted from its departure against the model's clock
 *  (docs/design/identity-listing-retention.md). */
final case class IdentityRetentionWindow(value: FiniteDuration) extends AnyVal {
  def duration: java.time.Duration = java.time.Duration.ofMillis(value.toMillis)
}
/** `KINOWO_IDENTITY_SHADOW_INTERVAL_SECONDS` — how often the identity model's paced live lookup fill
 *  runs a round (its own claimed window, independent of the settle). */
final case class IdentityLookupFillInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_IDENTITY_SHADOW_INITIAL_DELAY_SECONDS` — how long after boot the fill's first round waits. */
final case class IdentityLookupFillInitialDelay(value: FiniteDuration) extends AnyVal
/** `KINOWO_IDENTITY_SHADOW_LOOKUP_RATE` — live asks per minute the lookup fill may make: the cap on
 *  its share of the lookup chain. */
final case class IdentityLookupFillRate(perMinute: Int) extends AnyVal {
  def pace: FiniteDuration = scala.concurrent.duration.Duration(60000L / perMinute.max(1), "millis")
  /** How many asks fit in `window` at this rate (at least one). */
  def allowanceOver(window: FiniteDuration): Int = (perMinute.toLong * window.toSeconds / 60).toInt.max(1)
  def halved: IdentityLookupFillRate = IdentityLookupFillRate((perMinute / 2).max(1))
}
