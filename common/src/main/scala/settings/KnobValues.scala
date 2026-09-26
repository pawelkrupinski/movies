package settings

import scala.concurrent.duration.FiniteDuration

// The tuning knobs [[ProcessConfiguration]] resolves — one type each, for the same reason as
// `ConfigurationValues`: two Ints or two durations must not be swappable. Each is flippable
// on `/admin/config`; its default lives where it is read (`ProcessConfiguration`'s callers
// pass it), and it is unwrapped only where a scheduler, a semaphore or a query needs the
// number.

// ── Concurrency and budgets ──────────────────────────────────────────────────
/** `KINOWO_BG_CONCURRENCY` — run permits shared by scrape, enrichment and the refreshers. */
final case class BackgroundConcurrency(value: Int) extends AnyVal
/** `KINOWO_WORKER_POOL_SIZE` — task-worker threads. */
final case class WorkerPoolSize(value: Int) extends AnyVal
/** `KINOWO_WORKER_LIVENESS_STALE_MINUTES` — how stale a heartbeat may get before /health fails. */
final case class LivenessStaleAfter(value: FiniteDuration) extends AnyVal
/** `KINOWO_CONFIG_REFRESH_SECONDS` — how often the admin overrides are re-read. */
final case class ConfigRefreshInterval(value: FiniteDuration) extends AnyVal

// ── Scrape cadence ────────────────────────────────────────────────────────────
/** `KINOWO_SCRAPE_CHUNK_SPREAD_MINUTES`. */
final case class ScrapeChunkSpread(value: FiniteDuration) extends AnyVal
/** `KINOWO_SCRAPE_INITIAL_DELAY_SECONDS`. */
final case class ScrapeInitialDelay(value: FiniteDuration) extends AnyVal
/** `KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK`. */
final case class ScrapeMaxEnqueuePerTick(value: Int) extends AnyVal
/** `KINOWO_SCRAPE_BOOT_RAMP_MINUTES`. */
final case class ScrapeBootRamp(value: FiniteDuration) extends AnyVal
/** `KINOWO_SCRAPE_ENQUEUE_SPREAD_SLICES`. */
final case class ScrapeEnqueueSpreadSlices(value: Int) extends AnyVal
/** `KINOWO_SCRAPE_MAX_OUTSTANDING_TASKS`. */
final case class ScrapeMaxOutstandingTasks(value: Int) extends AnyVal
/** `KINOWO_SCRAPE_TASKS_PER_VENUE`. */
final case class ScrapeTasksPerVenue(value: Int) extends AnyVal
/** `KINOWO_SCRAPE_FRESHNESS_MINUTES` — how long a venue's scrape stays fresh. */
final case class ScrapeFreshness(value: FiniteDuration) extends AnyVal

// ── Enrichment, resolution and staging ───────────────────────────────────────
/** `KINOWO_ENRICHMENT_MAX_ENQUEUE_PER_TICK`. */
final case class EnrichmentMaxEnqueuePerTick(value: Int) extends AnyVal
/** `KINOWO_ENRICHMENT_TICK_INTERVAL_SECONDS`. */
final case class EnrichmentTickInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_DETAIL_MAX_ENQUEUE_PER_TICK`. */
final case class DetailMaxEnqueuePerTick(value: Int) extends AnyVal
/** `KINOWO_DETAIL_TICK_INTERVAL_SECONDS`. */
final case class DetailTickInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_TMDB_RETRY_MAX_ENQUEUE_PER_TICK`. */
final case class TmdbRetryMaxEnqueuePerTick(value: Int) extends AnyVal
/** `KINOWO_SETTLE_INTERVAL_SECONDS`. */
final case class SettleInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_OMDB_BACKFILL_INTERVAL_SECONDS`. */
final case class OmdbBackfillInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_STAGING_PROMOTE_INITIAL_SECONDS`. */
final case class StagingPromoteInitialDelay(value: FiniteDuration) extends AnyVal
/** `KINOWO_STAGING_PROMOTE_SECONDS`. */
final case class StagingPromoteInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_STAGING_STUCK_MINUTES` — how long a staging row may sit before it is alerted on. */
final case class StagingStuckThreshold(value: FiniteDuration) extends AnyVal
/** `KINOWO_STAGING_STUCK_SCAN_MINUTES`. */
final case class StagingStuckScanInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_FILMWEB_DROP_THRESHOLD` — consecutive drops before the Filmweb alert fires. */
final case class FilmwebDropThreshold(value: Int) extends AnyVal
/** `KINOWO_ZYTE_SESSION_TTL_SECONDS`. */
final case class ZyteSessionTtl(value: FiniteDuration) extends AnyVal

// ── Caches and the read model ──────────────────────────────────────────────────
/** `KINOWO_CACHE_REHYDRATE_SECONDS` — the movie cache's backstop rehydrate. */
final case class CacheRehydrateInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS` — 0: retry the boot hydrate until it succeeds. */
final case class BootHydrateMaxAttempts(value: Int) extends AnyVal
/** `KINOWO_BOOT_HYDRATE_RETRY_MS`. */
final case class BootHydrateRetryInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_READMODEL_PRUNE_SECONDS`. */
final case class ReadModelPruneInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_READMODEL_PRUNE_BOOT_DELAY_SECONDS`. */
final case class ReadModelPruneBootDelay(value: FiniteDuration) extends AnyVal
/** `KINOWO_READMODEL_RELOAD_SECONDS` — the web read model's backstop reload. */
final case class ReadModelReloadInterval(value: FiniteDuration) extends AnyVal
/** `KINOWO_READMODEL_COLD_RETRY_SECONDS`. */
final case class ReadModelColdRetryInterval(value: FiniteDuration) extends AnyVal
/** How many rows one pass of a rechecked audit samples — either audit's own knob. */
sealed trait AuditSample extends Any { def value: Int }
/** `KINOWO_READMODEL_AUDIT_SAMPLE`. */
final case class ReadModelAuditSample(value: Int) extends AnyVal with AuditSample

// ── Share cards ─────────────────────────────────────────────────────────────────
/** `KINOWO_SHARE_CARD_BUDGET_MB` — the card store's disk budget, in bytes. */
final case class ShareCardStorageBudget(bytes: Long) extends AnyVal
/** `KINOWO_SHARE_CARD_BACKFILL_BATCH`. */
final case class ShareCardBackfillBatch(value: Int) extends AnyVal
/** `KINOWO_SHARE_CARD_BACKFILL_MAX_BACKLOG`. */
final case class ShareCardBackfillMaxBacklog(value: Int) extends AnyVal
/** `KINOWO_SHARE_CARD_DECODE_MEMORY_MB` — the vips child's memory cap. */
final case class PosterDecodeMemoryCap(megabytes: Long) extends AnyVal
/** `KINOWO_SHARE_CARD_FIRST_HOLD_SECONDS` — how long a new card waits for its share card. */
final case class ShareCardFirstHold(value: FiniteDuration) extends AnyVal
/** `KINOWO_SHARE_CARD_AUDIT_SAMPLE`. */
final case class ShareCardAuditSample(value: Int) extends AnyVal with AuditSample

// ── Mongo ─────────────────────────────────────────────────────────────────────
// (MongoProbeTimeout, MongoMaxPoolSize, MongoOptional live with the Mongo values.)

// ── Per-host pacing ─────────────────────────────────────────────────────────────
/** A paced host's `KINOWO_*_PACE_MS` knob — its identity is the knob, so one host's pace can
 *  never be read as another's. */
enum PaceKnob(val key: String) {
  case Filmstarts extends PaceKnob("KINOWO_FILMSTARTS_PACE_MS")
  case Sensacine  extends PaceKnob("KINOWO_SENSACINE_PACE_MS")
  case Flicks     extends PaceKnob("KINOWO_FLICKS_PACE_MS")
  case FlicksUs   extends PaceKnob("KINOWO_FLICKS_US_PACE_MS")
  case Alamo      extends PaceKnob("KINOWO_ALAMO_PACE_MS")
  case ShowcaseUs extends PaceKnob("KINOWO_SHOWCASE_US_PACE_MS")
  case Landmark   extends PaceKnob("KINOWO_LANDMARK_PACE_MS")
  case Ocine      extends PaceKnob("KINOWO_OCINE_PACE_MS")
  case Kinoprogramm extends PaceKnob("KINOWO_KINOPROGRAMM_PACE_MS")
}
/** The live minimum gap between two requests to a paced host. */
final case class HostPace(value: java.time.Duration) extends AnyVal
