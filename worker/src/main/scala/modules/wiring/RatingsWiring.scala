package modules.wiring

import modules.WorkerWiring
import services.attempts.{EnrichmentAttemptStore, MongoEnrichmentAttemptStore}
import services.cadence.{MongoRatingCadenceStore, RatingCadenceStore}
import services.enrichment.{FilmwebRatings, ImdbRatings, MetascoreRatings, MongoOmdbAttemptStore, OmdbAttemptStore, OmdbBackfill, RottenTomatoesRatings}
import services.events.ImdbIdMissing
import services.freshness.FreshnessKind
import services.tasks.{BulkCadenceRecorder, EnrichTaskKeys, EnrichmentReaper, OmdbBackfillReaper, RatingEnqueuer, RatingHandler, TaskHandler, TaskType}
import tools.Env

import scala.concurrent.duration.{DurationLong, FiniteDuration}

/** Rating refresh: the four `*Ratings` sources, the queue handlers that run
 *  them per row, the reaper that is the SOLE enqueue path, and the OMDb
 *  identifier backfill that is off unless its key is set. */
trait RatingsWiring { self: WorkerWiring =>

  // The *Ratings classes refresh synchronously (the queue's RatingHandler / the
  // operator bulk walk), so they own no EC — only imdbIdResolver still runs
  // async off the bus and draws a shared-budget EC.
  // Each bulk walk feeds its displayed-value changes into the SAME tmdbId-keyed
  // adaptive cadence the per-row RatingHandler feeds (BulkCadenceRecorder), so an
  // operator's corpus refresh can't move a rating without the cadence seeing it —
  // which a later per-row refresh would otherwise mis-read as a fresh change.
  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient, BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.ImdbRating),
    enrichmentLanguage = country.language)
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient, rtLinkCache,
    cadenceRecorder = BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.RtRating))
  lazy val metascoreRatings = new MetascoreRatings(movieCache, tmdbClient, metacriticClient, mcLinkCache,
    cadenceRecorder = BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.McRating))
  lazy val filmwebRatings = new FilmwebRatings(movieCache, tmdbClient, filmwebClient, filmwebLinkCache,
    onImdbIdMissing = (title, year, searchTitle) => eventBus.publish(ImdbIdMissing(title, year, searchTitle)),
    cadenceRecorder = BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.FilmwebRating))
  // OMDb IDENTIFIER backfill — feature-gated by the OMDB_API_KEY secret. `Some`
  // only when the key is set, so nothing references the OMDb path on the default
  // (key-absent) deployment and the feature is completely inert. When present it
  // recovers a row's MISSING `imdbId` (by title+year search) and
  // `rottenTomatoesUrl` (OMDb tomatoURL) — never a rating value. The canonical
  // ImdbRatings / RottenTomatoesRatings then fetch the scores FROM those ids/links
  // on their next EnrichmentReaper tick, keeping one canonical writer per value.
  //
  // Kept OFF the always-on queue: a dedicated TaskType/FreshnessKind would ripple
  // through ~41 exhaustive matches + the queue/metrics codecs, and OMDb is a
  // cheap one-shot gap-filler, not a recurring per-row refresh. Drive a full
  // backfill with the `scripts.OmdbBackfillRun` runMain (calls `refreshAllNow()`
  // here); re-runnable/schedulable, `orElse` write-back never overrides.
  // No cadence recorder: recording OMDb's id/url writes under another source's
  // key would corrupt that source's change history; the default no-op is correct.
  lazy val omdbAttemptStore: OmdbAttemptStore = new MongoOmdbAttemptStore(mongoConnection.database)
  lazy val omdbBackfill: Option[OmdbBackfill] =
    Env.get("OMDB_API_KEY").map(_ => new OmdbBackfill(movieCache, omdbClient, omdbAttemptStore))

  // OMDb identifier backfill runs as a coarse worker TASK (TaskType.RefreshAllOmdb,
  // handled by the BulkRefreshHandler in OperatorWiring). This reaper is just the
  // daily, cluster-claimed ENQUEUER: it puts ONE sweep task on the queue per window
  // (the constant `bulkDedup` key collapses any overlap), so the work runs on the
  // TaskWorker with the rest of the pipeline's metrics/retries — not on a private
  // scheduler thread. Only when the feature is on (`omdbBackfill` is `Some`).
  def omdbBackfillIntervalSeconds: FiniteDuration =
    Env.positiveLong("KINOWO_OMDB_BACKFILL_INTERVAL_SECONDS", OmdbBackfillReaper.DefaultInterval.toSeconds).seconds
  lazy val omdbBackfillReaper: Option[OmdbBackfillReaper] =
    omdbBackfill.map(_ => new OmdbBackfillReaper(
      () => { taskQueue.enqueue(TaskType.RefreshAllOmdb, EnrichTaskKeys.bulkDedup(TaskType.RefreshAllOmdb)); () },
      interval = omdbBackfillIntervalSeconds, runStore = scheduledRunStore))

  // Rating refresh as queue tasks. The handlers reuse each *Ratings class's
  // per-row refreshOneSync; the EnrichmentReaper is the SOLE enqueue path — it
  // refreshes each row once per 4h, phase-spread across frequent ticks and capped
  // per tick. A freshly-resolved film's first ratings come from the reaper's
  // due-immediately first pass (within a tick, bounded by the cap), NOT from an
  // instant per-resolution-event burst, so a cohort of resolutions can't fan out
  // into a rating-task spike (the midday `kinowo_worker_tasks` peaks).
  // ONE shared due schedule (`ratingDueWindow`, an eager member of the root) backs
  // both the reaper (enqueue) and every handler (pickup re-gate), so they agree on
  // what's due — see [[services.tasks.DueWindow]].
  // Its period is the rating TTL (4h, `Freshness.ttlFor`).
  // Per-(source, film) change history → adaptive refresh interval. The rating
  // DueWindow resolves each key's period from its cadence stats instead of a flat
  // 4h, so a film whose displayed value hasn't moved backs off toward 4 days while
  // a fresh/volatile one stays at the 2h base. The reaper + handler share this one
  // instance (their due definitions must agree — see DueWindow).
  lazy val ratingCadenceStore: RatingCadenceStore =
    new MongoRatingCadenceStore(mongoConnection.database)
  // Last-attempt-per-(source, film) log, read by the web app's /debug expand
  // section. Write-only here; a failed write is swallowed, since observing an
  // enrichment must never break it.
  lazy val enrichmentAttemptStore: EnrichmentAttemptStore =
    new MongoEnrichmentAttemptStore(mongoConnection.database)
  // The Filmweb rating handler is wired only for a Filmweb-enabled country (its
  // TaskType is otherwise never enqueued — the EnrichmentReaper is the sole
  // enqueue path and there's no Filmweb source to move a value).
  lazy val ratingHandlers: Seq[TaskHandler] = Seq(
    new RatingHandler(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    freshnessStore, ratingDueWindow, ratingCadenceStore, imdbRatings.refreshOneSync,         metrics = taskMetrics, attempts = enrichmentAttemptStore),
    new RatingHandler(TaskType.RtRating,      FreshnessKind.RtRating,      freshnessStore, ratingDueWindow, ratingCadenceStore, rottenTomatoesRatings.refreshOneSync, metrics = taskMetrics, attempts = enrichmentAttemptStore),
    new RatingHandler(TaskType.McRating,      FreshnessKind.McRating,      freshnessStore, ratingDueWindow, ratingCadenceStore, metascoreRatings.refreshOneSync,    metrics = taskMetrics, attempts = enrichmentAttemptStore)
  ) ++ Option.when(filmwebEnabled)(
    new RatingHandler(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, freshnessStore, ratingDueWindow, ratingCadenceStore, filmwebRatings.refreshOneSync,      metrics = taskMetrics, attempts = enrichmentAttemptStore))
  // Cap on rating-refresh tasks the EnrichmentReaper enqueues per tick. The phase
  // spread keeps steady-state ticks small (~N·tickInterval/period per source ≈ a
  // handful across all four at the 1min cadence), so this only bites a cold/long-down
  // corpus where every row is due at once — bounding that recovery burst, the same
  // lever as the scrape reaper. Set comfortably above the steady-state so normal
  // operation is never throttled; the leftover stays due and drains over the next ticks.
  def maxEnrichmentEnqueuePerTick: Int = Env.positiveLong("KINOWO_ENRICHMENT_MAX_ENQUEUE_PER_TICK", 250L).toInt
  // How often the reaper wakes to enqueue the now-due slice (the spread granularity).
  // Finer = flatter per-minute rating trickle on the `kinowo_worker_tasks` panel,
  // at the cost of cheap in-memory corpus scans. Default 1min (≈240 ticks per 4h).
  def enrichmentTickInterval: FiniteDuration =
    Env.positiveLong("KINOWO_ENRICHMENT_TICK_INTERVAL_SECONDS", EnrichmentReaper.DefaultTickInterval.toSeconds).seconds
  // The per-row rating-enqueue decision, shared by the reaper's corpus walk and the
  // newcomer-fold kick (`MovieService.announceResolvedNewMovie`) so the two agree on
  // eligibility + the tmdbId-keyed due gate. ONE instance, handed to both.
  lazy val ratingEnqueuer = new RatingEnqueuer(taskQueue, freshnessStore, ratingDueWindow, country)
  lazy val enrichmentReaper = new EnrichmentReaper(movieCache, taskQueue, freshnessStore,
    dueWindow = ratingDueWindow, tickInterval = enrichmentTickInterval,
    maxEnqueuePerTick = maxEnrichmentEnqueuePerTick,
    runStore = scheduledRunStore, enqueuer = Some(ratingEnqueuer))
}
