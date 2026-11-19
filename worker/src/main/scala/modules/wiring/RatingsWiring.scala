package modules.wiring

import settings.{EnrichmentMaxEnqueuePerTick, EnrichmentTickInterval, OmdbBackfillInterval}

import services.attempts.{EnrichmentAttemptStore, MongoEnrichmentAttemptStore}
import services.cadence.{MongoRatingCadenceStore, RatingCadenceStore}
import services.enrichment.{FilmwebRatings, ImdbRatings, ImdbRowUpdates, MetascoreRatings, MongoOmdbAttemptStore, OmdbAttemptStore, OmdbBackfill, OmdbBackoff, OmdbSearch, RatingColumns, RatingSiteFilms, RottenTomatoesRatings, SiteRatings}
import services.events.ImdbIdMissing
import services.freshness.FreshnessKind
import services.tasks.{BulkCadenceRecorder, ClaimedEnqueueReaper, EnrichTaskKeys, EnrichmentReaper, OmdbBackfillReaper, RatingEnqueuer, RatingHandler, TaskHandler}
import services.tasktypes.TaskType

import scala.concurrent.duration.DurationInt


/** Rating refresh: the four rating sources (IMDb's `ImdbRatings`, each URL-keyed site's port run by
 *  `SiteRatings`), the queue handlers that run them per row, the reaper that is the SOLE enqueue path, and the OMDb
 *  identifier backfill that is off unless its key is set. */
trait RatingsWiring { self: WorkerWiring =>

  // The rating refreshers refresh synchronously (the queue's RatingHandler / the
  // operator bulk walk), so they own no EC — only imdbIdResolver still runs
  // async off the bus and draws a shared-budget EC.
  // Each bulk walk feeds its displayed-value changes into the SAME tmdbId-keyed
  // adaptive cadence the per-row RatingHandler feeds (BulkCadenceRecorder), so an
  // operator's corpus refresh can't move a rating without the cadence seeing it —
  // which a later per-row refresh would otherwise mis-read as a fresh change.
  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient, new ImdbRowUpdates(country.language),
    BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.ImdbRating, clock))
  // Every URL-keyed rating site is its port (`*Ratings`: find, read) and its row columns, run by the one shared flow
  // (SiteRatings); every site searches and judges pages by the same film, the row and what TMDB knows of it.
  lazy val ratingSiteFilms = new RatingSiteFilms(tmdbClient, movieCache.normalizer)
  lazy val rottenTomatoesRatings = new SiteRatings(movieCache,
    new RottenTomatoesRatings(ratingSiteFilms, rottenTomatoesClient, movieCache.normalizer, rtLinkCache), RatingColumns.RottenTomatoes,
    stopwatch, BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.RtRating, clock))
  lazy val metascoreRatings = new SiteRatings(movieCache,
    new MetascoreRatings(ratingSiteFilms, metacriticClient, movieCache.normalizer, mcLinkCache), RatingColumns.Metacritic,
    stopwatch, BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.McRating, clock))
  lazy val filmwebRatings = new SiteRatings(movieCache,
    new FilmwebRatings(ratingSiteFilms, filmwebClient, movieCache.normalizer, filmwebLinkCache,
      onImdbIdMissing = (title, year, searchTitle) => eventBus.publish(ImdbIdMissing(title, year, searchTitle))), RatingColumns.Filmweb,
    stopwatch, BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.FilmwebRating, clock))
  // OMDb IDENTIFIER backfill — feature-gated by the OMDB_API_KEY secret. `Some`
  // only when the key is set, so nothing references the OMDb path on the default
  // (key-absent) deployment and the feature is completely inert. When present it
  // recovers a row's MISSING `imdbId` (by title+year search) — never a rating
  // value, never a rating site's link (RottenTomatoesRatings owns
  // `rottenTomatoesUrl`). ImdbRatings then fetches the score FROM the id on its
  // next EnrichmentReaper tick, keeping one canonical writer per value.
  //
  // Kept OFF the always-on queue: a dedicated TaskType/FreshnessKind would ripple
  // through ~41 exhaustive matches + the queue/metrics codecs, and OMDb is a
  // cheap gap-filler, not a recurring per-row refresh; the daily sweep below
  // drives it, and its `orElse` write-back never overrides.
  // No cadence recorder: recording OMDb's id writes under another source's
  // key would corrupt that source's change history; the default no-op is correct.
  lazy val omdbAttemptStore: OmdbAttemptStore = new MongoOmdbAttemptStore(mongoConnection.database)
  lazy val omdbBackfill: Option[OmdbBackfill] =
    configuration.omdbApiKey.map(_ => new OmdbBackfill(movieCache, new OmdbSearch(omdbClient, movieCache.normalizer),
      new OmdbBackoff(omdbAttemptStore, clock)))

  // OMDb identifier backfill runs as a coarse worker TASK (TaskType.RefreshAllOmdb,
  // handled by the BulkRefreshHandler in OperatorWiring). This reaper is just the
  // daily, cluster-claimed ENQUEUER: it puts ONE sweep task on the queue per window
  // (the constant `bulkDedup` key collapses any overlap), so the work runs on the
  // TaskWorker with the rest of the pipeline's metrics/retries — not on a private
  // scheduler thread. Only when the feature is on (`omdbBackfill` is `Some`).
  def omdbBackfillInterval: OmdbBackfillInterval =
    configuration.omdbBackfillInterval(OmdbBackfillInterval(OmdbBackfillReaper.DefaultInterval))
  lazy val omdbBackfillReaper: Option[OmdbBackfillReaper] = managedResources.stoppingEach(
    omdbBackfill.map(_ => new OmdbBackfillReaper(
      () => taskQueue.enqueued(TaskType.RefreshAllOmdb, EnrichTaskKeys.bulkDedup(TaskType.RefreshAllOmdb), submittedAt = clock.instant()),
      interval = omdbBackfillInterval, runStore = scheduledRunStore, clock = clock, ticks = ticks, threadPools = threadPools)))

  // Rating refresh as queue tasks. The handlers reuse each rating refresher's
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
    new RatingHandler(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    freshnessStore, ratingDueWindow, ratingCadenceStore, imdbRatings.refreshOneSync,         clock = clock, metrics = taskPipelineMetrics, attempts = enrichmentAttemptStore),
    new RatingHandler(TaskType.RtRating,      FreshnessKind.RtRating,      freshnessStore, ratingDueWindow, ratingCadenceStore, rottenTomatoesRatings.refreshOneSync, clock = clock, metrics = taskPipelineMetrics, attempts = enrichmentAttemptStore),
    new RatingHandler(TaskType.McRating,      FreshnessKind.McRating,      freshnessStore, ratingDueWindow, ratingCadenceStore, metascoreRatings.refreshOneSync,    clock = clock, metrics = taskPipelineMetrics, attempts = enrichmentAttemptStore)
  ) ++ Option.when(filmwebEnabled)(
    new RatingHandler(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, freshnessStore, ratingDueWindow, ratingCadenceStore, filmwebRatings.refreshOneSync,      clock = clock, metrics = taskPipelineMetrics, attempts = enrichmentAttemptStore))
  // Cap on rating-refresh tasks the EnrichmentReaper enqueues per tick. The phase
  // spread keeps steady-state ticks small (~N·tickInterval/period per source ≈ a
  // handful across all four at the 1min cadence), so this only bites a cold/long-down
  // corpus where every row is due at once — bounding that recovery burst, the same
  // lever as the scrape reaper. Set comfortably above the steady-state so normal
  // operation is never throttled; the leftover stays due and drains over the next ticks.
  def maxEnrichmentEnqueuePerTick: EnrichmentMaxEnqueuePerTick =
    configuration.enrichmentMaxEnqueuePerTick(EnrichmentMaxEnqueuePerTick(250))
  // How often the reaper wakes to enqueue the now-due slice (the spread granularity).
  // Finer = flatter per-minute rating trickle on the `kinowo_worker_tasks` panel,
  // at the cost of cheap in-memory corpus scans. Default 1min (≈240 ticks per 4h).
  def enrichmentTickInterval: EnrichmentTickInterval =
    configuration.enrichmentTickInterval(EnrichmentTickInterval(EnrichmentReaper.DefaultTickInterval))
  // The per-row rating-enqueue decision, shared by the reaper's corpus walk and the
  // newcomer-fold kick (`MovieService.announceResolvedNewMovie`) so the two agree on
  // eligibility + the tmdbId-keyed due gate. ONE instance, handed to both.
  lazy val ratingEnqueuer = new RatingEnqueuer(taskQueue, freshnessStore, ratingDueWindow, country)
  lazy val enrichmentReaper = new EnrichmentReaper(movieCache, freshnessStore, ratingEnqueuer,
    maxEnqueuePerTick = maxEnrichmentEnqueuePerTick, clock = clock)
  // Its schedule: a tick every `enrichmentTickInterval`, one machine a window, held until the rating stamps hydrated.
  lazy val enrichmentReaperSchedule = managedResources.stopping(new ClaimedEnqueueReaper(EnrichmentReaper.Occurrence,
    () => enrichmentReaper.tick(), enrichmentTickInterval.value, initialDelay = 0.seconds, scheduledRunStore, clock,
    ticks = ticks, threadPools = threadPools, ready = () => enrichmentReaper.ready, recordedAs = Some(EnrichmentReaper.Tick)))
}
