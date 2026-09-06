package modules.wiring

import modules.WorkerWiring
import services.cinemas.common.DetailEnricher
import services.tasks.{DetailReaper, DetailTaskEnqueuer, EnrichDetailsHandler}
import tools.Env

import scala.concurrent.duration.{DurationLong, FiniteDuration}

/** Deferred per-film detail: the cinemas that scrape BARE and fill their detail
 *  through EnrichDetails queue tasks, the handler that fetches it, and the
 *  event-driven enqueuers + periodic reaper that put those tasks on the queue. */
trait DetailWiring { self: WorkerWiring =>

  // Cinemas that defer their per-film detail (implement DetailEnricher) scrape
  // BARE; their detail is filled via EnrichDetails queue tasks. Indexed by
  // detailGroup for the handler (task → fetch); the per-cinema enqueuers and
  // reaper iterate the list directly.
  lazy val detailEnrichers: Seq[DetailEnricher] =
    cinemaScraperCatalog.all.collect { case de: DetailEnricher => de }

  /** Cinemas that defer per-film detail AND whose detail supplies TMDB hints —
   *  a film one of these scrapes (with a detail filmUrl) waits for its
   *  EnrichDetails task before TMDB resolution. This set gates the DIRECT scrape
   *  path only; staging waits for every detail cinema (`StagingSteps`). A display-only enricher
   *  (`defersTmdbResolution = false`, e.g. KinoMuza) still rides the
   *  EnrichDetails pipeline but isn't held back: it resolves from the listing
   *  and merges its synopsis/poster/trailer in asynchronously. */
  lazy val deferredDetailCinemas: Set[models.Cinema] =
    detailEnrichers.filter(_.defersTmdbResolution).map(_.cinema).toSet

  // The shared detail refresh schedule is `detailDueWindow`, an eager member of the
  // root: the SAME instance backs the reaper (enqueue gate) and the handler (pickup
  // gate) so they agree on "due" — see [[services.tasks.DueWindow]].
  lazy val enrichDetailsHandler = new EnrichDetailsHandler(
    detailEnrichers.map(de => de.detailGroup -> de).toMap, movieCache,
    freshnessStore, uptimeMonitor, eventBus, detailDueWindow,
    screeningTokens = screeningTokens
  )
  // Detail enqueue is event-driven: one enqueuer per deferred cinema fires the
  // first detail fetch off CinemaMovieAdded; the reaper is the periodic
  // refresh/retry backstop (CinemaMovieAdded fires only on first appearance),
  // phase-spread + capped so a re-key cohort trickles instead of dumping (~1k
  // EnrichDetails in one tick, which cascaded into the ResolveTmdb/rating bursts
  // that pinned the shared-CPU credit). Same lever as the scrape/rating reapers.
  lazy val detailEnqueuers: Seq[DetailTaskEnqueuer] =
    detailEnrichers.map(de => new DetailTaskEnqueuer(de, movieCache, taskQueue, freshnessStore))
  def maxDetailEnqueuePerTick: Int = Env.positiveLong("KINOWO_DETAIL_MAX_ENQUEUE_PER_TICK", 50L).toInt
  // How often the detail reaper wakes to enqueue the now-due slice (the spread
  // granularity). Finer = flatter per-minute `EnrichDetails` trickle on the
  // `kinowo_worker_tasks` panel, at the cost of cheap in-memory corpus scans.
  // Default 1min (≈360 ticks per 6h).
  def detailTickInterval: FiniteDuration =
    Env.positiveLong("KINOWO_DETAIL_TICK_INTERVAL_SECONDS", DetailReaper.DefaultTickInterval.toSeconds).seconds
  lazy val detailReaper = new DetailReaper(detailEnrichers, movieCache, taskQueue, freshnessStore, eventBus,
    dueWindow = detailDueWindow, tickInterval = detailTickInterval, maxEnqueuePerTick = maxDetailEnqueuePerTick,
    runStore = scheduledRunStore)
}
