package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.freshness.FreshnessKind
import services.movies.{CacheSyncMetrics, RepositoryWriteMetrics, ResolveDuplicateMetrics, ChangeStreamLiveness, ChangeStreamMetrics, MergeMetrics, MergeReason, RekeyReason, ScrapeLandingMetrics, ScreeningsMetrics, SideCollectionChangeMetrics, SplitMetrics}
import services.readmodel.ReadModelProjectionMetrics
import services.staging.{StagingMetrics, StagingStep}
import services.tasks.{QueueSnapshot, RatingLatencyMetrics, ResolveMode, Task, TaskState, TaskType}

import java.time.Instant

/** Lifecycle hook the [[services.tasks.TaskWorker]] calls so instrumentation
 *  stays out of the worker's hot path. Segregated to the two moments the worker
 *  knows about — a claim, and the outcome of running the handler — so a metrics
 *  sink (or a test spy) implements just these, not the whole queue contract. */
trait TaskObserver {
  /** A worker claimed `task` and is about to run its handler. */
  def onStarted(task: Task): Unit
  /** The handler returned: `outcome` is one of [[WorkerTaskMetrics.Outcomes]];
   *  `handleMillis` is the handler's wall-clock (0 for `no_handler`, which never
   *  ran one). */
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit
}

object TaskObserver {
  val NoOp: TaskObserver = new TaskObserver {
    def onStarted(task: Task): Unit                                       = ()
    def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = ()
  }

  /** Fan one worker's lifecycle callbacks out to several observers, so the
   *  [[TaskWorker]] still takes a single observer while every sink sees every
   *  task. */
  def composite(observers: TaskObserver*): TaskObserver = new TaskObserver {
    def onStarted(task: Task): Unit                                       = observers.foreach(_.onStarted(task))
    def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = observers.foreach(_.onFinished(task, outcome, handleMillis))
  }
}

/**
 * Prometheus instrumentation for the worker's task pipeline — the worker-side
 * counterpart to the web app's uptime `/metrics` (controllers.MetricsController).
 *
 * It counts the full lifecycle of every deferred task (enqueue → claim → finish)
 * keyed by `TaskType`, times the work, and (refreshed from a live queue snapshot
 * at scrape time) exposes the current backlog. Built on the official Prometheus
 * Java client rather than a hand-rolled exposition; the fleet's Prometheus
 * scrapes it over the pod's NodePort, and Grafana charts the `rate(...)` of each
 * family per `task_type`.
 *
 * Multi-country: ONE JVM runs a [[modules.WorkerWiring]] per country against a
 * single shared [[WorkerTaskMetrics.Series]] (registered once on the shared
 * registry). Every series carries a `country` label so the two countries' rows
 * never collide on the single `/metrics` endpoint. This class is the cheap
 * PER-COUNTRY facade a wiring holds: it binds one `countryCode` and forwards every
 * record to the shared `Series`, so all existing call sites (which see the narrow
 * `TaskObserver` / `MergeMetrics` / … traits) stay unchanged.
 *
 * Counters are monotonic since-boot totals — `rate()` handles the reset on each
 * worker reboot. Every `TaskType` (× every label value × every country) is
 * materialized to 0 at construction so its series exists from boot and Grafana
 * draws a continuous line rather than a gap for a type/outcome that hasn't
 * happened yet.
 *
 * Wiring: [[MeteredTaskQueue]] feeds `recordEnqueue`; [[services.tasks.TaskWorker]]
 * feeds `onStarted`/`onFinished` (this class is its `TaskObserver`); the queue
 * gauges are refreshed from a per-country `QueueSnapshot` each `Series.scrape()`.
 */
class WorkerTaskMetrics(countryCode: String, series: WorkerTaskMetrics.Series)
  extends TaskObserver with MergeMetrics with SplitMetrics with ReadModelProjectionMetrics with RatingLatencyMetrics with ScreeningsMetrics with CacheSyncMetrics with ScrapeLandingMetrics with ResolveDuplicateMetrics with StagingMetrics with RepositoryWriteMetrics with services.readmodel.DecodeFailureMetrics {

  // ── RatingLatencyMetrics ────────────────────────────────────────────────────
  def recordFirstRatingDelay(site: String, seconds: Double): Unit = series.recordFirstRatingDelay(countryCode, site, seconds)

  // ── MergeMetrics / SplitMetrics ─────────────────────────────────────────────
  def recordMerge(reason: MergeReason, victims: Int): Unit = series.recordMerge(countryCode, reason, victims)
  override def recordRekey(reason: RekeyReason): Unit      = series.recordRekey(countryCode, reason)
  def recordSplit(fragments: Int): Unit                    = series.recordSplit(countryCode, fragments)

  // ── ReadModelProjectionMetrics ──────────────────────────────────────────────
  def recordWrite(target: String, op: String, count: Int): Unit = series.recordWrite(countryCode, target, op, count)
  def recordFilmPruned(reason: String, count: Int): Unit        = series.recordFilmPruned(countryCode, reason, count)
  def recordCardRetired(reason: String): Unit                   = series.recordCardRetired(countryCode, reason)
  def recordDriftWrites(documents: Int): Unit                   = series.recordDriftWrites(countryCode, documents)
  def recordCatchUp(rows: Int): Unit                            = series.recordCatchUp(countryCode, rows)
  def recordCardWrite(changed: Set[String]): Unit               = series.recordCardWrite(countryCode, changed)
  def recordProject(trigger: ReadModelProjectionMetrics.ProjectTrigger, wallSeconds: Double, cpuSeconds: Double): Unit =
    series.recordProject(countryCode, trigger, wallSeconds, cpuSeconds)
  def recordWriteBurst(seconds: Double): Unit                    = series.recordWriteBurst(countryCode, seconds)
  def recordMetadataProjection(reused: Boolean): Unit           = series.recordMetadataProjection(countryCode, reused)
  def recordVenueProjection(rebuilt: Int, reused: Int): Unit     = series.recordVenueProjection(countryCode, rebuilt, reused)
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit = series.recordReconcileSweep(countryCode, kind, didWork)
  def recordHeal(trigger: String, rows: Int): Unit              = series.recordHeal(countryCode, trigger, rows)

  // ── StagingMetrics ──────────────────────────────────────────────────────────
  def recordNewcomerKick(groupRows: Int): Unit = series.recordStagingNewcomerKick(countryCode, groupRows)

  // ── CacheSyncMetrics ────────────────────────────────────────────────────────
  def recordRehydrate(changedUpserts: Int, deletes: Int): Unit = series.recordRehydrate(countryCode, changedUpserts, deletes)

  // ── ResolveDuplicateMetrics ─────────────────────────────────────────────────
  def recordDuplicate(mode: ResolveMode, upgraded: Boolean): Unit = series.recordResolveDuplicate(countryCode, mode, upgraded)

  // ── ScrapeLandingMetrics ────────────────────────────────────────────────────
  def recordGuardVerdict(guard: String, verdict: String): Unit = series.recordScrapeGuardVerdict(countryCode, guard, verdict)
  def recordWriteSkipped(reason: String): Unit                 = series.recordScrapeWriteSkipped(countryCode, reason)

  // ── RepositoryWriteMetrics ──────────────────────────────────────────────────
  def recordWriteFailed(collection: String, op: String, exception: String): Unit =
    series.recordRepositoryWriteFailed(countryCode, collection, op, exception)

  // ── DecodeFailureMetrics ────────────────────────────────────────────────────
  def recordDecodeFailure(collection: String): Unit = series.recordDecodeFailure(countryCode, collection)

  // ── ChangeStreamMetrics for `movies` ────────────────────────────────────────
  // A separate object rather than a direct mixin: `ChangeStreamMetrics.recordCoalescedChange`
  // and `ScreeningsMetrics.recordCoalescedChange` erase to the same signature, so one class
  // cannot implement both directly — the same reason `slotsChangeMetrics` below is its own
  // object rather than a third mixin.
  val movieChangeMetrics: ChangeStreamMetrics = new ChangeStreamMetrics {
    def recordEvent(op: String): Unit        = series.recordEvent(countryCode, op)
    def recordUpdateKind(kind: String): Unit = series.recordUpdateKind(countryCode, kind)
    def recordCoalescedChange(): Unit        = series.recordMovieCoalesced(countryCode)
  }

  // ── ScreeningsMetrics ───────────────────────────────────────────────────────
  def recordChangeEvent(op: String): Unit = series.recordScreeningsChangeEvent(countryCode, op)
  def recordWrite(outcome: String, count: Int): Unit = series.recordScreeningsWrite(countryCode, outcome, count)
  def recordCoalescedChange(): Unit = series.recordScreeningsCoalesced(countryCode)

  // ── SideCollectionChangeMetrics for `movie_slots` ───────────────────────────
  // A separate object rather than a third mixin: the slots cursor answers the same two
  // questions as the screenings one, so its trait has the same two methods, and one class
  // cannot route the same signature to two counters.
  val slotsChangeMetrics: SideCollectionChangeMetrics = new SideCollectionChangeMetrics {
    def recordChangeEvent(op: String): Unit = series.recordSlotsChangeEvent(countryCode, op)
    def recordCoalescedChange(): Unit       = series.recordSlotsCoalesced(countryCode)
  }

  // ── Task lifecycle ──────────────────────────────────────────────────────────
  def recordEnqueue(taskType: TaskType, result: String): Unit = series.recordEnqueue(countryCode, taskType, result)
  def onStarted(task: Task): Unit                             = series.onStarted(countryCode, task)
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = series.onFinished(countryCode, task, outcome, handleMillis)
}

object WorkerTaskMetrics {

  /** The per-country queue/staging sample [[Series.scrape]] folds into the gauges
   *  for one country on each scrape. `snapshot` is that country's live queue
   *  monitor, `stagingByStep` its `StagingReaper.stepCounts()` — each None when its read
   *  FAILED, which holds that read's gauges at their last reading — and
   *  `changeStreamLiveness` the repository's record of when each change-stream cursor
   *  last delivered and what it has handed the apply thread that has not been applied yet —
   *  read at scrape time, so a silent cursor's age and a stuck apply's lag keep climbing. */
  case class CountryQueueSample(countryCode: String, snapshot: Option[QueueSnapshot], stagingByStep: Option[Map[StagingStep, Int]],
                                changeStreamLiveness: ChangeStreamLiveness)

  object CountryQueueSample extends play.api.Logging {
    def apply(countryCode: String, snapshot: QueueSnapshot, stagingByStep: Map[StagingStep, Int],
              changeStreamLiveness: ChangeStreamLiveness): CountryQueueSample =
      CountryQueueSample(countryCode, Some(snapshot), Some(stagingByStep), changeStreamLiveness)

    /** Take one country's sample from its live reads, each on its own. Both THROW on a failed
     *  read (an unreadable queue is not an empty one), and the worker renders its whole
     *  exposition — every country, the JVM, every counter — in one pass that keeps the last
     *  good bytes when it throws: a read escaping here froze every series the worker exports
     *  for as long as it kept failing. A failed read holds only its own gauges. */
    def read(countryCode: String, snapshot: => QueueSnapshot, stagingByStep: => Map[StagingStep, Int],
             changeStreamLiveness: ChangeStreamLiveness): CountryQueueSample = {
      def held[A](what: String, read: => A): Option[A] =
        scala.util.Try(read).fold(e => {
          logger.warn(s"metrics: $countryCode $what read failed, holding its gauges at their last reading: ${e.getMessage}"); None
        }, Some(_))
      CountryQueueSample(countryCode, held("queue", snapshot), held("staging", stagingByStep), changeStreamLiveness)
    }
  }

  /**
   * The registered-once metric objects, SHARED across every country's
   * [[WorkerTaskMetrics]] facade. Registered on the single worker registry so
   * ONE `/metrics` scrape exposes every country's task pipeline; each series
   * carries a leading `country` label so the countries never collide. Seeded to 0
   * for the full cartesian of (country × every other label value) so no Grafana
   * gap opens before a given (country, type, outcome) first fires.
   */
  class Series(poolSize: Int, countryCodes: Seq[String], registry: PrometheusRegistry = new PrometheusRegistry()) {

    // The client auto-appends `_total` to counter names, so they're declared without it.
    private val enqueued = Counter.builder()
      .name("kinowo_worker_tasks_enqueued")
      .help("Tasks added to the queue (result=added) or collapsed onto an active duplicate (result=deduped) since boot, by country and type.")
      .labelNames("country", "task_type", "result")
      .register(registry)

    private val started = Counter.builder()
      .name("kinowo_worker_tasks_started")
      .help("Tasks claimed and kicked off since boot, by country and type.")
      .labelNames("country", "task_type")
      .register(registry)

    private val finished = Counter.builder()
      .name("kinowo_worker_tasks_finished")
      .help("Tasks that finished a handler run since boot, by country, type and outcome (done=fully worked, skipped=data already fresh, rescheduled=transient retry, deferred=refused untried by an open circuit, failed=handler threw, no_handler=no wired handler).")
      .labelNames("country", "task_type", "outcome")
      .register(registry)

    private val duration = Histogram.builder()
      .name("kinowo_worker_task_duration_seconds")
      .help("Handler wall-clock of fully-worked (done) tasks, by country and type.")
      .labelNames("country", "task_type")
      .classicUpperBounds(DurationBucketsSeconds*)
      .classicOnly()
      .register(registry)

    private val ratingFirstAttemptDelay = Histogram.builder()
      .name("kinowo_worker_rating_first_attempt_delay_seconds")
      .help("Delay between a film's TMDB resolution and the FIRST attempt to fetch its rating, by country and site (imdb/fw/rt/mc). Measures the EnrichmentReaper's first-pass latency now that ratings aren't enqueued the instant a film resolves.")
      .labelNames("country", "site")
      .classicUpperBounds(RatingDelayBucketsSeconds*)
      .classicOnly()
      .register(registry)

    private val queueDepth = Gauge.builder()
      .name("kinowo_worker_queue_depth")
      .help("Tasks currently in the queue, by country and state.")
      .labelNames("country", "state")
      .register(registry)

    private val waitingByType = Gauge.builder()
      .name("kinowo_worker_queue_waiting_by_type")
      .help("Waiting (claimable) tasks currently in the queue, by country and type. Sampled from the bounded active snapshot.")
      .labelNames("country", "task_type")
      .register(registry)

    private val oldestWaitingAge = Gauge.builder()
      .name("kinowo_worker_queue_oldest_waiting_age_seconds")
      .help("Seconds the oldest CLAIMABLE waiting task per country and type has been claimable (tasks held in retry backoff or not yet due are excluded) — head-of-line latency / starvation signal.")
      .labelNames("country", "task_type")
      .register(registry)

    // The mirror of the head-of-line age: that one counts only CLAIMABLE rows, so a task the
    // queue holds back is invisible to it by design. This is the longest remaining hold among
    // the held-back rows — never more than TaskWorker.MaxBackoff unless something parked a
    // task past the cap (alerted as WorkerTaskParkedTooLong).
    private val parkedMax = Gauge.builder()
      .name("kinowo_worker_queue_parked_max_seconds")
      .help("Longest remaining hold, in seconds, among waiting tasks per country and type that the queue is holding back (retry backoff, a Deferred's instant, a staggered chunk not yet due); 0 when none is held. Bounded by TaskWorker.MaxBackoff (1800s) for every hold the queue sets, so a value above it means a task parked past the cap. Sampled from the bounded active snapshot.")
      .labelNames("country", "task_type")
      .register(registry)

    // The worker pool is a single SharedExecutionBudget across all countries, so
    // this is a process-level gauge with no country label (pairing it per-country
    // would misleadingly imply a per-country pool).
    private val poolSizeGauge = Gauge.builder()
      .name("kinowo_worker_pool_size")
      .help("Configured worker pool size (shared across countries) — pair with queue_depth{state=\"worked_on\"} for utilization.")
      .register(registry)

    private val stagingMovies = Gauge.builder()
      .name("kinowo_worker_staging_movies")
      .help("Incubating films currently in pending_movies, by country and the step each needs next (detail → resolve_tmdb → resolve_imdb → fold). Distinct films (a film's cinema rows count once); sum = total movies in staging.")
      .labelNames("country", "step")
      .register(registry)

    // A GAUGE COMPUTED AT SCRAPE TIME, never a stored age: the value is `now - lastDelivered`
    // on every scrape, so a cursor that stalls draws a straight diagonal instead of freezing
    // at whatever it last said. The event COUNTERS beside it cannot tell a stalled cursor
    // from a quiet night — both are a flat rate — and a dead stream after a Mongo migration
    // once sat behind them for hours. See [[services.movies.ChangeStreamLiveness]].
    private val changeStreamLastEventAge = Gauge.builder()
      .name("kinowo_worker_change_stream_last_event_age_seconds")
      .help("Seconds since each change-stream cursor (collection=movies|screenings|movie_slots) last DELIVERED an event to the worker, by country; since boot when it never has. Recomputed on every scrape, so an open-but-silent cursor climbs in a straight line. Read against the country's scrape cadence: the movies cursor is quiet for at most one sweep while ratings and scrapes are being written; longer than that with kinowo_worker_corpus_movies moving means the stream is not delivering the writes.")
      .labelNames("country", "collection")
      .register(registry)

    // The APPLY side of the same three cursors, also computed at scrape time: an event delivered
    // on time can still wait behind the single apply thread, and until 2026-09-23 the resume token
    // was saved at delivery, so a restart skipped whatever was waiting — with nothing to show how
    // much that was. See [[services.movies.ChangeStreamLiveness.queued]].
    private val changeStreamApplyPending = Gauge.builder()
      .name("kinowo_worker_change_stream_apply_pending")
      .help("Change events each cursor (collection=movies|screenings|movie_slots) handed to the worker's apply thread that have not been applied yet, by country. Bounded by the cursor's demand window (256); near zero in steady state because an apply is one stitch read. Pinned near the window means the apply thread cannot keep up, and every one of these is a change the read model has not seen. Coalesced events ride an apply already queued and are not counted twice.")
      .labelNames("country", "collection")
      .register(registry)

    private val changeStreamApplyLag = Gauge.builder()
      .name("kinowo_worker_change_stream_apply_lag_seconds")
      .help("Seconds the OLDEST not-yet-applied change event of each cursor (collection=movies|screenings|movie_slots) has waited since delivery, by country; 0 when nothing is waiting. Recomputed on every scrape, so an apply thread that is stuck climbs in a straight line. The delivered-vs-applied lag: the read model is at least this stale for that change. Alerted by ChangeStreamApplyLagging (over 10 minutes for 10 minutes).")
      .labelNames("country", "collection")
      .register(registry)

    private val merges = Counter.builder()
      .name("kinowo_worker_merges")
      .help("Movie rows folded into another row since boot (one per victim absorbed; a cluster of N counts N−1), by country and reason — canonicalize=periodic same-film settle/rehydrate fold, resolved-settle=TMDB-resolve year fold, tmdb-identity=runtime same-tmdbId put-gate, normalize-rebuild=title-rule change re-merges rows that now share a key. Each fold orphans the victim's title|year freshness, so rate() is the re-key re-enrichment load.")
      .labelNames("country", "reason")
      .register(registry)

    private val rekeys = Counter.builder()
      .name("kinowo_worker_rekeys")
      .help("Movie rows that stayed the same film but moved to a new title|year key since boot, by country and reason — resolved-year=TMDB concluded a year for a yearless row, canonicalize=the settle re-spelled or re-yeared a lone row, embedded-year=a year a venue wrote into its title promoted a yearless key, forced-reset=the operator's forced re-enrich re-keyed onto the scraped year. A re-key is the one cost a stable film id would remove, so rate() is the measurement that decides that change.")
      .labelNames("country", "reason")
      .register(registry)

    private val splits = Counter.builder()
      .name("kinowo_worker_splits")
      .help("Cinema slots the settle pass re-diverted to staging since boot because their row held a SECOND film (MixedFilmSplitter), by country — the inverse of a merge. Each re-resolves on its own hints, so rate() is the un-merge re-enrichment load; a healthy corpus needs almost none, so a sustained rate means the detector is reading ordinary rows as two films.")
      .labelNames("country")
      .register(registry)

    private val readModelWrites = Counter.builder()
      .name("kinowo_worker_readmodel_writes")
      .help("Denormalised read-model documents (re)written since boot, by country, target (movie|screening) and op (upsert|delete). rate() is the reprojection churn the worker pushes through the web's web_movies/web_screenings change streams.")
      .labelNames("country", "target", "op")
      .register(registry)

    private val readModelFilmsPruned = Counter.builder()
      .name("kinowo_worker_readmodel_films_pruned")
      .help("Cards the read-model PRUNE sweep removed since boot, by country and reason: row-gone = no movies document projects to that id any more; variant-gone = the row lives but no longer projects to that variant id (a decorated listing that vanished). The rule (2026-09-07) is that this stays at ZERO — the change-stream path retires every card it no longer produces (see readmodel_cards_retired) — so a sustained rate is a projection defect to chase by its reason, never accepted churn. Pair with kinowo_worker_merges_total for a row-gone cause.")
      .labelNames("country", "reason")
      .register(registry)

    private val readModelCardsRetired = Counter.builder()
      .name("kinowo_worker_readmodel_cards_retired")
      .help("Cards the read-model CHANGE-STREAM path retired on its own since boot, by country and reason: variant-gone = a re-projected row no longer produces that variant card (its decorated listing vanished); row-deleted = the row was deleted or merged away (a delete event); row-unready = a re-projected row lost its readiness. These are the removals the prune used to do 30 minutes late; the prune counter is what is LEFT after them.")
      .labelNames("country", "reason")
      .register(registry)

    private val readModelCatchUpRows = Counter.builder()
      .name("kinowo_worker_readmodel_catchup_rows")
      .help("Source rows the read-model prune sweep re-projected because they were written AFTER the movies change-stream cursor's last delivered event, by country, since boot. The catch-up for a cursor that is open and silent: a live cursor delivers a write within seconds, so this stays at zero; a sustained rate is the read-model side of ChangeStreamMoviesCursorSilent and means the site was up to 30 minutes stale between sweeps.")
      .labelNames("country")
      .register(registry)

    private val readModelHeals = Counter.builder()
      .name("kinowo_worker_readmodel_heals")
      .help("Ready source rows re-projected because the read model lacked one of their cards or venues, by country and trigger (sweep=the 30-min prune sweep, boot=the check start() runs). Each is a row the change-stream path should already have written — a card it retired and never restored, a slot event it missed — so ZERO is the healthy reading and a recurring rate is a stream-path defect: on 2026-09-22 a TMDB re-try made rows briefly unready (readmodel_cards_retired{reason=row-unready} ~500/day) and only these heals, ~26 a day and 30 minutes late each, put the cards back. Alerted by ReadModelHealsRecurring.")
      .labelNames("country", "trigger")
      .register(registry)

    private val readModelCardWrites = Counter.builder()
      .name("kinowo_worker_readmodel_card_writes")
      .help("web_movies card documents written since boot, by country and cause. new=no card existed; one of title|poster|facts|synopsis|synopsis-by-city|ratings|trailers|age-rating=exactly that part of the card moved (facts = runtime, year, genres, countries, directors, cast); multiple=more than one part moved (see readmodel_card_rewrite_parts for which). The synopsis-by-city line answers whether moving that map (44% of card bytes in the fixture read model) to its own collection would spare the read model any card rewrites: near zero means the split saves bytes per document but not writes.")
      .labelNames("country", "cause")
      .register(registry)

    private val readModelCardRewriteParts = Counter.builder()
      .name("kinowo_worker_readmodel_card_rewrite_parts")
      .help("Card parts that differed from the card written before, one increment per part per rewrite, by country and part — the decomposition of readmodel_card_writes{cause=multiple}. A rewrite that moved ratings and the poster counts once under each.")
      .labelNames("country", "part")
      .register(registry)

    private val readModelDriftWrites = Counter.builder()
      .name("kinowo_worker_readmodel_drift_writes")
      .help("Read-model documents the rolling CONTENT check rewrote since boot, by country. Every other sweep compares IDS — the prune removes a card whose row is gone, the heal writes one that is missing — and none of them can see a row that exists and is WRONG: three UK films held showtimes from August until 2026-09-08, served to users and unreachable by the change stream because their source had stopped changing. One slice of the corpus is re-projected per prune sweep (one of 48 slices, so the whole corpus once a day, plus a whole-corpus pass when the projection's derivation version changes) and the projection's own diff writes only what drifted. Zero is the healthy reading; a sustained rate means the incremental path is losing writes.")
      .labelNames("country")
      .register(registry)

    private val readModelProjectDuration = Histogram.builder()
      .name("kinowo_worker_readmodel_project_duration_seconds")
      .help("Wall-clock of one pure ReadModelProjection.projectAll per source row since boot, by country — the LATENCY signal (percentiles, the duration heatmap). NOT a CPU share: concurrent projections make rate(_sum) exceed one core-second per second, and steal on a throttled box inflates it further. Use kinowo_worker_readmodel_project_cpu_seconds_total for CPU attribution.")
      .labelNames("country")
      .classicUpperBounds(ProjectBucketsSeconds*)
      .classicOnly()
      .register(registry)

    private val readModelProjectCpu = Counter.builder()
      .name("kinowo_worker_readmodel_project_cpu_seconds")
      .help("Thread CPU seconds burned inside pure ReadModelProjection.projectAll since boot, by country. rate()*100 is projection's true share of worker CPU in centi-cores — the credit-floor driver the CPU-drivers dashboard stacks against JIT, GC and everything else. Unlike the wall-clock histogram this is immune to concurrency and to steal, so it can be compared against process_cpu_seconds_total.")
      .labelNames("country")
      .register(registry)

    private val readModelWriteBurst = Histogram.builder()
      .name("kinowo_worker_readmodel_write_burst_seconds")
      .help("Wall-clock of the WRITE half of one project() call since boot, by country — writer.upsertMovie/diffScreenings for a single source row, once its variants are computed. Separate from readmodel_project_duration_seconds, which times only the computation before any write. On 2026-09-08 a wide UK release took up to ~40 minutes to reach all 73 of its cities after its source row turned ready, with nothing distinguishing computation, writing, or change-stream delivery as the slow part — this histogram is the write-phase half of that answer; a long write burst against a short project_duration for the same call points at the write loop (city count, Mongo round-trips), not at resolve/synopsisByCity/ratings.")
      .labelNames("country")
      .classicUpperBounds(DurationBucketsSeconds*)
      .classicOnly()
      .register(registry)

    private val readModelProjectCalls = Counter.builder()
      .name("kinowo_worker_readmodel_project_calls")
      .help("Source rows projected (projectAll invoked) since boot, by country and trigger — the throughput denominator for readmodel_project_duration_seconds. trigger=stream is a change-stream event (any of the three cursors) and the only share ReadModelProjectionTriggerUnaccounted compares against the cursors' events; heal|catch-up|content|derivation|share-card|hold-release|reproject re-project rows no event asked for, by design. derivation is the whole corpus once per worker after a new derivation (ReadModelDerivation.History — a change to what the projection derives, cards-only or full); until 2026-09-26 every read-model snapshot regeneration counted, and five such deploys inside an hour held DE/UK/PL at 1-2/s over their events for an hour.")
      .labelNames("country", "trigger")
      .register(registry)

    private val readModelMetadataProjections = Counter.builder()
      .name("kinowo_worker_readmodel_metadata_projections")
      .help("Projections by country and whether the metadata half (resolve/synopsisByCity/ratingsFor) was REUSED from the per-film cache (outcome=reused — a showtime-only change at an already-present cinema, only the cheap screenings half re-ran) or RECOMPUTED (outcome=recomputed — a rating/synopsis/new-cinema change, or a first projection). rate(reused) / rate(reused+recomputed) is opt-1's hit ratio — high reuse under reproject/enrich showtime churn is the CPU win.")
      .labelNames("country", "outcome")
      .register(registry)

    private val readModelVenueProjections = Counter.builder()
      .name("kinowo_worker_readmodel_venue_projections")
      .help("Venues' screenings rows a projection considered, by country and whether the row was REBUILT (outcome=rebuilt — the venue's slots moved since the row was written, or this process never wrote it) or kept unbuilt (outcome=reused). A showtime change at one venue of a wide release is one rebuilt against thousands reused.")
      .labelNames("country", "outcome")
      .register(registry)

    private val stagingNewcomerKicks = Counter.builder()
      .name("kinowo_worker_staging_newcomer_kicks")
      .help("StagingNewcomerDiverted events the StagingReaper handled since boot, by country — each one decodes the film's whole staging group to pick its next step. The denominator for kinowo_worker_staging_newcomer_kick_rows_total.")
      .labelNames("country")
      .register(registry)

    private val stagingNewcomerKickRows = Counter.builder()
      .name("kinowo_worker_staging_newcomer_kick_rows")
      .help("Staging rows decoded by newcomer kicks since boot, by country. A kick is due only for a film NEW to staging, so rows/kick sits near 1; a kick per venue JOINING an incubating film (the pre-2026-09-23 shape) makes the k-th of N venues decode k rows, rows/kick ~N/2 and the scrape tick quadratic in a wide film's venues.")
      .labelNames("country")
      .register(registry)

    private val cacheRehydrateChanges = Counter.builder()
      .name("kinowo_worker_cache_rehydrate_changes")
      .help("Rows the MovieCache's periodic backstop rehydrate (full findAll reload) caught that the INCREMENTAL change stream missed, by country and kind (changed=a put whose cached value differed = a missed upsert; deleted=a key gone from Mongo the delete-apply didn't drop). After resume-token persistence + cache delete-apply this should be ~0 in steady state; a rate flat at 0 proves the 30-min rehydrate is redundant and can be retired. NOTE: the one-time BOOT hydrate counts EVERY row as changed — read the rate over steady state, not the raw counter.")
      .labelNames("country", "kind")
      .register(registry)


    private val readModelReconcileSweeps = Counter.builder()
      .name("kinowo_worker_readmodel_reconcile_sweeps")
      .help("Read-model orphan-prune sweeps since boot (kind=prune, the cheap id-only prune that removes deleted/re-keyed rows), by country and did_work (true=pruned >=1 doc, false=no-op). A prune with did_work=true is the deletes/re-keys the change stream can't deliver. (The full re-projection sweep was retired, so kind is always prune now.)")
      .labelNames("country", "kind", "did_work")
      .register(registry)

    private val changeEvents = Counter.builder()
      .name("kinowo_worker_movie_change_events")
      .help("Movie change-stream events the shared cursor consumed since boot, by country and op (insert|update|replace|delete). rate() is the change-stream volume the cache + read-model projector reproject from.")
      .labelNames("country", "op")
      .register(registry)

    private val changeUpdateKinds = Counter.builder()
      .name("kinowo_worker_movie_change_update_kinds")
      .help("For UPDATE events, which field kind changed since boot, by country: source_data=a cinema slot (scrape write), rating=a rating value/url, identity=tmdb/imdb id + resolution lifecycle, updated_at_only=a no-op that touched only updatedAt (a redundant-write canary — should stay ~0). A multi-field update counts under each kind it touched.")
      .labelNames("country", "kind")
      .register(registry)

    private val movieCoalesced = Counter.builder()
      .name("kinowo_worker_movie_coalesced_changes")
      .help("Movies-doc change events that rode an apply already queued for their film instead of buying their own, by country — the movies cursor's twin of screenings/movie_slots_coalesced_changes, sharing the SAME pending set as those two. `dropCinemaSlots` writes `retainedSynopses` to `movies` in the same tick it deletes the dropped venue's screenings/movie_slots rows, so one logical slot-drop used to buy this cursor its own re-projection on top of the side cursors' (coalesced) one. coalesced/(coalesced+readmodel_project_calls) is the share now folded away. All GENUINE changes; no write guard can remove them.")
      .labelNames("country")
      .register(registry)

    private val screeningsChangeEvents = Counter.builder()
      .name("kinowo_worker_screenings_change_events")
      .help("Screenings change-stream events the SECOND cursor consumed since boot, by country and op (insert|update|replace|delete). The read-model projection's larger trigger: this cursor rings once per changed screenings DOCUMENT — one per (film, cinema slot) — and each ring costs a stitch read plus a full projection. Read readmodel_project_calls_total against the SUM of this and movie_change_events; against movie_change_events alone it reads as an unexplained 55:1, which is what a 2026-09-04 projection climb looked like while this half of the input had no counter.")
      .labelNames("country", "op")
      .register(registry)

    private val screeningsWrites = Counter.builder()
      .name("kinowo_worker_screenings_writes")
      .help("Slot writes the screenings store was asked to make since boot, by country and outcome. written=the row's showtimes moved and it was written; unchanged=the row already held exactly what the caller asked for, so the write was DROPPED — it never reached the oplog, never rang the screenings change stream and never bought a read-model projection. A high unchanged SHARE is the guard working, not a fault: `replaceFilm` is film-wide while its callers' change is one venue, so a wide release rewrote every row it had (297 of 298 redundantly on prod DE, 2026-09-04). Watch the WRITTEN rate against the scrape rate instead — written climbing under a flat scrape rate is a caller that has started rewriting rows it did not change.")
      .labelNames("country", "outcome")
      .register(registry)

    private val screeningsCoalesced = Counter.builder()
      .name("kinowo_worker_screenings_coalesced_changes")
      .help("Screenings change events that rode an apply already queued for their film instead of buying their own, by country. The screenings cursor rings once per (film, cinema slot), so a film that changes at every venue rings once per venue — and one re-read after the burst sees all of it, because the apply reads the film's CURRENT state. This is the saving made visible: coalesced/(coalesced+readmodel_project_calls) is the share of the fan-out collapsed, ~0 where films move one venue at a time and approaching 1 on a wide release (the widest US film carries 3,327 slots). Unlike screenings_writes{outcome=unchanged} these are all GENUINE changes; no write guard can remove them.")
      .labelNames("country")
      .register(registry)

    private val slotsChangeEvents = Counter.builder()
      .name("kinowo_worker_movie_slots_change_events")
      .help("movie_slots change-stream events the THIRD cursor consumed since boot, by country and op (insert|update|replace|delete). One event per changed slot DOCUMENT — one per (film, cinema slot) — and each ring costs a stitch read plus a full projection, like the screenings cursor it mirrors. A venue's slot lands WITHOUT a movies write whenever the film document is unchanged (the usual case under the split), and the projection cannot emit that venue's row until the slot exists; before this cursor 63 UK and 33 PL (film, venue) pairs whose slot landed after the film's last projection were never projected again (prod, 2026-09-07). Read readmodel_project_calls_total against the SUM of this, screenings_change_events and movie_change_events.")
      .labelNames("country", "op")
      .register(registry)

    private val slotsCoalesced = Counter.builder()
      .name("kinowo_worker_movie_slots_coalesced_changes")
      .help("movie_slots change events that rode an apply already queued for their film instead of buying their own, by country — the slots twin of screenings_coalesced_changes, and the two cursors share ONE pending set, so a film's screenings row and slot row arriving together are one re-read. All GENUINE changes; no write guard can remove them.")
      .labelNames("country")
      .register(registry)

    private val resolveRetryDuplicates = Counter.builder()
      .name("kinowo_worker_resolve_retry_duplicates")
      .help("TMDB re-try resolves (mode=retry-miss|force) that found their film's resolve already queued, by country, mode and outcome: upgraded = merged into the WAITING task, which now searches in the re-try's mode; not-upgraded = the queued task was already being worked on with its old mode (or already carried this mode), so this re-try did not add a search. Before 2026-09-23 every one of these was DROPPED, invisible behind tasks_enqueued{result=deduped}, and a plain resolve then stopped at the remembered miss the re-try existed to look past, for another 24h. A plain duplicate loses nothing and is not counted.")
      .labelNames("country", "mode", "outcome")
      .register(registry)

    private val scrapeGuardVerdicts = Counter.builder()
      .name("kinowo_worker_scrape_guard_verdicts")
      .help("ScrapeLanding's depth and breadth guards (services.movies.ScrapeHealth) rejecting or accepting a tick, by country, guard (depth|breadth) and verdict (reject|accept). `healthy` is not counted — the overwhelming default on every tick of every cinema, and answered better by a scrape-completed counter elsewhere. Added 2026-09-13: before this, a guard stuck rejecting (or repeatedly giving up) for hours was visible only by grepping [scrape-depth]/[scrape-prune] log lines by cinema name — which is how long Kino Aurum's breadth-guard deadlock (57 accumulated slot-keys against an 11-film board, permanently below the prune-floor ratio) went unnoticed. A `reject` RATE that never falls is the signal to alert on; a sustained run of `accept`s on one axis means a scraper that has been degraded in the SAME shape for hours, not a one-off.")
      .labelNames("country", "guard", "verdict")
      .register(registry)

    private val scrapeWriteSkipped = Counter.builder()
      .name("kinowo_worker_scrape_write_skipped")
      .help("A scrape observed a title this tick but its write did not land, by country and reason (services.movies.ScrapeLandingMetrics.SkipReason): cache-miss-race is MovieCache.putIfPresent returning false because a concurrent rekey of some OTHER title invalidated this key between the read and the compute; unreadable-row is the cache-miss branch finding the stored row could not be read at all (see the WARN this pairs with). Both were already reasoned about and handled downstream (the title is spared from that tick's prune) but neither had a counter before 2026-09-13 — a skip this shaped throws nothing and logs nothing on its own, so a real, sustained skip and a once-off race were otherwise indistinguishable without reading screenings/movie_slots directly.")
      .labelNames("country", "reason")
      .register(registry)

    private val repositoryWriteFailed = Counter.builder()
      .name("kinowo_worker_repository_write_failed")
      .help("A MovieRepository / SlotsRepository / ScreeningsRepository / StagingRepository write that THREW, by country, collection (movies|movie_slots|screenings|pending_movies), op (upsert, replaceFilm, upsertSlot, updateIfPresent, delete, ...) and exception (the class's simple name). ZERO IS THE HEALTHY READING. Added 2026-09-24: a codec bug failed 34 upserts over ~6h and every one was logged at WARN and returned Unit, while the cache kept the unwritten row, so two new films never reached the site until a restart and nothing counted it. Every failure is also a WARN line naming the film; the cache rolls the row back so the next identical scrape retries. Alerted by RepositoryWritesFailing (worker-pipeline.rules). Every known (collection, op) is seeded at 0 for the expected exception classes (RepositoryWriteMetrics.SeededExceptions) so increase() sees the first failure; any other class appears on its first failure and counts from its second.")
      .labelNames("country", "collection", "op", "exception")
      .register(registry)

    private val decodeFailures = Counter.builder()
      .name("kinowo_worker_decode_failures")
      .help("Documents that could not be decoded, by country and collection. web_movies|web_screenings: SKIPPED by a whole-collection scan, a film or screening the reader goes without while the rest of its page is kept. movies: a read that FAILED on it — a point read answering unreadable, or the whole corpus scan left incomplete. movies|screenings|movie_slots from a change stream: a post-image it SKIPPED (not applied) rather than end its cursor on, which before 2026-09-24 killed the stream for good. ZERO IS THE HEALTHY READING; each was a WARN line and nothing else before 2026-09-24. Alerted by DocumentsUndecodable (worker-pipeline.rules). Seeded at 0 per collection so increase() sees the first skip.")
      .labelNames("country", "collection")
      .register(registry)

    seed()

    /** Materialize every series at 0 for every country so it exists from boot (no
     *  Grafana gaps). */
    private def seed(): Unit = {
      countryCodes.foreach { c =>
        TaskType.all.foreach { t =>
          EnqueueResults.foreach(r => enqueued.labelValues(c, t.name, r))
          started.labelValues(c, t.name)
          Outcomes.foreach(o => finished.labelValues(c, t.name, o))
          duration.labelValues(c, t.name)
          waitingByType.labelValues(c, t.name).set(0.0)
          oldestWaitingAge.labelValues(c, t.name).set(0.0)
          parkedMax.labelValues(c, t.name).set(0.0)
        }
        RatingSites.foreach(s => ratingFirstAttemptDelay.labelValues(c, s))
        QueueStates.foreach(s => queueDepth.labelValues(c, s).set(0.0))
        StagingStep.all.foreach(s => stagingMovies.labelValues(c, s.label).set(0.0))
        MergeReason.all.foreach(r => merges.labelValues(c, r.label))
        RekeyReason.all.foreach(r => rekeys.labelValues(c, r.label))
        splits.labelValues(c).inc(0.0) // materialize the series at 0 so Grafana draws a continuous line
        ScrapeLandingMetrics.Guards.foreach(g =>
          ScrapeLandingMetrics.Verdicts.foreach(v => scrapeGuardVerdicts.labelValues(c, g, v).inc(0.0)))
        ScrapeLandingMetrics.SkipReasons.foreach(r => scrapeWriteSkipped.labelValues(c, r).inc(0.0))
        RepositoryWriteMetrics.Writes.foreach { case (collection, op) =>
          RepositoryWriteMetrics.SeededExceptions.foreach(e => repositoryWriteFailed.labelValues(c, collection, op, e).inc(0.0))
        }
        services.readmodel.DecodeFailureMetrics.Collections.foreach(coll => decodeFailures.labelValues(c, coll).inc(0.0))
        RetryModes.foreach(m => ResolveDuplicateOutcomes.foreach(o => resolveRetryDuplicates.labelValues(c, m, o).inc(0.0)))
        ReadModelProjectionMetrics.Targets.foreach(t =>
          ReadModelProjectionMetrics.Ops.foreach(o => readModelWrites.labelValues(c, t, o)))
        // Materialize at 0 so Grafana draws a continuous line — and for the prune, so the
        // line that MUST stay at zero is visibly at zero rather than absent (the rule).
        ReadModelProjectionMetrics.PruneReasons.foreach(r => readModelFilmsPruned.labelValues(c, r).inc(0.0))
        ReadModelProjectionMetrics.RetireReasons.foreach(r => readModelCardsRetired.labelValues(c, r).inc(0.0))
        readModelDriftWrites.labelValues(c).inc(0.0)   // zero is the healthy reading, so it must be drawn
        readModelCatchUpRows.labelValues(c).inc(0.0) // ditto — zero is the healthy reading, so it must be drawn
        ReadModelProjectionMetrics.HealTriggers.foreach(t => readModelHeals.labelValues(c, t).inc(0.0)) // ditto
        // Every trigger at 0 from boot: ReadModelProjectionTriggerUnaccounted reads `stream` alone,
        // and a series that does not exist yet makes it silent rather than compare against zero.
        ReadModelProjectionMetrics.ProjectTrigger.values.foreach(t => readModelProjectCalls.labelValues(c, t.label).inc(0.0))
        readModelProjectCpu.labelValues(c).inc(0.0)       // ditto — the CPU-attribution counter the drivers panel stacks
        readModelProjectDuration.labelValues(c).observe(0.0) // materialize the histogram (_sum/_count/_bucket) from boot — no Grafana gap
        readModelWriteBurst.labelValues(c).observe(0.0)      // ditto — the write-phase half of that same answer
        ReadModelProjectionMetrics.MetadataOutcomes.foreach(o => readModelMetadataProjections.labelValues(c, o))
        ReadModelProjectionMetrics.VenueOutcomes.foreach(o => readModelVenueProjections.labelValues(c, o))
        ReadModelProjectionMetrics.ReconcileKinds.foreach(k =>
          Seq("true", "false").foreach(w => readModelReconcileSweeps.labelValues(c, k, w)))
        stagingNewcomerKicks.labelValues(c).inc(0.0)    // materialize so rate() has a baseline from boot
        stagingNewcomerKickRows.labelValues(c).inc(0.0)
        Seq("changed", "deleted").foreach(k => cacheRehydrateChanges.labelValues(c, k))
        ChangeStreamMetrics.Ops.foreach(o => changeEvents.labelValues(c, o))
        movieCoalesced.labelValues(c)
        ChangeStreamMetrics.Ops.foreach(o => screeningsChangeEvents.labelValues(c, o))
        ScreeningsMetrics.Outcomes.foreach(o => screeningsWrites.labelValues(c, o))
        screeningsCoalesced.labelValues(c)
        ChangeStreamMetrics.Ops.foreach(o => slotsChangeEvents.labelValues(c, o))
        slotsCoalesced.labelValues(c)
        ChangeStreamMetrics.Kinds.foreach(k => changeUpdateKinds.labelValues(c, k))
        ChangeStreamLiveness.Collections.foreach { coll =>
          changeStreamLastEventAge.labelValues(c, coll).set(0.0)
          changeStreamApplyPending.labelValues(c, coll).set(0.0)
          changeStreamApplyLag.labelValues(c, coll).set(0.0)
        }
      }
      poolSizeGauge.set(poolSize.toDouble)
    }

    // ── RatingLatencyMetrics ──────────────────────────────────────────────────
    def recordFirstRatingDelay(country: String, site: String, seconds: Double): Unit =
      ratingFirstAttemptDelay.labelValues(country, site).observe(math.max(0.0, seconds))

    /** Each absorbed victim row is one increment under its fold's reason. */
    def recordMerge(country: String, reason: MergeReason, victims: Int): Unit =
      if (victims > 0) merges.labelValues(country, reason.label).inc(victims.toDouble)

    /** One increment per row whose key moved while it stayed the same film. */
    def recordRekey(country: String, reason: RekeyReason): Unit =
      rekeys.labelValues(country, reason.label).inc()

    /** Each cinema slot re-diverted by a mixed-row split is one increment. */
    def recordSplit(country: String, fragments: Int): Unit =
      if (fragments > 0) splits.labelValues(country).inc(fragments.toDouble)

    // ── ReadModelProjectionMetrics ────────────────────────────────────────────
    def recordWrite(country: String, target: String, op: String, count: Int): Unit =
      if (count > 0) readModelWrites.labelValues(country, target, op).inc(count.toDouble)

    def recordFilmPruned(country: String, reason: String, count: Int): Unit =
      if (count > 0) readModelFilmsPruned.labelValues(country, reason).inc(count.toDouble)

    def recordCardRetired(country: String, reason: String): Unit =
      readModelCardsRetired.labelValues(country, reason).inc()

    def recordDriftWrites(country: String, documents: Int): Unit =
      if (documents > 0) readModelDriftWrites.labelValues(country).inc(documents.toDouble)

    def recordCatchUp(country: String, rows: Int): Unit =
      if (rows > 0) readModelCatchUpRows.labelValues(country).inc(rows.toDouble)

    def recordHeal(country: String, trigger: String, rows: Int): Unit =
      if (rows > 0) readModelHeals.labelValues(country, trigger).inc(rows.toDouble)

    def recordCardWrite(country: String, changed: Set[String]): Unit = {
      readModelCardWrites.labelValues(country, ReadModelProjectionMetrics.cardWriteCause(changed)).inc()
      changed.foreach(part => readModelCardRewriteParts.labelValues(country, part).inc())
    }

    def recordProject(country: String, trigger: ReadModelProjectionMetrics.ProjectTrigger, wallSeconds: Double, cpuSeconds: Double): Unit = {
      readModelProjectDuration.labelValues(country).observe(math.max(0.0, wallSeconds))
      readModelProjectCpu.labelValues(country).inc(math.max(0.0, cpuSeconds))
      readModelProjectCalls.labelValues(country, trigger.label).inc()
    }

    def recordWriteBurst(country: String, seconds: Double): Unit =
      readModelWriteBurst.labelValues(country).observe(math.max(0.0, seconds))

    def recordMetadataProjection(country: String, reused: Boolean): Unit =
      readModelMetadataProjections.labelValues(country,
        if (reused) ReadModelProjectionMetrics.MetadataOutcome.Reused
        else ReadModelProjectionMetrics.MetadataOutcome.Recomputed).inc()

    def recordVenueProjection(country: String, rebuilt: Int, reused: Int): Unit = {
      if (rebuilt > 0) readModelVenueProjections.labelValues(country, ReadModelProjectionMetrics.VenueOutcome.Rebuilt).inc(rebuilt.toDouble)
      if (reused > 0)  readModelVenueProjections.labelValues(country, ReadModelProjectionMetrics.VenueOutcome.Reused).inc(reused.toDouble)
    }

    def recordReconcileSweep(country: String, kind: String, didWork: Boolean): Unit =
      readModelReconcileSweeps.labelValues(country, kind, didWork.toString).inc()

    // ── StagingMetrics ────────────────────────────────────────────────────────
    def recordStagingNewcomerKick(country: String, groupRows: Int): Unit = {
      stagingNewcomerKicks.labelValues(country).inc()
      stagingNewcomerKickRows.labelValues(country).inc(math.max(0, groupRows).toDouble)
    }

    // ── CacheSyncMetrics ──────────────────────────────────────────────────────
    def recordRehydrate(country: String, changedUpserts: Int, deletes: Int): Unit = {
      if (changedUpserts > 0) cacheRehydrateChanges.labelValues(country, "changed").inc(changedUpserts.toDouble)
      if (deletes > 0)        cacheRehydrateChanges.labelValues(country, "deleted").inc(deletes.toDouble)
    }

    def recordResolveDuplicate(country: String, mode: ResolveMode, upgraded: Boolean): Unit =
      resolveRetryDuplicates.labelValues(country, modeLabel(mode),
        if (upgraded) ResolveDuplicateOutcome.Upgraded else ResolveDuplicateOutcome.NotUpgraded).inc()

    // ── ScrapeLandingMetrics ──────────────────────────────────────────────────
    def recordScrapeGuardVerdict(country: String, guard: String, verdict: String): Unit =
      scrapeGuardVerdicts.labelValues(country, guard, verdict).inc()
    def recordScrapeWriteSkipped(country: String, reason: String): Unit =
      scrapeWriteSkipped.labelValues(country, reason).inc()

    // ── RepositoryWriteMetrics ─────────────────────────────────────────────────
    def recordRepositoryWriteFailed(country: String, collection: String, op: String, exception: String): Unit =
      repositoryWriteFailed.labelValues(country, collection, op, exception).inc()

    // ── DecodeFailureMetrics ───────────────────────────────────────────────────
    def recordDecodeFailure(country: String, collection: String): Unit =
      decodeFailures.labelValues(country, collection).inc()

    // ── ChangeStreamMetrics ────────────────────────────────────────────────────
    def recordEvent(country: String, op: String): Unit       = changeEvents.labelValues(country, op).inc()
    def recordUpdateKind(country: String, kind: String): Unit = changeUpdateKinds.labelValues(country, kind).inc()
    def recordMovieCoalesced(country: String): Unit          = movieCoalesced.labelValues(country).inc()

    // ── ScreeningsMetrics ──────────────────────────────────────────────────────
    def recordScreeningsChangeEvent(country: String, op: String): Unit = screeningsChangeEvents.labelValues(country, op).inc()
    def recordScreeningsWrite(country: String, outcome: String, count: Int): Unit =
      if (count > 0) screeningsWrites.labelValues(country, outcome).inc(count.toDouble)
    def recordScreeningsCoalesced(country: String): Unit = screeningsCoalesced.labelValues(country).inc()

    // ── SideCollectionChangeMetrics for `movie_slots` ──────────────────────────
    def recordSlotsChangeEvent(country: String, op: String): Unit = slotsChangeEvents.labelValues(country, op).inc()
    def recordSlotsCoalesced(country: String): Unit               = slotsCoalesced.labelValues(country).inc()

    def recordEnqueue(country: String, taskType: TaskType, result: String): Unit =
      enqueued.labelValues(country, taskType.name, result).inc()

    def onStarted(country: String, task: Task): Unit =
      started.labelValues(country, task.taskType.name).inc()

    def onFinished(country: String, task: Task, outcome: String, handleMillis: Long): Unit = {
      finished.labelValues(country, task.taskType.name, outcome).inc()
      // Only fully-worked tasks contribute to the duration histogram — a Skipped
      // task only paid a freshness check, and a Reschedule/failure didn't finish.
      if (outcome == Outcome.Done) duration.labelValues(country, task.taskType.name).observe(handleMillis / 1000.0)
    }

    /** Refresh each country's queue + staging gauges from its live sample and
     *  render the full exposition (task pipeline + census + JVM, all on the shared
     *  registry). Called from the worker's `/metrics` handler on each Fly scrape
     *  with one [[CountryQueueSample]] per running country. */
    def scrape(samples: Seq[CountryQueueSample], now: Instant): String = {
      samples.foreach { s =>
        s.snapshot.foreach(refreshQueueGauges(s.countryCode, _, now))
        s.stagingByStep.foreach { byStep =>
          StagingStep.all.foreach(step => stagingMovies.labelValues(s.countryCode, step.label).set(byStep.getOrElse(step, 0).toDouble))
        }
        ChangeStreamLiveness.Collections.foreach { coll =>
          changeStreamLastEventAge.labelValues(s.countryCode, coll).set(s.changeStreamLiveness.ageSeconds(coll, now))
          changeStreamApplyPending.labelValues(s.countryCode, coll).set(s.changeStreamLiveness.pendingApplies(coll).toDouble)
          changeStreamApplyLag.labelValues(s.countryCode, coll).set(s.changeStreamLiveness.applyLagSeconds(coll, now))
        }
      }
      PrometheusExposition.render(registry)
    }

    private def refreshQueueGauges(country: String, snapshot: QueueSnapshot, now: Instant): Unit = {
      queueDepth.labelValues(country, TaskState.Waiting).set(snapshot.counts.getOrElse(TaskState.Waiting, 0L).toDouble)
      queueDepth.labelValues(country, TaskState.WorkedOn).set(snapshot.counts.getOrElse(TaskState.WorkedOn, 0L).toDouble)

      val waiting = snapshot.active.filter(_.state == TaskState.Waiting).groupBy(_.taskType)
      TaskType.all.foreach { t =>
        val rows = waiting.getOrElse(t.name, Nil)
        waitingByType.labelValues(country, t.name).set(rows.size.toDouble)
        // Head-of-line age counts only CLAIMABLE rows, from when each became claimable:
        // a row parked in retry backoff (or a staggered chunk not yet due) is being held
        // back on purpose, and aging it from `submittedAt` made one retrying task read as
        // a pool that could not keep up (UK/ES 2026-09-18..22, pool idle throughout).
        val age = rows.filter(_.claimableAt(now)).map(_.claimableSince).minOption
          .map(oldest => math.max(0L, now.getEpochSecond - oldest.getEpochSecond).toDouble)
          .getOrElse(0.0)
        oldestWaitingAge.labelValues(country, t.name).set(age)
        val parked = rows.flatMap(_.nextEligibleAt).filter(_.isAfter(now))
          .map(until => until.getEpochSecond - now.getEpochSecond).maxOption.getOrElse(0L)
        parkedMax.labelValues(country, t.name).set(parked.toDouble)
      }
    }
  }

  object Outcome {
    val Done        = "done"
    val Skipped     = "skipped"
    val Rescheduled = "rescheduled"
    // Returned untried because a precondition refused it (circuit open) — kept
    // apart from `rescheduled` precisely because the two used to be indistinguishable:
    // a host-wide block read as a flood of genuine task failures on the panel.
    val Deferred    = "deferred"
    val Failed      = "failed"
    val NoHandler   = "no_handler"
    // Dropped after `maxAttempts` failures — the terminal state of a task that never
    // succeeded. The one outcome that means work was LOST at this pool: a reaper may
    // re-create it, but nothing here will run it again.
    val Exhausted   = "exhausted"
    // Dropped on its FIRST failure because the failure is deterministic — a violated
    // `require` — so a retry could only replay it. Also lost work, like `exhausted`,
    // but it names a bug in the task's inputs rather than an upstream that stayed down.
    val Permanent   = "permanent"
  }
  val Outcomes: Seq[String] =
    Seq(Outcome.Done, Outcome.Skipped, Outcome.Rescheduled, Outcome.Deferred, Outcome.Failed, Outcome.NoHandler, Outcome.Exhausted, Outcome.Permanent)

  // `failed` = the queue could not answer (a Mongo error, not a duplicate key): the
  // task was NOT queued and its caller only learns so from this series.
  object EnqueueResult { val Added = "added"; val Deduped = "deduped"; val Failed = "failed" }
  val EnqueueResults: Seq[String] = Seq(EnqueueResult.Added, EnqueueResult.Deduped, EnqueueResult.Failed)

  private val QueueStates: Seq[String] = Seq(TaskState.Waiting, TaskState.WorkedOn)

  /** `outcome` of a re-try resolve that landed on an already-queued one. */
  object ResolveDuplicateOutcome { val Upgraded = "upgraded"; val NotUpgraded = "not-upgraded" }
  private val ResolveDuplicateOutcomes: Seq[String] = Seq(ResolveDuplicateOutcome.Upgraded, ResolveDuplicateOutcome.NotUpgraded)

  /** The `mode` label of a re-try resolve; Normal is never reported (see ResolveDuplicateMetrics). */
  private def modeLabel(mode: ResolveMode): String = mode match {
    case ResolveMode.Normal    => "normal"
    case ResolveMode.RetryMiss => "retry-miss"
    case ResolveMode.Force     => "force"
  }
  private val RetryModes: Seq[String] = Seq(ResolveMode.RetryMiss, ResolveMode.Force).map(modeLabel)

  /** Fixed histogram upper bounds (seconds), spanning a sub-second freshness
   *  skip up to a slow multi-minute scrape/detail fetch. */
  val DurationBucketsSeconds: Seq[Double] = Seq(0.1, 0.5, 1.0, 2.0, 5.0, 10.0, 30.0, 60.0, 120.0)

  /** Upper bounds (seconds) for one pure projectAll — sub-millisecond for a
   *  single-variant row up to the ~0.3s worst-case many-variant film (post
   *  synopsis-memoization; pre-memoization the tail reached ~2.3s). */
  val ProjectBucketsSeconds: Seq[Double] = Seq(0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5)

  /** Upper bounds (seconds) for the TMDB-resolved → first-rating-attempt delay:
   *  ~one reaper tick (≤1min) in steady state, stretching to hours when a large
   *  resolution cohort drains over the per-tick cap. 30s … 4h. */
  val RatingDelayBucketsSeconds: Seq[Double] =
    Seq(30.0, 60.0, 120.0, 300.0, 600.0, 1200.0, 1800.0, 3600.0, 7200.0, 14400.0)

  /** The four rating-site labels, materialized at boot so each series exists from
   *  the start (no Grafana gaps). Mirrors the [[FreshnessKind]] rating labels. */
  val RatingSites: Seq[String] =
    Seq(FreshnessKind.ImdbRating, FreshnessKind.FilmwebRating, FreshnessKind.RtRating, FreshnessKind.McRating).map(_.label)
}
