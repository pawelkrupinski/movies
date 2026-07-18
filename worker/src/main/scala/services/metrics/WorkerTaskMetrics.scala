package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.freshness.FreshnessKind
import services.movies.{CacheSyncMetrics, ChangeStreamMetrics, MergeMetrics, MergeReason, SplitMetrics}
import services.readmodel.ReadModelProjectionMetrics
import services.staging.StagingStep
import services.tasks.{QueueSnapshot, RatingLatencyMetrics, Task, TaskState, TaskType}

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
   *  [[TaskWorker]] still takes a single observer while both the Prometheus sink
   *  and the scrape-throttle monitor see every task. */
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
 * Java client rather than a hand-rolled exposition; Fly's managed Prometheus
 * scrapes it via the `[[metrics]]` block in fly.worker.toml, and the self-hosted
 * Grafana charts the `rate(...)` of each family per `task_type`.
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
  extends TaskObserver with MergeMetrics with SplitMetrics with ReadModelProjectionMetrics with RatingLatencyMetrics with ChangeStreamMetrics with CacheSyncMetrics {

  // ── RatingLatencyMetrics ────────────────────────────────────────────────────
  def recordFirstRatingDelay(site: String, seconds: Double): Unit = series.recordFirstRatingDelay(countryCode, site, seconds)

  // ── MergeMetrics / SplitMetrics ─────────────────────────────────────────────
  def recordMerge(reason: MergeReason, victims: Int): Unit = series.recordMerge(countryCode, reason, victims)
  def recordSplit(fragments: Int): Unit                    = series.recordSplit(countryCode, fragments)

  // ── ReadModelProjectionMetrics ──────────────────────────────────────────────
  def recordWrite(target: String, op: String, count: Int): Unit = series.recordWrite(countryCode, target, op, count)
  def recordFilmPruned(count: Int): Unit                        = series.recordFilmPruned(countryCode, count)
  def recordProject(seconds: Double): Unit                      = series.recordProject(countryCode, seconds)
  def recordMetadataProjection(reused: Boolean): Unit           = series.recordMetadataProjection(countryCode, reused)
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit = series.recordReconcileSweep(countryCode, kind, didWork)

  // ── CacheSyncMetrics ────────────────────────────────────────────────────────
  def recordRehydrate(changedUpserts: Int, deletes: Int): Unit = series.recordRehydrate(countryCode, changedUpserts, deletes)

  // ── ChangeStreamMetrics ─────────────────────────────────────────────────────
  def recordEvent(op: String): Unit        = series.recordEvent(countryCode, op)
  def recordUpdateKind(kind: String): Unit = series.recordUpdateKind(countryCode, kind)

  // ── Task lifecycle ──────────────────────────────────────────────────────────
  def recordEnqueue(taskType: TaskType, result: String): Unit = series.recordEnqueue(countryCode, taskType, result)
  def onStarted(task: Task): Unit                             = series.onStarted(countryCode, task)
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = series.onFinished(countryCode, task, outcome, handleMillis)
}

object WorkerTaskMetrics {

  /** The per-country queue/staging/throttle sample [[Series.scrape]] folds into
   *  the gauges for one country on each Fly scrape. `snapshot` is that country's
   *  live queue monitor, `stagingByStep` its `StagingReaper.stepCounts()`,
   *  `throttled` its `ScrapeThrottleSignal`. */
  case class CountryQueueSample(countryCode: String, snapshot: QueueSnapshot, stagingByStep: Map[StagingStep, Int], throttled: Boolean)

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
      .help("Tasks that finished a handler run since boot, by country, type and outcome (done=fully worked, skipped=data already fresh, rescheduled=transient retry, failed=handler threw, no_handler=no wired handler).")
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
      .help("Age of the oldest waiting task per country and type — head-of-line latency / starvation signal.")
      .labelNames("country", "task_type")
      .register(registry)

    // The worker pool is a single SharedExecutionBudget across all countries, so
    // this is a process-level gauge with no country label (pairing it per-country
    // would misleadingly imply a per-country pool).
    private val poolSizeGauge = Gauge.builder()
      .name("kinowo_worker_pool_size")
      .help("Configured worker pool size (shared across countries) — pair with queue_depth{state=\"worked_on\"} for utilization.")
      .register(registry)

    // 1 while a country's reapers are backing off (CPU-credit throttled — by the
    // external Grafana credit gate OR the in-process scrape-duration backstop), 0
    // otherwise. A Grafana alert on this drives the Telegram start/end
    // notification, and it's a clean on/off panel. Set at scrape time from each
    // country's live signal.
    private val throttledGauge = Gauge.builder()
      .name("kinowo_worker_throttled")
      .help("1 = a country's reapers are backing off (CPU-credit throttled), 0 = full enqueue.")
      .labelNames("country")
      .register(registry)

    private val stagingMovies = Gauge.builder()
      .name("kinowo_worker_staging_movies")
      .help("Incubating films currently in pending_movies, by country and the step each needs next (detail → resolve_tmdb → resolve_imdb → fold). Distinct films (a film's cinema rows count once); sum = total movies in staging.")
      .labelNames("country", "step")
      .register(registry)

    private val merges = Counter.builder()
      .name("kinowo_worker_merges")
      .help("Movie rows folded into another row since boot (one per victim absorbed; a cluster of N counts N−1), by country and reason — canonicalize=periodic same-film settle/rehydrate fold, resolved-settle=TMDB-resolve year fold, tmdb-identity=runtime same-tmdbId put-gate, normalize-rebuild=title-rule change re-merges rows that now share a key. Each fold orphans the victim's title|year freshness, so rate() is the re-key re-enrichment load.")
      .labelNames("country", "reason")
      .register(registry)

    private val splits = Counter.builder()
      .name("kinowo_worker_splits")
      .help("New movie rows spawned by un-merging since boot (a 1→N split counts N−1), by country — the inverse of a merge. Only a title-rule change (NormalizeRebuild) re-keys a row's slots onto distinct keys; each split-off is born fresh (no tmdbId) and re-resolves, so rate() is the un-merge re-enrichment load. Pairs with kinowo_worker_merges_total{reason=\"normalize-rebuild\"}.")
      .labelNames("country")
      .register(registry)

    private val readModelWrites = Counter.builder()
      .name("kinowo_worker_readmodel_writes")
      .help("Denormalised read-model documents (re)written since boot, by country, target (movie|screening) and op (upsert|delete). rate() is the reprojection churn the worker pushes through the web's web_movies/web_screenings change streams.")
      .labelNames("country", "target", "op")
      .register(registry)

    private val readModelFilmsPruned = Counter.builder()
      .name("kinowo_worker_readmodel_films_pruned")
      .help("Films whose derived documents were removed from the read model during reconcile since boot, by country — a source row that vanished or was re-keyed (its filmId changed). The event that can briefly 404 a film deep-link until the new key propagates to the web; pair with kinowo_worker_merges_total for the re-key cause.")
      .labelNames("country")
      .register(registry)

    private val readModelProjectDuration = Histogram.builder()
      .name("kinowo_worker_readmodel_project_duration_seconds")
      .help("Wall-clock of one pure ReadModelProjection.projectAll (CPU-bound, no I/O) per source row since boot, by country. rate(_sum)*100 is projection's share of worker CPU in centi-cores — the credit-floor driver the CPU-drivers dashboard stacks against JIT + everything else.")
      .labelNames("country")
      .classicUpperBounds(ProjectBucketsSeconds*)
      .classicOnly()
      .register(registry)

    private val readModelProjectCalls = Counter.builder()
      .name("kinowo_worker_readmodel_project_calls")
      .help("Source rows projected (projectAll invoked) since boot, by country — the throughput denominator for readmodel_project_duration_seconds. Driven by the incremental change-stream path (the periodic full reproject sweep was retired).")
      .labelNames("country")
      .register(registry)

    private val readModelMetadataProjections = Counter.builder()
      .name("kinowo_worker_readmodel_metadata_projections")
      .help("Projections by country and whether the metadata half (resolve/synopsisByCity/ratingsFor) was REUSED from the per-film cache (outcome=reused — a showtime-only change at an already-present cinema, only the cheap screenings half re-ran) or RECOMPUTED (outcome=recomputed — a rating/synopsis/new-cinema change, or a first projection). rate(reused) / rate(reused+recomputed) is opt-1's hit ratio — high reuse under reproject/enrich showtime churn is the CPU win.")
      .labelNames("country", "outcome")
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
        }
        RatingSites.foreach(s => ratingFirstAttemptDelay.labelValues(c, s))
        QueueStates.foreach(s => queueDepth.labelValues(c, s).set(0.0))
        StagingStep.all.foreach(s => stagingMovies.labelValues(c, s.label).set(0.0))
        MergeReason.all.foreach(r => merges.labelValues(c, r.label))
        splits.labelValues(c).inc(0.0) // materialize the series at 0 so Grafana draws a continuous line
        ReadModelProjectionMetrics.Targets.foreach(t =>
          ReadModelProjectionMetrics.Ops.foreach(o => readModelWrites.labelValues(c, t, o)))
        readModelFilmsPruned.labelValues(c).inc(0.0) // materialize the series at 0 so Grafana draws a continuous line
        readModelProjectCalls.labelValues(c).inc(0.0)     // materialize at 0 so the counter series (+ its _created) exists from boot
        readModelProjectDuration.labelValues(c).observe(0.0) // materialize the histogram (_sum/_count/_bucket) from boot — no Grafana gap
        ReadModelProjectionMetrics.MetadataOutcomes.foreach(o => readModelMetadataProjections.labelValues(c, o))
        ReadModelProjectionMetrics.ReconcileKinds.foreach(k =>
          Seq("true", "false").foreach(w => readModelReconcileSweeps.labelValues(c, k, w)))
        Seq("changed", "deleted").foreach(k => cacheRehydrateChanges.labelValues(c, k))
        ChangeStreamMetrics.Ops.foreach(o => changeEvents.labelValues(c, o))
        ChangeStreamMetrics.Kinds.foreach(k => changeUpdateKinds.labelValues(c, k))
        throttledGauge.labelValues(c).set(0.0) // materialize at 0 so Grafana draws a continuous on/off line
      }
      poolSizeGauge.set(poolSize.toDouble)
    }

    // ── RatingLatencyMetrics ──────────────────────────────────────────────────
    def recordFirstRatingDelay(country: String, site: String, seconds: Double): Unit =
      ratingFirstAttemptDelay.labelValues(country, site).observe(math.max(0.0, seconds))

    /** Each absorbed victim row is one increment under its fold's reason. */
    def recordMerge(country: String, reason: MergeReason, victims: Int): Unit =
      if (victims > 0) merges.labelValues(country, reason.label).inc(victims.toDouble)

    /** Each new row spawned by an un-merge is one increment (a 1→N split = N−1). */
    def recordSplit(country: String, fragments: Int): Unit =
      if (fragments > 0) splits.labelValues(country).inc(fragments.toDouble)

    // ── ReadModelProjectionMetrics ────────────────────────────────────────────
    def recordWrite(country: String, target: String, op: String, count: Int): Unit =
      if (count > 0) readModelWrites.labelValues(country, target, op).inc(count.toDouble)

    def recordFilmPruned(country: String, count: Int): Unit =
      if (count > 0) readModelFilmsPruned.labelValues(country).inc(count.toDouble)

    def recordProject(country: String, seconds: Double): Unit = {
      readModelProjectDuration.labelValues(country).observe(math.max(0.0, seconds))
      readModelProjectCalls.labelValues(country).inc()
    }

    def recordMetadataProjection(country: String, reused: Boolean): Unit =
      readModelMetadataProjections.labelValues(country,
        if (reused) ReadModelProjectionMetrics.MetadataOutcome.Reused
        else ReadModelProjectionMetrics.MetadataOutcome.Recomputed).inc()

    def recordReconcileSweep(country: String, kind: String, didWork: Boolean): Unit =
      readModelReconcileSweeps.labelValues(country, kind, didWork.toString).inc()

    // ── CacheSyncMetrics ──────────────────────────────────────────────────────
    def recordRehydrate(country: String, changedUpserts: Int, deletes: Int): Unit = {
      if (changedUpserts > 0) cacheRehydrateChanges.labelValues(country, "changed").inc(changedUpserts.toDouble)
      if (deletes > 0)        cacheRehydrateChanges.labelValues(country, "deleted").inc(deletes.toDouble)
    }

    // ── ChangeStreamMetrics ────────────────────────────────────────────────────
    def recordEvent(country: String, op: String): Unit       = changeEvents.labelValues(country, op).inc()
    def recordUpdateKind(country: String, kind: String): Unit = changeUpdateKinds.labelValues(country, kind).inc()

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
        refreshQueueGauges(s.countryCode, s.snapshot, now)
        StagingStep.all.foreach(step => stagingMovies.labelValues(s.countryCode, step.label).set(s.stagingByStep.getOrElse(step, 0).toDouble))
        throttledGauge.labelValues(s.countryCode).set(if (s.throttled) 1.0 else 0.0)
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
        val age = rows.map(_.submittedAt).minOption
          .map(oldest => math.max(0L, now.getEpochSecond - oldest.getEpochSecond).toDouble)
          .getOrElse(0.0)
        oldestWaitingAge.labelValues(country, t.name).set(age)
      }
    }
  }

  object Outcome {
    val Done        = "done"
    val Skipped     = "skipped"
    val Rescheduled = "rescheduled"
    val Failed      = "failed"
    val NoHandler   = "no_handler"
  }
  val Outcomes: Seq[String] =
    Seq(Outcome.Done, Outcome.Skipped, Outcome.Rescheduled, Outcome.Failed, Outcome.NoHandler)

  object EnqueueResult { val Added = "added"; val Deduped = "deduped" }
  val EnqueueResults: Seq[String] = Seq(EnqueueResult.Added, EnqueueResult.Deduped)

  private val QueueStates: Seq[String] = Seq(TaskState.Waiting, TaskState.WorkedOn)

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
