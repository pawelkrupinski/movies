package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.freshness.FreshnessKind
import services.movies.{ChangeStreamMetrics, MergeMetrics, MergeReason, SplitMetrics}
import services.readmodel.ReadModelProjectionMetrics
import services.staging.StagingStep
import services.tasks.{QueueSnapshot, RatingLatencyMetrics, Task, TaskState, TaskType}

import java.io.ByteArrayOutputStream
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
 * Counters are monotonic since-boot totals — `rate()` handles the reset on each
 * worker reboot. Every `TaskType` (× every label value) is materialized to 0 at
 * construction so its series exists from boot and Grafana draws a continuous
 * line rather than a gap for a type/outcome that hasn't happened yet.
 *
 * Wiring: [[MeteredTaskQueue]] feeds `recordEnqueue`; [[services.tasks.TaskWorker]]
 * feeds `onStarted`/`onFinished` (this class is its `TaskObserver`); the queue
 * gauges are refreshed from a `QueueSnapshot` each `scrape()`.
 */
class WorkerTaskMetrics(poolSize: Int, registry: PrometheusRegistry = new PrometheusRegistry())
  extends TaskObserver with MergeMetrics with SplitMetrics with ReadModelProjectionMetrics with RatingLatencyMetrics with ChangeStreamMetrics {
  import WorkerTaskMetrics._

  // The client auto-appends `_total` to counter names, so they're declared without it.
  private val enqueued = Counter.builder()
    .name("kinowo_worker_tasks_enqueued")
    .help("Tasks added to the queue (result=added) or collapsed onto an active duplicate (result=deduped) since boot, by type.")
    .labelNames("task_type", "result")
    .register(registry)

  private val started = Counter.builder()
    .name("kinowo_worker_tasks_started")
    .help("Tasks claimed and kicked off since boot, by type.")
    .labelNames("task_type")
    .register(registry)

  private val finished = Counter.builder()
    .name("kinowo_worker_tasks_finished")
    .help("Tasks that finished a handler run since boot, by type and outcome (done=fully worked, skipped=data already fresh, rescheduled=transient retry, failed=handler threw, no_handler=no wired handler).")
    .labelNames("task_type", "outcome")
    .register(registry)

  private val duration = Histogram.builder()
    .name("kinowo_worker_task_duration_seconds")
    .help("Handler wall-clock of fully-worked (done) tasks, by type.")
    .labelNames("task_type")
    .classicUpperBounds(DurationBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val ratingFirstAttemptDelay = Histogram.builder()
    .name("kinowo_worker_rating_first_attempt_delay_seconds")
    .help("Delay between a film's TMDB resolution and the FIRST attempt to fetch its rating, by site (imdb/fw/rt/mc). Measures the EnrichmentReaper's first-pass latency now that ratings aren't enqueued the instant a film resolves.")
    .labelNames("site")
    .classicUpperBounds(RatingDelayBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val queueDepth = Gauge.builder()
    .name("kinowo_worker_queue_depth")
    .help("Tasks currently in the queue, by state.")
    .labelNames("state")
    .register(registry)

  private val waitingByType = Gauge.builder()
    .name("kinowo_worker_queue_waiting_by_type")
    .help("Waiting (claimable) tasks currently in the queue, by type. Sampled from the bounded active snapshot.")
    .labelNames("task_type")
    .register(registry)

  private val oldestWaitingAge = Gauge.builder()
    .name("kinowo_worker_queue_oldest_waiting_age_seconds")
    .help("Age of the oldest waiting task per type — head-of-line latency / starvation signal.")
    .labelNames("task_type")
    .register(registry)

  private val poolSizeGauge = Gauge.builder()
    .name("kinowo_worker_pool_size")
    .help("Configured worker pool size — pair with queue_depth{state=\"worked_on\"} for utilization.")
    .register(registry)

  // 1 while the reapers are backing off (CPU-credit throttled — by the external
  // Grafana credit gate OR the in-process scrape-duration backstop), 0 otherwise.
  // A Grafana alert on this drives the Telegram start/end notification, and it's a
  // clean on/off panel for the dashboard. Set at scrape time from the live signal.
  private val throttledGauge = Gauge.builder()
    .name("kinowo_worker_throttled")
    .help("1 = reapers are backing off (CPU-credit throttled), 0 = full enqueue.")
    .register(registry)

  private val stagingMovies = Gauge.builder()
    .name("kinowo_worker_staging_movies")
    .help("Incubating films currently in pending_movies, by the step each needs next (detail → resolve_tmdb → resolve_imdb → fold). Distinct films (a film's cinema rows count once); sum = total movies in staging.")
    .labelNames("step")
    .register(registry)

  private val merges = Counter.builder()
    .name("kinowo_worker_merges")
    .help("Movie rows folded into another row since boot (one per victim absorbed; a cluster of N counts N−1), by reason — canonicalize=periodic same-film settle/rehydrate fold, resolved-settle=TMDB-resolve year fold, tmdb-identity=runtime same-tmdbId put-gate, normalize-rebuild=title-rule change re-merges rows that now share a key. Each fold orphans the victim's title|year freshness, so rate() is the re-key re-enrichment load.")
    .labelNames("reason")
    .register(registry)

  private val splits = Counter.builder()
    .name("kinowo_worker_splits")
    .help("New movie rows spawned by un-merging since boot (a 1→N split counts N−1) — the inverse of a merge. Only a title-rule change (NormalizeRebuild) re-keys a row's slots onto distinct keys; each split-off is born fresh (no tmdbId) and re-resolves, so rate() is the un-merge re-enrichment load. Pairs with kinowo_worker_merges_total{reason=\"normalize-rebuild\"}.")
    .register(registry)

  private val readModelWrites = Counter.builder()
    .name("kinowo_worker_readmodel_writes")
    .help("Denormalised read-model documents (re)written since boot, by target (movie|screening) and op (upsert|delete). rate() is the reprojection churn the worker pushes through the web's web_movies/web_screenings change streams.")
    .labelNames("target", "op")
    .register(registry)

  private val readModelFilmsPruned = Counter.builder()
    .name("kinowo_worker_readmodel_films_pruned")
    .help("Films whose derived documents were removed from the read model during reconcile since boot — a source row that vanished or was re-keyed (its filmId changed). The event that can briefly 404 a film deep-link until the new key propagates to the web; pair with kinowo_worker_merges_total for the re-key cause.")
    .register(registry)

  private val changeEvents = Counter.builder()
    .name("kinowo_worker_movie_change_events")
    .help("Movie change-stream events the shared cursor consumed since boot, by op (insert|update|replace|delete). rate() is the change-stream volume the cache + read-model projector reproject from.")
    .labelNames("op")
    .register(registry)

  private val changeUpdateKinds = Counter.builder()
    .name("kinowo_worker_movie_change_update_kinds")
    .help("For UPDATE events, which field kind changed since boot: source_data=a cinema slot (scrape write), rating=a rating value/url, identity=tmdb/imdb id + resolution lifecycle, updated_at_only=a no-op that touched only updatedAt (a redundant-write canary — should stay ~0). A multi-field update counts under each kind it touched.")
    .labelNames("kind")
    .register(registry)

  private val writer = PrometheusTextFormatWriter.create()

  seed()

  /** Materialize every series at 0 so it exists from boot (no Grafana gaps). */
  private def seed(): Unit = {
    TaskType.all.foreach { t =>
      EnqueueResults.foreach(r => enqueued.labelValues(t.name, r))
      started.labelValues(t.name)
      Outcomes.foreach(o => finished.labelValues(t.name, o))
      duration.labelValues(t.name)
      waitingByType.labelValues(t.name).set(0.0)
      oldestWaitingAge.labelValues(t.name).set(0.0)
    }
    RatingSites.foreach(s => ratingFirstAttemptDelay.labelValues(s))
    QueueStates.foreach(s => queueDepth.labelValues(s).set(0.0))
    StagingStep.all.foreach(s => stagingMovies.labelValues(s.label).set(0.0))
    MergeReason.all.foreach(r => merges.labelValues(r.label))
    splits.inc(0.0) // materialize the single series at 0 so Grafana draws a continuous line
    ReadModelProjectionMetrics.Targets.foreach(t =>
      ReadModelProjectionMetrics.Ops.foreach(o => readModelWrites.labelValues(t, o)))
    readModelFilmsPruned.inc(0.0) // materialize the single series at 0 so Grafana draws a continuous line
    ChangeStreamMetrics.Ops.foreach(o => changeEvents.labelValues(o))
    ChangeStreamMetrics.Kinds.foreach(k => changeUpdateKinds.labelValues(k))
    poolSizeGauge.set(poolSize.toDouble)
    throttledGauge.set(0.0) // materialize at 0 so Grafana draws a continuous on/off line
  }

  // ── RatingLatencyMetrics ────────────────────────────────────────────────────
  def recordFirstRatingDelay(site: String, seconds: Double): Unit =
    ratingFirstAttemptDelay.labelValues(site).observe(math.max(0.0, seconds))

  /** Each absorbed victim row is one increment under its fold's reason. */
  def recordMerge(reason: MergeReason, victims: Int): Unit =
    if (victims > 0) merges.labelValues(reason.label).inc(victims.toDouble)

  /** Each new row spawned by an un-merge is one increment (a 1→N split = N−1). */
  def recordSplit(fragments: Int): Unit =
    if (fragments > 0) splits.inc(fragments.toDouble)

  // ── ReadModelProjectionMetrics ──────────────────────────────────────────────
  def recordWrite(target: String, op: String, count: Int): Unit =
    if (count > 0) readModelWrites.labelValues(target, op).inc(count.toDouble)

  def recordFilmPruned(count: Int): Unit =
    if (count > 0) readModelFilmsPruned.inc(count.toDouble)

  // ── ChangeStreamMetrics ─────────────────────────────────────────────────────
  def recordEvent(op: String): Unit           = changeEvents.labelValues(op).inc()
  def recordUpdateKind(kind: String): Unit     = changeUpdateKinds.labelValues(kind).inc()

  def recordEnqueue(taskType: TaskType, result: String): Unit =
    enqueued.labelValues(taskType.name, result).inc()

  def onStarted(task: Task): Unit =
    started.labelValues(task.taskType.name).inc()

  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = {
    finished.labelValues(task.taskType.name, outcome).inc()
    // Only fully-worked tasks contribute to the duration histogram — a Skipped
    // task only paid a freshness check, and a Reschedule/failure didn't finish.
    if (outcome == Outcome.Done) duration.labelValues(task.taskType.name).observe(handleMillis / 1000.0)
  }

  /** Refresh the queue + staging gauges from live samples and render the full
   *  exposition. Called from the worker's `/metrics` handler on each Fly scrape;
   *  `stagingByStep` comes from `StagingReaper.stepCounts()`, `throttled` from the
   *  live `ScrapeThrottleSignal` the reapers consult. */
  def scrape(snapshot: QueueSnapshot, stagingByStep: Map[StagingStep, Int], now: Instant, throttled: Boolean = false): String = {
    refreshQueueGauges(snapshot, now)
    StagingStep.all.foreach(s => stagingMovies.labelValues(s.label).set(stagingByStep.getOrElse(s, 0).toDouble))
    throttledGauge.set(if (throttled) 1.0 else 0.0)
    val out = new ByteArrayOutputStream()
    writer.write(out, registry.scrape())
    out.toString("UTF-8")
  }

  private def refreshQueueGauges(snapshot: QueueSnapshot, now: Instant): Unit = {
    queueDepth.labelValues(TaskState.Waiting).set(snapshot.counts.getOrElse(TaskState.Waiting, 0L).toDouble)
    queueDepth.labelValues(TaskState.WorkedOn).set(snapshot.counts.getOrElse(TaskState.WorkedOn, 0L).toDouble)

    val waiting = snapshot.active.filter(_.state == TaskState.Waiting).groupBy(_.taskType)
    TaskType.all.foreach { t =>
      val rows = waiting.getOrElse(t.name, Nil)
      waitingByType.labelValues(t.name).set(rows.size.toDouble)
      val age = rows.map(_.submittedAt).minOption
        .map(oldest => math.max(0L, now.getEpochSecond - oldest.getEpochSecond).toDouble)
        .getOrElse(0.0)
      oldestWaitingAge.labelValues(t.name).set(age)
    }
  }
}

object WorkerTaskMetrics {
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
