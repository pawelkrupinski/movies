package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.freshness.FreshnessKind
import services.movies.{CacheSyncMetrics, RepositoryWriteMetrics, ChangeStreamLiveness, ChangeStreamMetrics, ListingIntakeMetrics, ScreeningsMetrics, SideCollectionChangeMetrics}
import services.readmodel.ReadModelProjectionMetrics
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
 * `TaskObserver` / `ReadModelProjectionMetrics` / … traits) stay unchanged.
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
  extends TaskObserver with ReadModelProjectionMetrics with RatingLatencyMetrics with ScreeningsMetrics with CacheSyncMetrics with ListingIntakeMetrics with RepositoryWriteMetrics with services.readmodel.DecodeFailureMetrics with services.tasks.ChunkPageMemoMetrics {

  // ── RatingLatencyMetrics ────────────────────────────────────────────────────
  def recordFirstRatingDelay(site: String, seconds: Double): Unit = series.tasks.recordFirstRatingDelay(countryCode, site, seconds)

  // ── ReadModelProjectionMetrics ──────────────────────────────────────────────
  def recordWrite(target: String, op: String, count: Int): Unit = series.readModel.recordWrite(countryCode, target, op, count)
  def recordFilmPruned(reason: String, count: Int): Unit        = series.readModel.recordFilmPruned(countryCode, reason, count)
  def recordCardRetired(reason: String): Unit                   = series.readModel.recordCardRetired(countryCode, reason)
  def recordDrift(cause: String, documents: Int): Unit          = series.readModel.recordDrift(countryCode, cause, documents)
  def recordUnlistedVenues(rows: Int, withheld: Boolean): Unit  = series.readModel.recordUnlistedVenues(countryCode, rows, withheld)
  def recordCatchUp(rows: Int): Unit                            = series.readModel.recordCatchUp(countryCode, rows)
  def recordCardWrite(changed: Set[String]): Unit               = series.readModel.recordCardWrite(countryCode, changed)
  def recordProject(trigger: ReadModelProjectionMetrics.ProjectTrigger, wallSeconds: Double, cpuSeconds: Double): Unit =
    series.readModel.recordProject(countryCode, trigger, wallSeconds, cpuSeconds)
  def recordWriteBurst(trigger: ReadModelProjectionMetrics.ProjectTrigger, seconds: Double): Unit =
    series.readModel.recordWriteBurst(countryCode, trigger, seconds)
  def recordMetadataProjection(trigger: ReadModelProjectionMetrics.ProjectTrigger, reused: Boolean): Unit =
    series.readModel.recordMetadataProjection(countryCode, trigger, reused)
  def recordVenueProjection(rebuilt: Int, reused: Int): Unit     = series.readModel.recordVenueProjection(countryCode, rebuilt, reused)
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit = series.readModel.recordReconcileSweep(countryCode, kind, didWork)
  def recordHeal(trigger: String, rows: Int): Unit              = series.readModel.recordHeal(countryCode, trigger, rows)

  // ── CacheSyncMetrics ────────────────────────────────────────────────────────
  def recordRehydrate(changedUpserts: Int, deletes: Int): Unit = series.corpusWrites.recordRehydrate(countryCode, changedUpserts, deletes)

  // ── ListingIntakeMetrics ────────────────────────────────────────────────────
  def recordGuardVerdict(guard: String, verdict: String): Unit = series.corpusWrites.recordScrapeGuardVerdict(countryCode, guard, verdict)
  def recordWriteSkipped(reason: String): Unit                 = series.corpusWrites.recordScrapeWriteSkipped(countryCode, reason)

  // ── RepositoryWriteMetrics ──────────────────────────────────────────────────
  def recordWriteFailed(collection: String, op: String, exception: String): Unit =
    series.corpusWrites.recordRepositoryWriteFailed(countryCode, collection, op, exception)

  // ── ChunkPageMemoMetrics ────────────────────────────────────────────────────
  def recordPage(outcome: String): Unit = series.tasks.recordChunkPage(countryCode, outcome)

  // ── DecodeFailureMetrics ────────────────────────────────────────────────────
  def recordDecodeFailure(collection: String): Unit = series.corpusWrites.recordDecodeFailure(countryCode, collection)

  // ── ChangeStreamMetrics for `movies` ────────────────────────────────────────
  // A separate object rather than a direct mixin: `ChangeStreamMetrics.recordCoalescedChange`
  // and `ScreeningsMetrics.recordCoalescedChange` erase to the same signature, so one class
  // cannot implement both directly — the same reason `slotsChangeMetrics` below is its own
  // object rather than a third mixin.
  val movieChangeMetrics: ChangeStreamMetrics = new ChangeStreamMetrics {
    def recordEvent(op: String): Unit        = series.changeStreams.recordEvent(countryCode, op)
    def recordUpdateKind(kind: String): Unit = series.changeStreams.recordUpdateKind(countryCode, kind)
    def recordCoalescedChange(): Unit        = series.changeStreams.recordMovieCoalesced(countryCode)
    override def recordApply(path: String, reason: String): Unit = series.changeStreams.recordChangeApply(countryCode, path, reason)
    override def recordVenueDecline(reason: String): Unit       = series.changeStreams.recordVenueDecline(countryCode, reason)
  }

  // ── ScreeningsMetrics ───────────────────────────────────────────────────────
  def recordChangeEvent(op: String): Unit = series.changeStreams.recordScreeningsChangeEvent(countryCode, op)
  def recordWrite(outcome: String, count: Int): Unit = series.changeStreams.recordScreeningsWrite(countryCode, outcome, count)
  def recordCoalescedChange(): Unit = series.changeStreams.recordScreeningsCoalesced(countryCode)

  // ── SideCollectionChangeMetrics for `movie_slots` ───────────────────────────
  // A separate object rather than a third mixin: the slots cursor answers the same two
  // questions as the screenings one, so its trait has the same two methods, and one class
  // cannot route the same signature to two counters.
  val slotsChangeMetrics: SideCollectionChangeMetrics = new SideCollectionChangeMetrics {
    def recordChangeEvent(op: String): Unit = series.changeStreams.recordSlotsChangeEvent(countryCode, op)
    def recordCoalescedChange(): Unit       = series.changeStreams.recordSlotsCoalesced(countryCode)
  }

  // ── Task lifecycle ──────────────────────────────────────────────────────────
  def recordEnqueue(taskType: TaskType, result: String): Unit = series.tasks.recordEnqueue(countryCode, taskType, result)
  def onStarted(task: Task): Unit                             = series.tasks.onStarted(countryCode, task)
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = series.tasks.onFinished(countryCode, task, outcome, handleMillis)
}

object WorkerTaskMetrics {

  /** The per-country queue sample [[Series.scrape]] folds into the gauges
   *  for one country on each scrape. `snapshot` is that country's live queue
   *  monitor — None when its read FAILED, which holds its gauges at their last reading — and
   *  `changeStreamLiveness` the repository's record of when each change-stream cursor
   *  last delivered and what it has handed the apply thread that has not been applied yet —
   *  read at scrape time, so a silent cursor's age and a stuck apply's lag keep climbing. */
  case class CountryQueueSample(countryCode: String, snapshot: Option[QueueSnapshot], changeStreamLiveness: ChangeStreamLiveness)

  object CountryQueueSample extends play.api.Logging {
    def apply(countryCode: String, snapshot: QueueSnapshot, changeStreamLiveness: ChangeStreamLiveness): CountryQueueSample =
      CountryQueueSample(countryCode, Some(snapshot), changeStreamLiveness)

    /** Take one country's sample from its live reads. The queue read THROWS on a failure
     *  (an unreadable queue is not an empty one), and the worker renders its whole
     *  exposition — every country, the JVM, every counter — in one pass that keeps the last
     *  good bytes when it throws: a read escaping here froze every series the worker exports
     *  for as long as it kept failing. A failed read holds only its own gauges. */
    def read(countryCode: String, snapshot: => QueueSnapshot, changeStreamLiveness: ChangeStreamLiveness): CountryQueueSample = {
      def held[A](what: String, read: => A): Option[A] =
        scala.util.Try(read).fold(e => {
          logger.warn(s"metrics: $countryCode $what read failed, holding its gauges at their last reading: ${e.getMessage}"); None
        }, Some(_))
      CountryQueueSample(countryCode, held("queue", snapshot), changeStreamLiveness)
    }
  }

  /**
   * The registered-once metric objects, SHARED across every country's
   * [[WorkerTaskMetrics]] facade, one family of series per part of the worker. Registered on the
   * single worker registry so ONE `/metrics` scrape exposes every country's task pipeline; each
   * series carries a leading `country` label so the countries never collide. Seeded to 0 for the
   * full cartesian of (country × every other label value) so no Grafana gap opens before a given
   * (country, type, outcome) first fires.
   */
  class Series(poolSize: Int, countryCodes: Seq[String], registry: PrometheusRegistry = new PrometheusRegistry()) {
    val tasks: TaskPipelineSeries         = new TaskPipelineSeries(poolSize, countryCodes, registry)
    val changeStreams: ChangeStreamSeries = new ChangeStreamSeries(countryCodes, registry)
    val readModel: ReadModelSeries        = new ReadModelSeries(countryCodes, registry)
    val corpusWrites: CorpusWriteSeries   = new CorpusWriteSeries(countryCodes, registry)

    /** Refresh each country's queue gauges from its live sample and
     *  render the full exposition (task pipeline + census + JVM, all on the shared
     *  registry). Called from the worker's `/metrics` handler on each Fly scrape
     *  with one [[CountryQueueSample]] per running country. */
    def scrape(samples: Seq[CountryQueueSample], now: Instant): String = {
      samples.foreach { s =>
        s.snapshot.foreach(tasks.refreshQueueGauges(s.countryCode, _, now))
        changeStreams.refreshLiveness(s.countryCode, s.changeStreamLiveness, now)
      }
      PrometheusExposition.render(registry)
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

  private[metrics] val QueueStates: Seq[String] = Seq(TaskState.Waiting, TaskState.WorkedOn)

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
