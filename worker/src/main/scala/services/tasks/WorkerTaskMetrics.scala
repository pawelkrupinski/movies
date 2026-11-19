package services.tasks

import services.metrics.ChangeStreamSeries
import services.metrics.CorpusWriteSeries
import services.metrics.PrometheusExposition
import services.metrics.ReadModelSeries
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.freshness.FreshnessKind
import services.movies.ChangeStreamLiveness

import java.time.Instant

/**
 * Prometheus instrumentation for the worker's task pipeline -- the worker-side
 * counterpart to the web app's uptime `/metrics` (controllers.MetricsController).
 *
 * It counts the full lifecycle of every deferred task (enqueue → claim → finish)
 * keyed by `TaskType`, times the work, and (refreshed from a live queue snapshot
 * at scrape time) exposes the current backlog. Built on the official Prometheus
 * Java client rather than a hand-rolled exposition; the fleet's Prometheus
 * scrapes it over the pod's NodePort, and Grafana charts the `rate(...)` of each
 * family per `task_type`.
 *
 * Multi-country: ONE JVM runs a [[modules.wiring.WorkerWiring]] per country against a
 * single shared [[WorkerTaskMetrics.Series]] (registered once on the shared
 * registry). Every series carries a `country` label so the two countries' rows
 * never collide on the single `/metrics` endpoint. Each wiring binds its country to
 * one family at a time, through the binder of that family's port:
 * [[TaskPipelineBinder]] (the queue and the worker), `services.metrics.ReadModelSeriesBinder`
 * (the projector) and `services.metrics.CorpusSeriesBinder` (the cache's and the
 * repositories' writes, and the change-stream cursors).
 *
 * Counters are monotonic since-boot totals — `rate()` handles the reset on each
 * worker reboot. Every `TaskType` (× every label value × every country) is
 * materialized to 0 at construction so its series exists from boot and Grafana
 * draws a continuous line rather than a gap for a type/outcome that hasn't
 * happened yet. The queue gauges are refreshed from a per-country `QueueSnapshot`
 * each `Series.scrape()`.
 */
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
   * country's binders, one family of series per part of the worker. Registered on the
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

  private[tasks] val QueueStates: Seq[String] = Seq(TaskState.Waiting, TaskState.WorkedOn)

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
