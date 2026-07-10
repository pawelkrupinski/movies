package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerTaskMetrics.CountryQueueSample
import services.movies.MergeReason
import services.staging.StagingStep
import services.tasks.{QueueSnapshot, Task, TaskState, TaskSummary, TaskType}

import java.time.Instant

/**
 * Locks the worker's Prometheus task-pipeline exposition: the four lifecycle
 * counters/histogram (enqueued → started → finished + handler duration) and the
 * queue gauges refreshed from a live snapshot. The histogram's "fully-worked
 * only" rule and the per-type seeding (every series exists from boot) are the
 * load-bearing behaviour, so they're asserted directly.
 *
 * Multi-country: one JVM runs a wiring per country against a single shared
 * `Series`; every series carries a leading `country` label so the countries never
 * collide on the one `/metrics` endpoint. The country-label presence + isolation
 * are asserted explicitly below.
 */
class WorkerTaskMetricsSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-06-19T10:00:00Z")

  private def task(t: TaskType) = Task("id", t, "dedup", Map.empty, attempts = 1)

  private def summary(taskType: TaskType, state: String, submittedAt: Instant) =
    TaskSummary("id", taskType.name, "dedup", state, submittedAt, attempts = 1,
      workerId = None, leaseExpiresAt = None, lastError = None)

  private val emptySnapshot = QueueSnapshot(Map.empty, Nil)
  private val noStaging      = Map.empty[StagingStep, Int]

  /** A single-country ("pl") series + its facade — the common case. */
  private def newPl(): (WorkerTaskMetrics, WorkerTaskMetrics.Series) = {
    val series = new WorkerTaskMetrics.Series(poolSize = 4, countryCodes = Seq("pl"))
    (new WorkerTaskMetrics("pl", series), series)
  }

  private def scrapePl(series: WorkerTaskMetrics.Series,
                       snapshot: QueueSnapshot = emptySnapshot,
                       staging: Map[StagingStep, Int] = noStaging,
                       throttled: Boolean = false): String =
    series.scrape(Seq(CountryQueueSample("pl", snapshot, staging, throttled)), now)

  it should "tag every task-pipeline series with the emitting country" in {
    val (m, series) = newPl()
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.onStarted(task(TaskType.ResolveTmdb))

    val out = scrapePl(series)

    // The label is present and carries "pl" on representative counter + gauge series.
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 1""")
    out should include ("""kinowo_worker_tasks_started_total{country="pl",task_type="ResolveTmdb"} 1""")
    out should include ("""kinowo_worker_throttled{country="pl"} 0""")
  }

  it should "keep two countries' series separate on one shared registry" in {
    val series = new WorkerTaskMetrics.Series(poolSize = 4, countryCodes = Seq("pl", "uk"))
    val pl = new WorkerTaskMetrics("pl", series)
    val uk = new WorkerTaskMetrics("uk", series)

    pl.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    pl.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    uk.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)

    val out = series.scrape(Seq(
      CountryQueueSample("pl", emptySnapshot, noStaging, throttled = true),
      CountryQueueSample("uk", emptySnapshot, noStaging, throttled = false)), now)

    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_tasks_enqueued_total{country="uk",result="added",task_type="ScrapeCinema"} 1""")
    // Per-country throttle: pl backing off, uk not.
    out should include ("""kinowo_worker_throttled{country="pl"} 1""")
    out should include ("""kinowo_worker_throttled{country="uk"} 0""")
  }

  it should "expose kinowo_worker_throttled as 0/1 from the scrape's throttle flag" in {
    val (_, series) = newPl()
    scrapePl(series)                       should include ("""kinowo_worker_throttled{country="pl"} 0""")
    scrapePl(series, throttled = true)     should include ("""kinowo_worker_throttled{country="pl"} 1""")
    scrapePl(series, throttled = false)    should include ("""kinowo_worker_throttled{country="pl"} 0""")
  }

  "WorkerTaskMetrics" should "count enqueues by type and result" in {
    val (m, series) = newPl()
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Deduped)

    val out = scrapePl(series)

    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="deduped",task_type="ScrapeCinema"} 1""")
  }

  it should "count started and finished tasks, split by outcome" in {
    val (m, series) = newPl()
    m.onStarted(task(TaskType.ResolveTmdb))
    m.onFinished(task(TaskType.ResolveTmdb), WorkerTaskMetrics.Outcome.Done, handleMillis = 500)
    m.onStarted(task(TaskType.ResolveTmdb))
    m.onFinished(task(TaskType.ResolveTmdb), WorkerTaskMetrics.Outcome.Skipped, handleMillis = 5)

    val out = scrapePl(series)

    out should include ("""kinowo_worker_tasks_started_total{country="pl",task_type="ResolveTmdb"} 2""")
    out should include ("""kinowo_worker_tasks_finished_total{country="pl",outcome="done",task_type="ResolveTmdb"} 1""")
    out should include ("""kinowo_worker_tasks_finished_total{country="pl",outcome="skipped",task_type="ResolveTmdb"} 1""")
  }

  it should "record handler duration ONLY for fully-worked (done) tasks" in {
    val (m, series) = newPl()
    m.onFinished(task(TaskType.EnrichDetails), WorkerTaskMetrics.Outcome.Done, handleMillis = 1500)
    m.onFinished(task(TaskType.EnrichDetails), WorkerTaskMetrics.Outcome.Skipped, handleMillis = 9000)

    val out = scrapePl(series)

    // Only the 1.5s done observation counts — the skipped one is excluded.
    out should include ("""kinowo_worker_task_duration_seconds_count{country="pl",task_type="EnrichDetails"} 1""")
    out should include ("""kinowo_worker_task_duration_seconds_sum{country="pl",task_type="EnrichDetails"} 1.5""")
    // 1.5s falls in the le=2.0 bucket but not le=1.0 (le is the last label, per convention).
    out should include ("""kinowo_worker_task_duration_seconds_bucket{country="pl",task_type="EnrichDetails",le="2.0"} 1""")
    out should include ("""kinowo_worker_task_duration_seconds_bucket{country="pl",task_type="EnrichDetails",le="1.0"} 0""")
  }

  it should "expose queue depth, per-type waiting, and oldest-waiting age from the snapshot" in {
    val (_, series) = newPl()
    val snapshot = QueueSnapshot(
      counts = Map(TaskState.Waiting -> 5L, TaskState.WorkedOn -> 2L),
      active = Seq(
        summary(TaskType.ScrapeCinema, TaskState.Waiting, now.minusSeconds(30)),
        summary(TaskType.ScrapeCinema, TaskState.Waiting, now.minusSeconds(90)),
        summary(TaskType.EnrichDetails, TaskState.WorkedOn, now.minusSeconds(10))
      ))

    val out = scrapePl(series, snapshot)

    out should include ("""kinowo_worker_queue_depth{country="pl",state="waiting"} 5""")
    out should include ("""kinowo_worker_queue_depth{country="pl",state="worked_on"} 2""")
    out should include ("""kinowo_worker_queue_waiting_by_type{country="pl",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_queue_oldest_waiting_age_seconds{country="pl",task_type="ScrapeCinema"} 90""")
    // The pool is a single shared budget across countries, so pool_size is unlabelled.
    out should include ("kinowo_worker_pool_size 4")
  }

  it should "seed every task type to 0 so the series exists from boot" in {
    val (_, series) = newPl()
    val out = scrapePl(series)

    // A type that never ran still appears at 0 (no Grafana gap).
    out should include ("""kinowo_worker_tasks_started_total{country="pl",task_type="StagingFold"} 0""")
    out should include ("""kinowo_worker_tasks_finished_total{country="pl",outcome="done",task_type="RtRating"} 0""")
  }

  it should "expose staging movie counts by step, seeding unused steps to 0" in {
    val (_, series) = newPl()
    val staging = Map[StagingStep, Int](StagingStep.Detail -> 3, StagingStep.Fold -> 1)

    val out = scrapePl(series, staging = staging)

    out should include ("""kinowo_worker_staging_movies{country="pl",step="detail"} 3""")
    out should include ("""kinowo_worker_staging_movies{country="pl",step="fold"} 1""")
    // A step with nobody waiting still appears at 0.
    out should include ("""kinowo_worker_staging_movies{country="pl",step="resolve_tmdb"} 0""")
    out should include ("""kinowo_worker_staging_movies{country="pl",step="resolve_imdb"} 0""")
  }

  it should "count movie-row merges by reason, summing victims and seeding unused reasons to 0" in {
    val (m, series) = newPl()
    m.recordMerge(MergeReason.Canonicalize, 2)
    m.recordMerge(MergeReason.Canonicalize, 1)
    m.recordMerge(MergeReason.TmdbIdentity, 1)
    m.recordMerge(MergeReason.NormalizeRebuild, 2)
    m.recordMerge(MergeReason.ResolvedSettle, 0)  // a no-victim fold contributes nothing

    val out = scrapePl(series)

    out should include ("""kinowo_worker_merges_total{country="pl",reason="canonicalize"} 3""")
    out should include ("""kinowo_worker_merges_total{country="pl",reason="tmdb-identity"} 1""")
    out should include ("""kinowo_worker_merges_total{country="pl",reason="normalize-rebuild"} 2""")
    // Seeded so the series exists from boot; the 0-victim call left it at 0.
    out should include ("""kinowo_worker_merges_total{country="pl",reason="resolved-settle"} 0""")
  }

  it should "count movie-row splits, summing fragments and seeding the series to 0" in {
    val (m, series) = newPl()
    m.recordSplit(2)  // a 1→3 un-merge spawned 2 new rows
    m.recordSplit(1)
    m.recordSplit(0)  // a no-fragment split contributes nothing

    scrapePl(series) should include ("""kinowo_worker_splits_total{country="pl"} 3""")
  }

  it should "seed the splits series to 0 so it exists from boot" in {
    val (_, series) = newPl()
    scrapePl(series) should include ("""kinowo_worker_splits_total{country="pl"} 0""")
  }

  it should "observe the TMDB-resolved → first-rating-attempt delay per site, seeding all four" in {
    val (m, series) = newPl()
    m.recordFirstRatingDelay("imdb", 300.0)

    val out = scrapePl(series)

    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="imdb"} 1""")
    // Seeded so every site's series exists from boot even before its first observation.
    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="fw"} 0""")
    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="rt"} 0""")
    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="mc"} 0""")
  }
}
