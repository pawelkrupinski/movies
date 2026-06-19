package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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
 */
class WorkerTaskMetricsSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-06-19T10:00:00Z")

  private def task(t: TaskType) = Task("id", t, "dedup", Map.empty, attempts = 1)

  private def summary(taskType: TaskType, state: String, submittedAt: Instant) =
    TaskSummary("id", taskType.name, "dedup", state, submittedAt, attempts = 1,
      workerId = None, leaseExpiresAt = None, lastError = None)

  private val emptySnapshot = QueueSnapshot(Map.empty, Nil)
  private val noStaging      = Map.empty[StagingStep, Int]

  "WorkerTaskMetrics" should "count enqueues by type and result" in {
    val m = new WorkerTaskMetrics(poolSize = 4)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Deduped)

    val out = m.scrape(emptySnapshot, noStaging, now)

    out should include ("""kinowo_worker_tasks_enqueued_total{result="added",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_tasks_enqueued_total{result="deduped",task_type="ScrapeCinema"} 1""")
  }

  it should "count started and finished tasks, split by outcome" in {
    val m = new WorkerTaskMetrics(poolSize = 4)
    m.onStarted(task(TaskType.ResolveTmdb))
    m.onFinished(task(TaskType.ResolveTmdb), WorkerTaskMetrics.Outcome.Done, handleMillis = 500)
    m.onStarted(task(TaskType.ResolveTmdb))
    m.onFinished(task(TaskType.ResolveTmdb), WorkerTaskMetrics.Outcome.Skipped, handleMillis = 5)

    val out = m.scrape(emptySnapshot, noStaging, now)

    out should include ("""kinowo_worker_tasks_started_total{task_type="ResolveTmdb"} 2""")
    out should include ("""kinowo_worker_tasks_finished_total{outcome="done",task_type="ResolveTmdb"} 1""")
    out should include ("""kinowo_worker_tasks_finished_total{outcome="skipped",task_type="ResolveTmdb"} 1""")
  }

  it should "record handler duration ONLY for fully-worked (done) tasks" in {
    val m = new WorkerTaskMetrics(poolSize = 4)
    m.onFinished(task(TaskType.EnrichDetails), WorkerTaskMetrics.Outcome.Done, handleMillis = 1500)
    m.onFinished(task(TaskType.EnrichDetails), WorkerTaskMetrics.Outcome.Skipped, handleMillis = 9000)

    val out = m.scrape(emptySnapshot, noStaging, now)

    // Only the 1.5s done observation counts — the skipped one is excluded.
    out should include ("""kinowo_worker_task_duration_seconds_count{task_type="EnrichDetails"} 1""")
    out should include ("""kinowo_worker_task_duration_seconds_sum{task_type="EnrichDetails"} 1.5""")
    // 1.5s falls in the le=2.0 bucket but not le=1.0 (le is the last label, per convention).
    out should include ("""kinowo_worker_task_duration_seconds_bucket{task_type="EnrichDetails",le="2.0"} 1""")
    out should include ("""kinowo_worker_task_duration_seconds_bucket{task_type="EnrichDetails",le="1.0"} 0""")
  }

  it should "expose queue depth, per-type waiting, and oldest-waiting age from the snapshot" in {
    val m = new WorkerTaskMetrics(poolSize = 4)
    val snapshot = QueueSnapshot(
      counts = Map(TaskState.Waiting -> 5L, TaskState.WorkedOn -> 2L),
      active = Seq(
        summary(TaskType.ScrapeCinema, TaskState.Waiting, now.minusSeconds(30)),
        summary(TaskType.ScrapeCinema, TaskState.Waiting, now.minusSeconds(90)),
        summary(TaskType.EnrichDetails, TaskState.WorkedOn, now.minusSeconds(10))
      ))

    val out = m.scrape(snapshot, noStaging, now)

    out should include ("""kinowo_worker_queue_depth{state="waiting"} 5""")
    out should include ("""kinowo_worker_queue_depth{state="worked_on"} 2""")
    out should include ("""kinowo_worker_queue_waiting_by_type{task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_queue_oldest_waiting_age_seconds{task_type="ScrapeCinema"} 90""")
    out should include ("kinowo_worker_pool_size 4")
  }

  it should "seed every task type to 0 so the series exists from boot" in {
    val out = new WorkerTaskMetrics(poolSize = 4).scrape(emptySnapshot, noStaging, now)

    // A type that never ran still appears at 0 (no Grafana gap).
    out should include ("""kinowo_worker_tasks_started_total{task_type="StagingFold"} 0""")
    out should include ("""kinowo_worker_tasks_finished_total{outcome="done",task_type="RtRating"} 0""")
  }

  it should "expose staging movie counts by step, seeding unused steps to 0" in {
    val m = new WorkerTaskMetrics(poolSize = 4)
    val staging = Map[StagingStep, Int](StagingStep.Detail -> 3, StagingStep.Fold -> 1)

    val out = m.scrape(emptySnapshot, staging, now)

    out should include ("""kinowo_worker_staging_movies{step="detail"} 3""")
    out should include ("""kinowo_worker_staging_movies{step="fold"} 1""")
    // A step with nobody waiting still appears at 0.
    out should include ("""kinowo_worker_staging_movies{step="resolve_tmdb"} 0""")
    out should include ("""kinowo_worker_staging_movies{step="resolve_imdb"} 0""")
  }

  it should "count movie-row merges by reason, summing victims and seeding unused reasons to 0" in {
    val m = new WorkerTaskMetrics(poolSize = 4)
    m.recordMerge(MergeReason.Canonicalize, 2)
    m.recordMerge(MergeReason.Canonicalize, 1)
    m.recordMerge(MergeReason.TmdbIdentity, 1)
    m.recordMerge(MergeReason.ResolvedSettle, 0)  // a no-victim fold contributes nothing

    val out = m.scrape(emptySnapshot, noStaging, now)

    out should include ("""kinowo_worker_merges_total{reason="canonicalize"} 3""")
    out should include ("""kinowo_worker_merges_total{reason="tmdb-identity"} 1""")
    // Seeded so the series exists from boot; the 0-victim call left it at 0.
    out should include ("""kinowo_worker_merges_total{reason="resolved-settle"} 0""")
  }
}
