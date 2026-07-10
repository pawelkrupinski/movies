package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.StagingStep
import services.tasks.{InMemoryTaskQueue, QueueSnapshot, TaskQueue, TaskType}

import java.time.Instant

/** The decorator must meter the real enqueue outcome: a fresh task as `added`,
 *  a collapse onto an active dup as `deduped`. */
class MeteredTaskQueueSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-06-19T10:00:00Z")

  "MeteredTaskQueue" should "record added vs deduped enqueues" in {
    val series        = new WorkerTaskMetrics.Series(poolSize = 4, countryCodes = Seq("pl"))
    val metrics       = new WorkerTaskMetrics("pl", series)
    val queue: TaskQueue = new MeteredTaskQueue(new InMemoryTaskQueue, metrics)

    queue.enqueue(TaskType.ImdbRating, "film|2026")        // added
    queue.enqueue(TaskType.ImdbRating, "film|2026")        // dup of the active one → deduped

    val out = series.scrape(Seq(WorkerTaskMetrics.CountryQueueSample("pl", QueueSnapshot(Map.empty, Nil), Map.empty[StagingStep, Int], throttled = false)), now)
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ImdbRating"} 1""")
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="deduped",task_type="ImdbRating"} 1""")
  }
}
