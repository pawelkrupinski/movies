package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class TaskWorkerSpec extends AnyFlatSpec with Matchers {
  import TaskType._

  // Runs each submitted task inline, so poll() completes its handlers before
  // returning — no sleeps, no races in the assertions.
  private val inlineEc: ExecutionContext = ExecutionContext.fromExecutor((r: Runnable) => r.run())

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  private class RecordingHandler(val taskType: TaskType, outcome: HandlerOutcome) extends TaskHandler {
    var seen: List[Task] = Nil
    override def handle(task: Task): HandlerOutcome = { seen ::= task; outcome }
  }

  private def worker(q: TaskQueue, hs: Seq[TaskHandler]) =
    new TaskWorker(q, hs, inlineEc, processingTimeout = 5.minutes, pollInterval = 1.minute, maxPerTick = 50)

  "poll" should "claim a waiting task, run its handler, and tombstone it on Done" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", Map("cinema" -> "x"), submittedAt = t0)
    val h = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    worker(q, Seq(h)).poll() shouldBe 1
    h.seen.map(_.dedupKey) shouldBe List("scrape|x")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  it should "tombstone on Skipped just like Done" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|x", submittedAt = t0)
    worker(q, Seq(new RecordingHandler(ImdbRating, HandlerOutcome.Skipped))).poll() shouldBe 1
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
  }

  it should "return a task to waiting on Reschedule" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|x", submittedAt = t0)
    worker(q, Seq(new RecordingHandler(ImdbRating, HandlerOutcome.Reschedule(Some("later"))))).poll() shouldBe 1
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 0L
  }

  it should "return a task to waiting when the handler throws" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|x", submittedAt = t0)
    val boom = new TaskHandler {
      val taskType = ImdbRating
      def handle(task: Task) = throw new RuntimeException("kaboom")
    }
    worker(q, Seq(boom)).poll() shouldBe 1
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "return a task to waiting when no handler is registered for its type" in {
    val q = new InMemoryTaskQueue
    q.enqueue(McRating, "mc|x", submittedAt = t0)
    worker(q, Seq.empty).poll() shouldBe 1
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "drain multiple waiting tasks in one tick and route each to its handler" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    q.enqueue(ImdbRating, "imdb|y", submittedAt = t0.plusSeconds(1))
    val scrape = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    val imdb   = new RecordingHandler(ImdbRating, HandlerOutcome.Done)
    worker(q, Seq(scrape, imdb)).poll() shouldBe 2
    scrape.seen.map(_.dedupKey) shouldBe List("scrape|x")
    imdb.seen.map(_.dedupKey) shouldBe List("imdb|y")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 2L
  }

  it should "do nothing when the queue is empty" in {
    new TaskWorker(new InMemoryTaskQueue, Seq.empty, inlineEc).poll() shouldBe 0
  }

  it should "time out a task stuck in processing past the timeout and reprocess it" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    // A worker claims it then "crashes": never completes, lease expires in 1ms.
    q.claim("crashed-worker", 1.millisecond).get
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 1L
    Thread.sleep(5) // let the 1ms lease expire in real time

    val h = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    worker(q, Seq(h)).poll() shouldBe 1 // reaped back to waiting, then claimed + run
    h.seen.map(_.dedupKey) shouldBe List("scrape|x")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 0L
  }
}
