package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class TaskWorkerSpec extends AnyFlatSpec with Matchers {
  import TaskType._
  import TaskWorker.PollResult

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  private class RecordingHandler(val taskType: TaskType, outcome: HandlerOutcome) extends TaskHandler {
    var seen: List[Task] = Nil
    override def handle(task: Task): HandlerOutcome = { seen ::= task; outcome }
  }

  private def worker(q: TaskQueue, hs: Seq[TaskHandler]) =
    new TaskWorker(q, hs, processingTimeout = 5.minutes, pollInterval = 1.minute, poolSize = 4)

  "claimAndRun" should "claim a waiting task, run its handler, and tombstone it on Done" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", Map("cinema" -> "x"), submittedAt = t0)
    val h = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    worker(q, Seq(h)).claimAndRun("w0") shouldBe PollResult.Completed
    h.seen.map(_.dedupKey) shouldBe List("scrape|x")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  it should "tombstone on Skipped just like Done" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|x", submittedAt = t0)
    worker(q, Seq(new RecordingHandler(ImdbRating, HandlerOutcome.Skipped))).claimAndRun("w0") shouldBe PollResult.Completed
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
  }

  it should "return a task to waiting on Reschedule" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|x", submittedAt = t0)
    worker(q, Seq(new RecordingHandler(ImdbRating, HandlerOutcome.Reschedule(Some("later"))))).claimAndRun("w0") shouldBe PollResult.Returned
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
    worker(q, Seq(boom)).claimAndRun("w0") shouldBe PollResult.Returned
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "return a task to waiting when no handler is registered for its type" in {
    val q = new InMemoryTaskQueue
    q.enqueue(McRating, "mc|x", submittedAt = t0)
    worker(q, Seq.empty).claimAndRun("w0") shouldBe PollResult.Returned
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "be idle when the queue is empty" in {
    worker(new InMemoryTaskQueue, Seq.empty).claimAndRun("w0") shouldBe PollResult.Idle
  }

  it should "claim only one task per call, so the pool processes one at a time per worker" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    q.enqueue(ImdbRating, "imdb|y", submittedAt = t0.plusSeconds(1))
    val scrape = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    val imdb   = new RecordingHandler(ImdbRating, HandlerOutcome.Done)
    val w      = worker(q, Seq(scrape, imdb))

    // First call takes exactly one task (the oldest); the second is still waiting.
    w.claimAndRun("w0") shouldBe PollResult.Completed
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L

    // A second call drains the rest; each task routed to its own handler.
    w.claimAndRun("w0") shouldBe PollResult.Completed
    w.claimAndRun("w0") shouldBe PollResult.Idle
    scrape.seen.map(_.dedupKey) shouldBe List("scrape|x")
    imdb.seen.map(_.dedupKey) shouldBe List("imdb|y")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 2L
  }

  it should "let two workers each hold one of two waiting tasks at once" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    q.enqueue(ScrapeCinema, "scrape|y", submittedAt = t0.plusSeconds(1))
    // Two distinct worker ids claim concurrently; the second can't steal the
    // first's leased task, so each ends up on a different one.
    val a = q.claim("w0", 5.minutes).get
    val b = q.claim("w1", 5.minutes).get
    Set(a.dedupKey, b.dedupKey) shouldBe Set("scrape|x", "scrape|y")
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 2L
  }

  it should "time out a task stuck in processing past the timeout and reprocess it" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    // A worker claims it then "crashes": never completes, lease expires in 1ms.
    q.claim("crashed-worker", 1.millisecond).get
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 1L
    Thread.sleep(5) // let the 1ms lease expire in real time

    q.reapExpiredLeases() shouldBe 1 // reaped back to waiting (the shared reaper's job)
    val h = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    worker(q, Seq(h)).claimAndRun("w0") shouldBe PollResult.Completed
    h.seen.map(_.dedupKey) shouldBe List("scrape|x")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 0L
  }

  "backoffMillis" should "ramp exponentially from pollInterval and cap at maxIdleInterval" in {
    // A quiet pool must stop polling the shared Mongo every few seconds; the idle
    // sleep doubles per consecutive empty claim and caps, so the floor decays.
    val w = new TaskWorker(new InMemoryTaskQueue, Seq.empty,
      pollInterval = 5.seconds, maxIdleInterval = 30.seconds, poolSize = 1)
    w.backoffMillis(1) shouldBe 5000L   // first idle → base
    w.backoffMillis(2) shouldBe 10000L  // ×2
    w.backoffMillis(3) shouldBe 20000L  // ×4
    w.backoffMillis(4) shouldBe 30000L  // ×8 = 40s, capped at 30s
    w.backoffMillis(50) shouldBe 30000L // stays capped, no overflow
  }
}
