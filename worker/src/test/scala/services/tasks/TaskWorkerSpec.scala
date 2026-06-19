package services.tasks

import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class TaskWorkerSpec extends AnyFlatSpec with Matchers with Eventually {
  import TaskType._
  import TaskWorker.PollResult

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  private class RecordingHandler(val taskType: TaskType, outcome: HandlerOutcome) extends TaskHandler {
    var seen: List[Task] = Nil
    override def handle(task: Task): HandlerOutcome = { seen ::= task; outcome }
  }

  private def worker(q: TaskQueue, hs: Seq[TaskHandler]) =
    new TaskWorker(q, hs, processingTimeout = 5.minutes, poolSize = 4)

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

  it should "hold a Rescheduled task back from immediate re-claim (per-task backoff)" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|x", submittedAt = t0)
    val w = worker(q, Seq(new RecordingHandler(ImdbRating, HandlerOutcome.Reschedule(Some("later")))))
    w.claimAndRun("w0") shouldBe PollResult.Returned
    // The worker released it with a backoff window (~5s for the first failure),
    // so an immediate re-claim at "now" finds nothing eligible…
    q.claim("w1", 1.minute) shouldBe None
    // …but it's still a Waiting task, claimable once the window has passed.
    q.claim("w1", 1.minute, Instant.now().plusSeconds(10)).map(_.dedupKey) shouldBe Some("imdb|x")
  }

  "TaskWorker.retryBackoffFor" should "ramp exponentially from 5s and cap at 30 minutes" in {
    TaskWorker.retryBackoffFor(1)   shouldBe 5.seconds
    TaskWorker.retryBackoffFor(2)   shouldBe 10.seconds
    TaskWorker.retryBackoffFor(3)   shouldBe 20.seconds
    TaskWorker.retryBackoffFor(10)  shouldBe 30.minutes  // capped
    TaskWorker.retryBackoffFor(100) shouldBe 30.minutes  // stays capped, no shift overflow
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

  "InMemoryTaskQueue.watchWaiting" should "ring on a fresh enqueue, not on a duplicate, and stop after close" in {
    val q     = new InMemoryTaskQueue
    val rings = new AtomicInteger(0)
    val handle = q.watchWaiting(() => { rings.incrementAndGet(); () }).get

    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0) shouldBe EnqueueResult.Added
    rings.get() shouldBe 1
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0) shouldBe EnqueueResult.Duplicate
    rings.get() shouldBe 1 // a no-op duplicate must not ring

    handle.close()
    q.enqueue(ImdbRating, "imdb|y", submittedAt = t0) shouldBe EnqueueResult.Added
    rings.get() shouldBe 1 // unsubscribed — no more rings
  }

  "TaskDoorbell" should "return at once when a ring already moved the generation past the snapshot (no lost wakeup)" in {
    val d     = new TaskDoorbell
    val since = d.generation
    d.ring() // ring lands in the gap between snapshot and park
    val started = System.nanoTime()
    d.awaitSince(since, 60000L) // must NOT block on the 60s timeout
    ((System.nanoTime() - started) / 1000000L) should be < 1000L
  }

  it should "wake a parked waiter when rung" in {
    val d     = new TaskDoorbell
    val since = d.generation
    @volatile var wokeAt = 0L
    val parked = new Thread(() => { d.awaitSince(since, 60000L); wokeAt = System.nanoTime() })
    parked.start()
    Thread.sleep(50) // let it park
    val rungAt = System.nanoTime()
    d.ring()
    parked.join(2000)
    parked.isAlive shouldBe false
    ((wokeAt - rungAt) / 1000000L) should be < 1000L
  }

  it should "time out and return when no ring arrives" in {
    val d       = new TaskDoorbell
    val started = System.nanoTime()
    d.awaitSince(d.generation, 80L)
    val elapsed = (System.nanoTime() - started) / 1000000L
    elapsed should be >= 60L  // waited ~the timeout (allow scheduler slack)
    elapsed should be < 2000L // but it did return
  }

  "a running pool" should "pick up a task pushed after start via the doorbell, without waiting out the idle backstop" in {
    val q = new InMemoryTaskQueue
    val h = new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)
    // idleBackstop a full minute: the ONLY way this task runs inside the
    // assertion window is the watchWaiting doorbell ringing the parked worker.
    val w = new TaskWorker(q, Seq(h), processingTimeout = 5.minutes,
      retryBackoff = 1.second, idleBackstop = 1.minute, poolSize = 1)
    w.start()
    try {
      Thread.sleep(100) // let the lone worker reach its idle park
      q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
      eventually(timeout(Span(3, Seconds)), interval(Span(20, Millis))) {
        q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
      }
      h.seen.map(_.dedupKey) shouldBe List("scrape|x")
    } finally w.stop()
  }

  "the TaskObserver" should "report a claim, then the mapped outcome for every handler result" in {
    import services.metrics.{TaskObserver, WorkerTaskMetrics}
    import WorkerTaskMetrics.Outcome

    class RecordingObserver extends TaskObserver {
      var started:  List[String]            = Nil
      var finished: List[(String, String)]  = Nil
      def onStarted(task: Task): Unit                                       = started ::= task.taskType.name
      def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = finished ::= (task.taskType.name -> outcome)
    }

    def observed(handlers: Seq[TaskHandler]): (List[String], List[(String, String)]) = {
      val q = new InMemoryTaskQueue
      q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
      val obs = new RecordingObserver
      new TaskWorker(q, handlers, processingTimeout = 5.minutes, poolSize = 4, observer = obs).claimAndRun("w0")
      (obs.started, obs.finished)
    }
    val throwing = new TaskHandler { val taskType = ScrapeCinema; def handle(task: Task): HandlerOutcome = throw new RuntimeException("boom") }

    observed(Seq(new RecordingHandler(ScrapeCinema, HandlerOutcome.Done)))         shouldBe (List("ScrapeCinema"), List("ScrapeCinema" -> Outcome.Done))
    observed(Seq(new RecordingHandler(ScrapeCinema, HandlerOutcome.Skipped)))      shouldBe (List("ScrapeCinema"), List("ScrapeCinema" -> Outcome.Skipped))
    observed(Seq(new RecordingHandler(ScrapeCinema, HandlerOutcome.Reschedule()))) shouldBe (List("ScrapeCinema"), List("ScrapeCinema" -> Outcome.Rescheduled))
    observed(Seq(throwing))                                                        shouldBe (List("ScrapeCinema"), List("ScrapeCinema" -> Outcome.Failed))
    observed(Seq.empty)                                                            shouldBe (List("ScrapeCinema"), List("ScrapeCinema" -> Outcome.NoHandler))
  }
}
