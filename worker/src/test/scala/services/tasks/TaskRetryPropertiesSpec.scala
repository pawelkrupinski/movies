package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.QueueResolveDispatcher
import tools.{MutableClock, RetryClassificationFailures}
import tools.contracts.RetryClassificationTable
import tools.contracts.RetryClassificationTable.Verdict

import java.time.{Duration, Instant}
import scala.concurrent.duration._

/**
 * The task queue's retry properties, each driven over every relevant input rather than one
 * example: the task rows of the retry-classification table, every handler outcome, every
 * pair of resolve modes. Each property is one the queue has broken before:
 *
 *  - a permanent failure is never retried (0e471cfb3 — a require failure retried 12 times);
 *  - every task stops within `maxAttempts` (0694490d0 — a failing task retried for ever);
 *  - a dedup'd retry never lowers a waiting task's mode (75868f3f6 — a re-try dropped);
 *  - no task parks longer than the backoff cap, whatever instant a handler names.
 */
class TaskRetryPropertiesSpec extends AnyFlatSpec with Matchers {
  import TaskWorker.PollResult

  private val t0 = Instant.parse("2026-09-24T12:00:00Z")
  private val taskRows = RetryClassificationTable.load.rowsFor("task")

  private final class CountingHandler(outcome: Task => HandlerOutcome) extends TaskHandler {
    val taskType: TaskType = TaskType.ResolveTmdb
    var runs = 0
    def handle(task: Task): HandlerOutcome = { runs += 1; outcome(task) }
  }

  private def throwing(failure: Throwable) = new CountingHandler(_ => throw failure)

  /** Run the pool against one task until it leaves the queue or `maxClaims` pass, the clock
   *  jumping past each backoff so every claim the queue would allow happens. */
  private def drain(handler: CountingHandler, maxAttempts: Int = TaskWorker.DefaultMaxAttempts,
                    maxClaims: Int = 100): (InMemoryTaskQueue, Int) = {
    val clock  = new MutableClock(t0)
    val queue  = new InMemoryTaskQueue
    queue.enqueue(TaskType.ResolveTmdb, "resolve-tmdb|x|2026", submittedAt = t0)
    val worker = new TaskWorker(queue, Seq(handler), maxAttempts = maxAttempts, clock = clock)
    var claims = 0
    var ticks  = 0
    while (ticks < maxClaims && queue.countByState().nonEmpty) {
      ticks += 1
      clock.advance(Duration.ofMillis(TaskWorker.MaxBackoff.toMillis + 1000))
      if (worker.claimAndRun("w0") != PollResult.Idle) claims += 1
    }
    (queue, claims)
  }

  "a task failure the table calls permanent" should "never be retried" in {
    val permanent = taskRows.filter(_.verdict == Verdict.Permanent)
    permanent should not be empty
    permanent.foreach { row =>
      val handler = throwing(RetryClassificationFailures.of(row))
      val (queue, _) = drain(handler)
      withClue(s"$row: ") {
        handler.runs shouldBe 1
        queue.countByState() shouldBe empty
      }
    }
  }

  "a task failure the table calls transient" should "be retried" in {
    val transient = taskRows.filter(_.verdict == Verdict.Transient)
    transient should not be empty
    transient.foreach { row =>
      val handler = throwing(RetryClassificationFailures.of(row))
      drain(handler, maxAttempts = 3)
      withClue(s"$row: ")(handler.runs shouldBe 3)
    }
  }

  "every task" should "stop within maxAttempts, however it keeps failing" in {
    val failing: Seq[(String, CountingHandler)] =
      taskRows.map(row => row.toString -> throwing(RetryClassificationFailures.of(row))) ++ Seq(
        "Reschedule" -> new CountingHandler(_ => HandlerOutcome.Reschedule(Some("still down"))),
        "Reschedule without an error" -> new CountingHandler(_ => HandlerOutcome.Reschedule()))
    for (maxAttempts <- Seq(1, 3, TaskWorker.DefaultMaxAttempts); (name, handler) <- failing) {
      handler.runs = 0
      val (queue, claims) = drain(handler, maxAttempts)
      withClue(s"$name with maxAttempts=$maxAttempts: ") {
        claims should be <= maxAttempts
        handler.runs should be <= maxAttempts
        queue.countByState() shouldBe empty
      }
    }
  }

  // A Deferred refunds its attempt, so it cannot exhaust; what bounds it is that each hold
  // is short — the clamp is what keeps a far-off instant from parking the task for good.
  "a Deferred task" should "stop within maxAttempts once its precondition clears and it fails" in {
    var calls = 0
    val handler = new CountingHandler(_ => { calls += 1; if (calls <= 3) HandlerOutcome.Deferred(Some("circuit open")) else throw new RuntimeException("down") })
    val (queue, _) = drain(handler, maxAttempts = 3)
    handler.runs shouldBe 3 + 3
    queue.countByState() shouldBe empty
  }

  "no task" should "park longer than the backoff cap, whatever instant its handler names" in {
    (0 to 200).foreach(attempts => TaskWorker.retryBackoffFor(attempts) should be <= TaskWorker.MaxBackoff)
    val farOff: Seq[(String, Instant => HandlerOutcome)] = Seq(
      "Reschedule"            -> (_ => HandlerOutcome.Reschedule(Some("later"))),
      "Deferred, no instant"  -> (_ => HandlerOutcome.Deferred(Some("circuit open"))),
      "Deferred, 6h away"     -> (now => HandlerOutcome.Deferred(Some("circuit open"), Some(now.plusSeconds(6 * 3600)))),
      "Deferred, a year away" -> (now => HandlerOutcome.Deferred(Some("circuit open"), Some(now.plusSeconds(365L * 86400)))))
    for ((name, outcome) <- farOff; priorAttempts <- Seq(0, 5, 10)) {
      val clock = new MutableClock(t0)
      val queue = new InMemoryTaskQueue
      queue.enqueue(TaskType.ResolveTmdb, "resolve-tmdb|x|2026", submittedAt = t0)
      (1 to priorAttempts).foreach { _ => val t = queue.claim("w9", 1.minute, clock.instant()).get; queue.release(t.id, "w9") }
      new TaskWorker(queue, Seq(new CountingHandler(_ => outcome(clock.instant()))), clock = clock).claimAndRun("w0")
      val parkedUntil = queue.monitor().active.flatMap(_.nextEligibleAt)
      withClue(s"$name after $priorAttempts attempt(s): ") {
        parkedUntil should not be empty
        parkedUntil.foreach(until => Duration.between(clock.instant(), until).toMillis should be <= TaskWorker.MaxBackoff.toMillis)
      }
    }
  }

  "a dedup'd resolve re-try" should "never lower the waiting task's mode" in {
    for (waiting <- ResolveMode.values; incoming <- ResolveMode.values) {
      val queue = new InMemoryTaskQueue
      val dispatcher = new QueueResolveDispatcher(queue)
      dispatcher.dispatch("La luz", Some(2026), None, None, waiting)
      dispatcher.dispatch("La luz", Some(2026), None, None, incoming)
      queue.monitor().active should have size 1
      val mode = queue.claim("w0", 1.minute, t0).map(task => EnrichTaskKeys.modeOf(task.payload))
      withClue(s"waiting $waiting, then $incoming: ") {
        mode shouldBe Some(EnrichTaskKeys.raisedMode(waiting, incoming))
        mode.get.ordinal should be >= math.max(waiting.ordinal, incoming.ordinal)
      }
    }
  }
}
