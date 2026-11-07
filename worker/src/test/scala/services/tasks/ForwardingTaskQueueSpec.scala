package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class ForwardingTaskQueueSpec extends AnyFlatSpec with Matchers {
  import TaskType._

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  /** A decorator that decorates nothing: every call must reach the queue under it. */
  private def undecorated(inner: TaskQueue): TaskQueue = new ForwardingTaskQueue(inner) {}

  "ForwardingTaskQueue" should "hand every call to the queue it wraps" in {
    val inner = new InMemoryTaskQueue
    val q     = undecorated(inner)
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0, notBefore = Some(t0.plusSeconds(600))) shouldBe EnqueueResult.Added
    q.waitingCount(ScrapeCinema) shouldBe 1
    q.amendWaiting("scrape|x", Map("k" -> "v")) shouldBe true
    q.claim("w1", 1.minute, t0) shouldBe None // held until notBefore
    // A decorator once inherited TaskQueue's `raise = false` default instead of forwarding it.
    q.raise("scrape|x", t0) shouldBe true
    val task = q.claim("w1", 1.minute, t0).getOrElse(fail("raised task not claimable"))
    task.dedupKey shouldBe "scrape|x"
    q.complete(task.id, "w1")
    inner.waitingCount(ScrapeCinema) shouldBe 0
    q.countByState() shouldBe inner.countByState()
    q.monitor(10) shouldBe inner.monitor(10)
    q.reapExpiredLeases(t0) shouldBe 0
    q.watchWaiting(_ => ()) shouldBe defined
  }

  "the queue decorators" should "forward what they do not decorate, raise included" in {
    val series = new services.metrics.WorkerTaskMetrics.Series(poolSize = 4, countryCodes = Seq("pl"))
    Seq[TaskQueue => TaskQueue](
      new CachingTaskQueue(_),
      new services.metrics.MeteredTaskQueue(_, new services.metrics.WorkerTaskMetrics("pl", series)),
      // The fixpoint ledger's queue answered every raise `false` (TaskQueue's default), unlike production's.
      new tools.ReaskCountingTaskQueue(_, new services.freshness.InMemoryFreshnessStore, _ => None, () => t0)
    ).foreach { decorate =>
      val q = decorate(new InMemoryTaskQueue)
      q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0, notBefore = Some(t0.plusSeconds(600)))
      q.raise("scrape|x", t0) shouldBe true
    }
  }
}
