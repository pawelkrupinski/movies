package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class CachingTaskQueueSpec extends AnyFlatSpec with Matchers {
  import TaskType._

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  /** Counts delegate enqueue calls so a cache-served repeat is observable. */
  private class CountingQueue extends InMemoryTaskQueue {
    var enqueueCalls = 0
    override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant, notBefore: Option[Instant]): EnqueueResult = {
      enqueueCalls += 1
      super.enqueue(taskType, dedupKey, payload, submittedAt, notBefore)
    }
  }

  "CachingTaskQueue" should "serve a repeat enqueue of an active key from cache without hitting the delegate" in {
    val delegate = new CountingQueue
    val q = new CachingTaskQueue(delegate)
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0) shouldBe EnqueueResult.Added
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0) shouldBe EnqueueResult.Duplicate
    delegate.enqueueCalls shouldBe 1 // the second was served from the local cache
  }

  it should "keep suppressing while a claimed-but-not-completed task is in flight" in {
    val delegate = new CountingQueue
    val q = new CachingTaskQueue(delegate)
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    q.claim("w1", 1.minute, t0) // worked_on — still active
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0) shouldBe EnqueueResult.Duplicate
    delegate.enqueueCalls shouldBe 1 // still cached — no round-trip while in flight
  }

  it should "re-hit the delegate after the task completes (cache evicted)" in {
    val delegate = new CountingQueue
    val q = new CachingTaskQueue(delegate)
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    val task = q.claim("w1", 1.minute, t0).get
    q.complete(task.id, "w1")
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0) shouldBe EnqueueResult.Added
    delegate.enqueueCalls shouldBe 2 // completion evicted the key → a real re-enqueue
  }

  it should "pass claim / waitingCount / monitor / complete through to the delegate" in {
    val delegate = new CountingQueue
    val q = new CachingTaskQueue(delegate)
    q.enqueue(ImdbRating, "imdb|y", submittedAt = t0)
    q.waitingCount(ImdbRating) shouldBe 1
    val task = q.claim("w1", 1.minute, t0).get
    task.dedupKey shouldBe "imdb|y"
    q.waitingCount(ImdbRating) shouldBe 0
    q.monitor().active.map(_.dedupKey) shouldBe Seq("imdb|y")
    q.complete(task.id, "w1")
    q.countByState() shouldBe empty
  }
}
