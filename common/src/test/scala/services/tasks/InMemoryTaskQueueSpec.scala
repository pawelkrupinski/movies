package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class InMemoryTaskQueueSpec extends AnyFlatSpec with Matchers {
  import TaskType._

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  "enqueue" should "add a new task and dedup a second with the same key while active" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|kino-x", submittedAt = t0) shouldBe EnqueueResult.Added
    q.enqueue(ScrapeCinema, "scrape|kino-x", submittedAt = t0) shouldBe EnqueueResult.Duplicate
    q.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "allow re-enqueue once the prior task is completed (tombstoned)" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|kino-x", submittedAt = t0)
    val task = q.claim("w1", 1.minute, t0).get
    q.complete(task.id, "w1")
    q.enqueue(ScrapeCinema, "scrape|kino-x", submittedAt = t0.plusSeconds(60)) shouldBe EnqueueResult.Added
  }

  "claim" should "hand out the oldest waiting task first (FIFO) and carry the payload" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ImdbRating, "imdb|b", Map("k" -> "vb"), submittedAt = t0.plusSeconds(10))
    q.enqueue(ImdbRating, "imdb|a", Map("k" -> "va"), submittedAt = t0)
    val first = q.claim("w1", 1.minute, t0).get
    first.dedupKey shouldBe "imdb|a"
    first.payload shouldBe Map("k" -> "va")
    q.claim("w1", 1.minute, t0).get.dedupKey shouldBe "imdb|b"
    q.claim("w1", 1.minute, t0) shouldBe None // nothing left waiting
  }

  it should "not hand the same task to a second claim" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    q.claim("w1", 1.minute, t0) should be(defined)
    q.claim("w2", 1.minute, t0) shouldBe None
  }

  "complete" should "tombstone only when the caller still holds the lease" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    val task = q.claim("w1", 1.minute, t0).get
    q.complete(task.id, "w2") // wrong owner — no-op
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 1L
    q.complete(task.id, "w1") // real owner
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
  }

  "release" should "return a task to waiting so it can be claimed again" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    val task = q.claim("w1", 1.minute, t0).get
    q.release(task.id, "w1", Some("transient"))
    val again = q.claim("w1", 1.minute, t0).get
    again.dedupKey shouldBe "scrape|x"
    again.attempts shouldBe 2 // claimed twice
  }

  "reapExpiredLeases" should "return expired worked-on tasks to waiting and guard against a late complete" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    val first = q.claim("w1", 1.minute, t0).get
    q.reapExpiredLeases(t0.plusSeconds(120)) shouldBe 1 // lease (t0+60s) expired by t0+120s
    val reclaimed = q.claim("w2", 1.minute, t0.plusSeconds(120)).get
    reclaimed.id shouldBe first.id
    q.complete(first.id, "w1") // original worker's late call — must not delete w2's task
    q.countByState().getOrElse(TaskState.WorkedOn, 0L) shouldBe 1L
    q.complete(reclaimed.id, "w2")
    q.countByState().getOrElse(TaskState.Deleted, 0L) shouldBe 1L
  }

  it should "not reap a lease that has not yet expired" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    q.claim("w1", 5.minutes, t0)
    q.reapExpiredLeases(t0.plusSeconds(60)) shouldBe 0
  }
}
