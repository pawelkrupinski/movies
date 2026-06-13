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

  it should "hold a task back from claim until its notBefore backoff elapses" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    val task = q.claim("w1", 1.minute, t0).get
    q.release(task.id, "w1", Some("transient"), notBefore = Some(t0.plusSeconds(30)))
    q.claim("w1", 1.minute, t0.plusSeconds(10)) shouldBe None                                   // still backed off
    q.claim("w1", 1.minute, t0.plusSeconds(30)).map(_.dedupKey) shouldBe Some("scrape|x")       // window elapsed
  }

  it should "not let a backed-off task block a newer, eligible one" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|old", submittedAt = t0)                                      // oldest
    val old = q.claim("w1", 1.minute, t0).get
    q.release(old.id, "w1", Some("transient"), notBefore = Some(t0.plusSeconds(60)))            // held back
    q.enqueue(ScrapeCinema, "scrape|new", submittedAt = t0.plusSeconds(5))
    q.claim("w1", 1.minute, t0.plusSeconds(10)).map(_.dedupKey) shouldBe Some("scrape|new")     // skips the backed-off old one
  }

  it should "clear a stale backoff window when re-released without one (immediately claimable)" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|x", submittedAt = t0)
    val t1 = q.claim("w1", 1.minute, t0).get
    q.release(t1.id, "w1", Some("transient"), notBefore = Some(t0.plusSeconds(300)))
    val t2 = q.claim("w1", 1.minute, t0.plusSeconds(300)).get
    q.release(t2.id, "w1", None, notBefore = None)                                               // no-handler-style release
    q.claim("w1", 1.minute, t0.plusSeconds(301)).map(_.dedupKey) shouldBe Some("scrape|x")
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

  "monitor" should "list active tasks oldest-first with their live state, and exclude tombstones" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|a", submittedAt = t0)
    q.enqueue(ImdbRating, "imdb|b", submittedAt = t0.plusSeconds(10))
    q.enqueue(RtRating, "rt|c", submittedAt = t0.plusSeconds(20))
    // Claim the oldest so it shows as worked_on with a worker + lease; complete one so it tombstones.
    val claimed = q.claim("w1", 1.minute, t0).get // scrape|a → worked_on
    q.enqueue(McRating, "mc|d", submittedAt = t0.plusSeconds(30))
    val toFinish = q.claim("w1", 1.minute, t0).get // imdb|b → worked_on
    q.complete(toFinish.id, "w1")                   // imdb|b → deleted (tombstone)

    val snap = q.monitor()
    snap.active.map(_.dedupKey) shouldBe Seq("scrape|a", "rt|c", "mc|d") // oldest-first, no tombstone
    snap.counts.getOrElse(TaskState.Deleted, 0L) shouldBe 1L            // tombstone counted, not listed

    val head = snap.active.head
    head.dedupKey shouldBe "scrape|a"
    head.state shouldBe TaskState.WorkedOn
    head.workerId shouldBe Some("w1")
    head.leaseExpiresAt shouldBe Some(t0.plusSeconds(60))
    head.id shouldBe claimed.id

    val waiting = snap.active(1)
    waiting.state shouldBe TaskState.Waiting
    waiting.workerId shouldBe None
    waiting.leaseExpiresAt shouldBe None
  }

  it should "cap the listed active tasks at activeLimit while still counting all" in {
    val q = new InMemoryTaskQueue
    (1 to 5).foreach(i => q.enqueue(ScrapeCinema, s"scrape|$i", submittedAt = t0.plusSeconds(i.toLong)))
    val snap = q.monitor(activeLimit = 3)
    snap.active.map(_.dedupKey) shouldBe Seq("scrape|1", "scrape|2", "scrape|3")
    snap.counts.getOrElse(TaskState.Waiting, 0L) shouldBe 5L
  }
}
