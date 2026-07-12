package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Eventually.eventually

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

class BulkRefreshHandlerSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  private def task = Task("id", TaskType.RefreshAllImdb,
    EnrichTaskKeys.bulkDedup(TaskType.RefreshAllImdb), Map.empty, attempts = 1)

  private def handler(store: BulkTaskResultStore, run: () => BulkRefreshResult) =
    new BulkRefreshHandler(TaskType.RefreshAllImdb, "test", run, store)

  "BulkRefreshHandler" should "dispatch the run and return Done immediately" in {
    val runs = new AtomicInteger(0)
    val h    = handler(new InMemoryBulkTaskResultStore, () => { runs.incrementAndGet(); BulkRefreshResult.message("ok") })

    h.handle(task) shouldBe Done
    eventually(runs.get shouldBe 1)
  }

  // The core of this change: the run's outcome outlives the (instantly deleted)
  // task doc by being written to the shared result store, keyed by taskType.
  it should "record the run's summary to the store on completion" in {
    val store = new InMemoryBulkTaskResultStore
    val h = handler(store, () =>
      BulkRefreshResult.counts(walked = 578, changed = 4, discovered = 0, failed = 0,
        message = "tick done — 4 changed, 0 failed, 574 unchanged."))

    h.handle(task) shouldBe Done

    eventually {
      val result = store.latest().get(TaskType.RefreshAllImdb).getOrElse(fail("no result recorded"))
      result.succeeded shouldBe true
      result.walked    shouldBe Some(578)
      result.changed   shouldBe Some(4)
      result.failed    shouldBe Some(0)
      result.message   should include ("4 changed")
    }
  }

  it should "record a failed result (not nothing) when the run throws" in {
    val store = new InMemoryBulkTaskResultStore
    val h = handler(store, () => throw new RuntimeException("boom"))

    h.handle(task) shouldBe Done

    eventually {
      val result = store.latest().get(TaskType.RefreshAllImdb).getOrElse(fail("no result recorded"))
      result.succeeded shouldBe false
      result.message   should include ("boom")
      result.changed   shouldBe None
    }
  }

  it should "skip an overlapping trigger while a run is still in progress, then allow a fresh run after it finishes" in {
    val started = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val runs    = new AtomicInteger(0)
    val store   = new InMemoryBulkTaskResultStore
    val h = handler(store, () => {
      runs.incrementAndGet()
      started.countDown()
      release.await() // hold the run open so the second trigger overlaps it
      BulkRefreshResult.message("done")
    })

    h.handle(task) shouldBe Done
    started.await() // first run is now in flight and parked

    h.handle(task) shouldBe Skipped // overlapping trigger collapses
    runs.get shouldBe 1
    // A Skipped trigger records nothing — it can't clobber the in-flight run's result.
    store.latest().get(TaskType.RefreshAllImdb) shouldBe None

    release.countDown()             // let the first run finish
    eventually(h.handle(task) shouldBe Done) // running flag cleared → a new run is allowed
    eventually(runs.get shouldBe 2)
  }

  it should "clear the running flag and recover even when the run throws" in {
    val attempts = new AtomicInteger(0)
    val h = handler(new InMemoryBulkTaskResultStore, () => {
      attempts.incrementAndGet(); throw new RuntimeException("boom")
    })

    h.handle(task) shouldBe Done
    eventually(attempts.get shouldBe 1)
    // A later trigger isn't blocked by the previous failure.
    eventually(h.handle(task) shouldBe Done)
    eventually(attempts.get shouldBe 2)
  }
}
