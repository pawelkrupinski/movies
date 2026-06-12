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

  "BulkRefreshHandler" should "dispatch the run and return Done immediately" in {
    val runs = new AtomicInteger(0)
    val h    = new BulkRefreshHandler(TaskType.RefreshAllImdb, "test", () => { runs.incrementAndGet(); () })

    h.handle(task) shouldBe Done
    eventually(runs.get shouldBe 1)
  }

  it should "skip an overlapping trigger while a run is still in progress, then allow a fresh run after it finishes" in {
    val started = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val runs    = new AtomicInteger(0)
    val h = new BulkRefreshHandler(TaskType.RefreshAllImdb, "test", () => {
      runs.incrementAndGet()
      started.countDown()
      release.await() // hold the run open so the second trigger overlaps it
    })

    h.handle(task) shouldBe Done
    started.await() // first run is now in flight and parked

    h.handle(task) shouldBe Skipped // overlapping trigger collapses
    runs.get shouldBe 1

    release.countDown()             // let the first run finish
    eventually(h.handle(task) shouldBe Done) // running flag cleared → a new run is allowed
    eventually(runs.get shouldBe 2)
  }

  it should "clear the running flag and recover even when the run throws" in {
    val attempts = new AtomicInteger(0)
    val h = new BulkRefreshHandler(TaskType.RefreshAllImdb, "test", () => {
      attempts.incrementAndGet(); throw new RuntimeException("boom")
    })

    h.handle(task) shouldBe Done
    eventually(attempts.get shouldBe 1)
    // A later trigger isn't blocked by the previous failure.
    eventually(h.handle(task) shouldBe Done)
    eventually(attempts.get shouldBe 2)
  }
}
