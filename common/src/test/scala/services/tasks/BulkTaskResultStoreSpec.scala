package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * Locks the [[BulkTaskResultStore]] contract against its in-memory
 * implementation: one entry per taskType, last write wins, counts round-trip.
 * The Mongo implementation shares this contract (its round-trip is covered by
 * the integration layer); the business logic — what a result IS — lives above
 * the store in [[BulkRefreshHandler]], so the fake is a plain last-write-wins map.
 */
class BulkTaskResultStoreSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-07-12T10:00:00Z")

  "InMemoryBulkTaskResultStore" should "start empty" in {
    new InMemoryBulkTaskResultStore().latest() shouldBe empty
  }

  it should "return the recorded result keyed by taskType, counts intact" in {
    val store = new InMemoryBulkTaskResultStore
    store.record(BulkTaskResult(TaskType.RefreshAllImdb, t0, succeeded = true,
      "tick done — 4 changed, 0 failed.", walked = Some(578), changed = Some(4), discovered = Some(0), failed = Some(0)))

    val result = store.latest()(TaskType.RefreshAllImdb)
    result.ranAt     shouldBe t0
    result.succeeded shouldBe true
    result.walked    shouldBe Some(578)
    result.changed   shouldBe Some(4)
    result.message   should include ("4 changed")
  }

  it should "overwrite the previous result for the same taskType (last run wins)" in {
    val store = new InMemoryBulkTaskResultStore
    store.record(BulkTaskResult(TaskType.RefreshAllRt, t0, succeeded = true, "first", changed = Some(1)))
    store.record(BulkTaskResult(TaskType.RefreshAllRt, t0.plusSeconds(3600), succeeded = false, "failed: boom"))

    store.latest() should have size 1
    val result = store.latest()(TaskType.RefreshAllRt)
    result.succeeded shouldBe false
    result.message   shouldBe "failed: boom"
    result.ranAt     shouldBe t0.plusSeconds(3600)
  }

  it should "keep results for different taskTypes independent" in {
    val store = new InMemoryBulkTaskResultStore
    store.record(BulkTaskResult(TaskType.RefreshAllImdb, t0, succeeded = true, "imdb"))
    store.record(BulkTaskResult(TaskType.SettleNow,      t0, succeeded = true, "settled"))

    store.latest().keySet shouldBe Set(TaskType.RefreshAllImdb, TaskType.SettleNow)
  }

  "BulkTaskResult.from" should "fold a walk's BulkRefreshResult into the persisted record" in {
    val walk = BulkRefreshResult.counts(walked = 10, changed = 2, discovered = 1, failed = 0, message = "done")
    val result = BulkTaskResult.from(TaskType.RefreshAllFilmweb, t0, succeeded = true, walk)

    result.taskType   shouldBe TaskType.RefreshAllFilmweb
    result.succeeded  shouldBe true
    result.walked     shouldBe Some(10)
    result.changed    shouldBe Some(2)
    result.discovered shouldBe Some(1)
    result.message    shouldBe "done"
  }
}
