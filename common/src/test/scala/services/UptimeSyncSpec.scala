package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `UptimeSync` on its own: a bucket store handed in by reference and a notify
 *  callback, no monitor and no Mongo. The monitor's own spec covers what a
 *  merged bucket looks like through the query API; this pins the seam itself —
 *  that the merge lands in the SHARED store and that only a change notifies. */
class UptimeSyncSpec extends AnyFlatSpec with Matchers {

  "UptimeSync.applyExternalUpdate" should "write into the shared bucket store and notify only when the bucket changed" in {
    val store = new UptimeMonitor.BucketStore()
    var notified = List.empty[String]
    val sync = new UptimeSync(store, (service, _) => notified = service :: notified)

    val timestamp = UptimeMonitor.bucketTimestamp(System.currentTimeMillis())
    sync.applyExternalUpdate("TMDB", timestamp, successes = 7, failures = 2, zeroes = 1, durationSumMs = 1400L, durationCount = 7, errors = Seq("HTTP 503"))

    val bucket = store.get("TMDB").get(timestamp)
    bucket.successes.get() shouldBe 7
    bucket.failures.get() shouldBe 2
    bucket.zeroes.get() shouldBe 1
    bucket.durationSumMs.get() shouldBe 1400L
    notified shouldBe List("TMDB")

    // Every poll re-reads the same snapshot; an unchanged one must stay silent.
    sync.applyExternalUpdate("TMDB", timestamp, successes = 7, failures = 2, zeroes = 1, durationSumMs = 1400L, durationCount = 7, errors = Seq("HTTP 503"))
    notified shouldBe List("TMDB")
  }
}
