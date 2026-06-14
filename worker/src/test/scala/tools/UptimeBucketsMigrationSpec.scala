package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import tools.UptimeBucketsMigration.RawBucket

class UptimeBucketsMigrationSpec extends AnyFlatSpec with Matchers {

  // A 15-min boundary to hang sub-buckets off; the old writer floored to 5-min,
  // so a single 15-min slot held up to three documents (at :00, :05, :10).
  private val slot = UptimeMonitor.bucketTimestamp(1700000000000L)
  private val fiveMin = 5 * 60 * 1000L

  "merge15Min" should "fold a slot's three 5-min sub-buckets into one, summing counts" in {
    val rows = Seq(
      RawBucket("TMDB", slot,               successes = 10, failures = 1, durationSumMs = 1000L, durationCount = 5, errors = Seq("a")),
      RawBucket("TMDB", slot + fiveMin,     successes = 20, failures = 0, durationSumMs = 2000L, durationCount = 8, errors = Seq("b")),
      RawBucket("TMDB", slot + 2 * fiveMin, successes = 30, failures = 2, durationSumMs = 3000L, durationCount = 7, errors = Seq("c"))
    )

    val merged = UptimeBucketsMigration.merge15Min(rows)
    merged should have size 1
    val m = merged.head
    m.service shouldBe "TMDB"
    m.bucketTs shouldBe slot
    m.successes shouldBe 60
    m.failures shouldBe 3
    m.durationSumMs shouldBe 6000L
    m.durationCount shouldBe 20
    m.errors shouldBe Seq("a", "b", "c") // concatenated in time order
  }

  it should "keep different 15-min slots and different services separate" in {
    val nextSlot = slot + UptimeMonitor.BucketDurationMs
    val rows = Seq(
      RawBucket("TMDB", slot,           successes = 5, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Nil),
      RawBucket("TMDB", slot + fiveMin, successes = 7, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Nil),
      RawBucket("TMDB", nextSlot,       successes = 9, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Nil),
      RawBucket("IMDb", slot,           successes = 3, failures = 1, durationSumMs = 0L, durationCount = 0, errors = Nil)
    )

    val merged = UptimeBucketsMigration.merge15Min(rows)
    merged should have size 3
    merged.find(m => m.service == "TMDB" && m.bucketTs == slot).get.successes shouldBe 12
    merged.find(m => m.service == "TMDB" && m.bucketTs == nextSlot).get.successes shouldBe 9
    merged.find(m => m.service == "IMDb" && m.bucketTs == slot).get.failures shouldBe 1
  }

  it should "cap concatenated errors at MaxErrorsPerBucket" in {
    val rows = (0 until 3).map { i =>
      RawBucket("TMDB", slot + i * fiveMin,
        successes = 0, failures = 5, durationSumMs = 0L, durationCount = 0,
        errors = (1 to 5).map(n => s"err-$i-$n"))
    }

    val merged = UptimeBucketsMigration.merge15Min(rows)
    merged.head.errors should have size UptimeMonitor.MaxErrorsPerBucket
    // First-by-time errors survive the cap.
    merged.head.errors.head shouldBe "err-0-1"
  }

  it should "be idempotent — a slot already collapsed to one boundary document merges to itself" in {
    val once = UptimeBucketsMigration.merge15Min(Seq(
      RawBucket("TMDB", slot,           successes = 5, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Nil),
      RawBucket("TMDB", slot + fiveMin, successes = 7, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Nil)
    ))
    val twice = UptimeBucketsMigration.merge15Min(once.map(m =>
      RawBucket(m.service, m.bucketTs, m.successes, m.failures, m.durationSumMs, m.durationCount, m.errors)))

    twice shouldBe once
    twice.head.successes shouldBe 12
  }
}
