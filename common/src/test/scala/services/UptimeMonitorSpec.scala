package services

import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UptimeMonitorSpec extends AnyFlatSpec with Matchers {

  // A db handle whose MongoClient has been closed. Any write against it throws
  // `IllegalStateException: state should be: open` synchronously at .subscribe
  // — no network needed — exactly as happens to in-flight scrapers when Play
  // hot-reloads (or prod shuts down) and closes the old client out from under
  // them. The throw escapes the .subscribe(onError) guard, so it must be caught
  // by the upsert itself, not delivered to the subscriber.
  private def closedClientDb = {
    val client = MongoClient("mongodb://localhost:27017")
    val db     = client.getDatabase("uptime-test")
    client.close()
    db
  }

  "UptimeMonitor" should "record successes and failures for a service" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB")
    monitor.recordSuccess("TMDB")
    monitor.recordFailure("TMDB", "IOException: timeout")

    val history = monitor.history("TMDB")
    history should have size 1
    history.head.successes shouldBe 2
    history.head.failures shouldBe 1
  }

  it should "return empty history for unknown service" in {
    val monitor = new UptimeMonitor()
    monitor.history("nonexistent") shouldBe empty
  }

  "average latency" should "be the mean of timed successful calls (1h and total)" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB", 100L)
    monitor.recordSuccess("TMDB", 300L)
    monitor.averageMs1h("TMDB")    shouldBe Some(200L)
    monitor.averageMsTotal("TMDB") shouldBe Some(200L)
  }

  it should "be None when no call has been timed" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB")            // untimed
    monitor.averageMs1h("TMDB")    shouldBe None
    monitor.averageMsTotal("TMDB") shouldBe None
    monitor.averageMs1h("never-seen") shouldBe None
  }

  it should "not let untimed successes drag the average toward zero" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB", 200L)
    monitor.recordSuccess("TMDB")            // untimed — must not count as 0ms
    monitor.recordSuccess("TMDB", 400L)
    // mean of the two TIMED calls (200, 400), not (200, 0, 400)
    monitor.averageMsTotal("TMDB") shouldBe Some(300L)
    monitor.history("TMDB").head.successes shouldBe 3   // all three still count as successes
  }

  it should "track services independently" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB")
    monitor.recordFailure("IMDb", "ConnectException: refused")

    monitor.services shouldBe Set("TMDB", "IMDb")
    monitor.history("TMDB").head.successes shouldBe 1
    monitor.history("TMDB").head.failures shouldBe 0
    monitor.history("IMDb").head.successes shouldBe 0
    monitor.history("IMDb").head.failures shouldBe 1
  }

  it should "store error messages on failure" in {
    val monitor = new UptimeMonitor()
    monitor.recordFailure("TMDB", "IOException: Connection refused")
    monitor.recordFailure("TMDB", "HTTP 503 for GET https://api.themoviedb.org")

    val errors = monitor.history("TMDB").head.errors
    errors should have size 2
    errors.head shouldBe "IOException: Connection refused"
    errors(1) shouldBe "HTTP 503 for GET https://api.themoviedb.org"
  }

  it should "cap errors at MaxErrorsPerBucket" in {
    val monitor = new UptimeMonitor()
    (1 to 20).foreach(i => monitor.recordFailure("TMDB", s"error $i"))

    monitor.history("TMDB").head.errors should have size UptimeMonitor.MaxErrorsPerBucket
    monitor.history("TMDB").head.failures shouldBe 20
  }

  it should "not store errors on success" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB")

    monitor.history("TMDB").head.errors shouldBe empty
  }

  it should "not let a closed-client Mongo write break recording or flushing" in {
    val monitor = new UptimeMonitor(Some(closedClientDb))

    // record* is now pure in-memory; the batched flush is the Mongo write. A
    // closed client throws IllegalStateException synchronously at `.subscribe`
    // inside the flush — that must be caught, and the in-memory bucket must
    // update regardless.
    noException should be thrownBy {
      monitor.recordSuccess("Kino Rialto")
      monitor.recordFailure("Kino Rialto", "TimeoutException: boom")
      monitor.flushNow()
    }

    val history = monitor.history("Kino Rialto").head
    history.successes shouldBe 1
    history.failures shouldBe 1
  }

  // Batched persistence (the fix for the /uptime change-stream overload): record*
  // only touches memory + marks the bucket dirty; the flusher coalesces a
  // bucket's many records into ONE write of its CUMULATIVE counts, so the serving
  // app's change-stream load is bounded by the flush cadence, not the fetch rate.
  "batched flushing" should "coalesce a bucket's records into one cumulative write and clear dirty" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB", 100L)
    monitor.recordSuccess("TMDB", 300L)
    monitor.recordFailure("TMDB", "boom")

    val first = monitor.drainDirty()
    first should have size 1
    val w = first.head
    w.service shouldBe "TMDB"
    w.successes shouldBe 2
    w.failures shouldBe 1
    w.durationSumMs shouldBe 400L
    w.durationCount shouldBe 2
    w.errors shouldBe List("boom")

    // Nothing new recorded → the drain cleared dirty → nothing to flush.
    monitor.drainDirty() shouldBe empty

    // A further record re-dirties the bucket; the next flush carries the
    // CUMULATIVE total, not just the delta.
    monitor.recordSuccess("TMDB")
    val second = monitor.drainDirty()
    second should have size 1
    second.head.successes shouldBe 3
  }

  // Regression for the read/write split: the worker records all the scraper +
  // enrichment metrics and writes them to the shared `uptimeBuckets` collection.
  // The web process serving /uptime POLLS that collection and merges each
  // snapshot via applyExternalUpdate, firing its SSE listeners — otherwise the
  // page froze at the last web-boot snapshot.
  "an external (worker) bucket update" should "surface in history, averages, services and fire listeners" in {
    val monitor = new UptimeMonitor()
    var notified = List.empty[String]
    monitor.addListener((s, _) => notified = s :: notified)

    val ts = UptimeMonitor.bucketTimestamp(System.currentTimeMillis())
    monitor.applyExternalUpdate("TMDB", ts,
      successes = 7, failures = 2, durationSumMs = 1400L, durationCount = 7, errors = Seq("HTTP 503"))

    val h = monitor.history("TMDB")
    h should have size 1
    h.head.successes shouldBe 7
    h.head.failures shouldBe 2
    h.head.errors shouldBe Seq("HTTP 503")
    monitor.averageMsTotal("TMDB") shouldBe Some(200L)
    monitor.services should contain ("TMDB")
    notified should contain ("TMDB")
  }

  // The change stream delivers the full post-image, which carries CUMULATIVE
  // counts for the bucket. Applying it must REPLACE the bucket, not add — else
  // re-delivery (driver resume) or the web app's own echoed writes double-count.
  it should "replace (not add to) the bucket's counts when applied repeatedly" in {
    val monitor = new UptimeMonitor()
    val ts = UptimeMonitor.bucketTimestamp(System.currentTimeMillis())
    monitor.applyExternalUpdate("IMDb", ts, successes = 3, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    monitor.applyExternalUpdate("IMDb", ts, successes = 5, failures = 1, durationSumMs = 0L, durationCount = 0, errors = Seq("boom"))

    monitor.history("IMDb").head.successes shouldBe 5
    monitor.history("IMDb").head.failures shouldBe 1
    monitor.history("IMDb").head.errors shouldBe Seq("boom")
  }

  // The poller re-reads the recently-changeable buckets every interval, so the
  // same bucket value is applied over and over. Re-applying an unchanged value
  // must NOT fire listeners — otherwise every poll would spam the /uptime SSE
  // with ~one event per bucket regardless of activity.
  it should "fire listeners only when the applied snapshot actually changed" in {
    val monitor = new UptimeMonitor()
    var notifications = 0
    monitor.addListener((_, _) => notifications += 1)
    val ts = UptimeMonitor.bucketTimestamp(System.currentTimeMillis())

    monitor.applyExternalUpdate("RT", ts, successes = 4, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 1
    // Same snapshot again (next poll, no new worker activity) → no listener fire.
    monitor.applyExternalUpdate("RT", ts, successes = 4, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 1
    // A real change (worker recorded more) → fires.
    monitor.applyExternalUpdate("RT", ts, successes = 5, failures = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 2
  }

  // The poll must NOT re-read the whole 24h collection every interval — that
  // unbounded find() dominated the serving box's CPU once the scraper (hence
  // bucket) count grew. Writes only ever hit the current slot, so the poll is
  // bounded to recently-changeable buckets via a `{bucket: {$gte}}` filter.
  "the poll query" should "fetch only buckets within the lookback window, not the whole collection" in {
    val monitor = new UptimeMonitor()
    val now = 1700000000000L
    val filterDoc = monitor.pollFilter(now).toBsonDocument()

    // A regression to find()/an empty filter would scan the whole collection.
    filterDoc.isEmpty shouldBe false
    filterDoc.getDocument("bucket").getDateTime("$gte").getValue shouldBe (now - UptimeMonitor.PollLookbackMs)
  }

  "BucketSnapshot.status" should "be green when all succeed" in {
    UptimeMonitor.BucketSnapshot(0, successes = 5, failures = 0, Seq.empty).status shouldBe "green"
  }

  it should "be red when all fail" in {
    UptimeMonitor.BucketSnapshot(0, successes = 0, failures = 3, Seq("err")).status shouldBe "red"
  }

  it should "be yellow when mixed" in {
    UptimeMonitor.BucketSnapshot(0, successes = 2, failures = 1, Seq("err")).status shouldBe "yellow"
  }

  it should "be empty when no data" in {
    UptimeMonitor.BucketSnapshot(0, successes = 0, failures = 0, Seq.empty).status shouldBe "empty"
  }
}
