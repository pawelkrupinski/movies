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

  "urlFromTags" should "extract the venue source page from the url: tag, ignoring other tags" in {
    UptimeMonitor.urlFromTags(Set("shared:FilmwebShowtimesClient", "url:https://www.filmweb.pl/cinema/-1714")) shouldBe
      Some("https://www.filmweb.pl/cinema/-1714")
    UptimeMonitor.urlFromTags(Set("custom:RialtoClient")) shouldBe None
    UptimeMonitor.urlFromTags(Set.empty) shouldBe None
  }

  "cinemaUrls" should "map every service carrying a url: tag to its URL, skipping the rest" in {
    val snapshot = Map(
      "DKF Rumcajs"              -> Set("shared:FilmwebShowtimesClient", "url:https://www.filmweb.pl/cinema/-1714"),
      "Multikino Stary Browar"  -> Set("shared:MultikinoClient"),
      "Kino Rialto"             -> Set("custom:RialtoClient", "url:https://www.kinorialto.poznan.pl"),
    )
    UptimeMonitor.cinemaUrls(snapshot) shouldBe Map(
      "DKF Rumcajs" -> "https://www.filmweb.pl/cinema/-1714",
      "Kino Rialto" -> "https://www.kinorialto.poznan.pl",
    )
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
      successes = 7, failures = 2, zeroes = 1, durationSumMs = 1400L, durationCount = 7, errors = Seq("HTTP 503"))

    val h = monitor.history("TMDB")
    h should have size 1
    h.head.successes shouldBe 7
    h.head.failures shouldBe 2
    h.head.zeroes shouldBe 1
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
    monitor.applyExternalUpdate("IMDb", ts, successes = 3, failures = 0, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    monitor.applyExternalUpdate("IMDb", ts, successes = 5, failures = 1, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq("boom"))

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

    monitor.applyExternalUpdate("RT", ts, successes = 4, failures = 0, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 1
    // Same snapshot again (next poll, no new worker activity) → no listener fire.
    monitor.applyExternalUpdate("RT", ts, successes = 4, failures = 0, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 1
    // A real change (worker recorded more) → fires.
    monitor.applyExternalUpdate("RT", ts, successes = 5, failures = 0, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 2
    // A change confined to the zeroes dimension also counts as a change → fires.
    monitor.applyExternalUpdate("RT", ts, successes = 5, failures = 0, zeroes = 1, durationSumMs = 0L, durationCount = 0, errors = Seq.empty)
    notifications shouldBe 3
  }

  // The poll must NOT re-read the whole 24h collection every interval — that
  // unbounded find() dominated the serving box's CPU once the scraper (hence
  // bucket) count grew. Writes only ever hit the current slot, so the poll is
  // bounded to recently-changeable buckets via a `{bucket: {$gte}}` filter.
  "the poll query" should "fetch only buckets within the lookback window, not the whole collection" in {
    val monitor = new UptimeMonitor()
    val now = 1700000000000L
    val filterDocument = monitor.pollFilter(now).toBsonDocument()

    // A regression to find()/an empty filter would scan the whole collection.
    filterDocument.isEmpty shouldBe false
    filterDocument.getDocument("bucket").getDateTime("$gte").getValue shouldBe (now - UptimeMonitor.PollLookbackMs)
  }

  // Boot hydrate must load only the displayed window, not the whole collection
  // via an unbounded find(). An unbounded scan with a tight timeout is what
  // stranded the serving process with no history when Mongo was slow at boot.
  "the hydrate query" should "bound to the displayed window, not scan the whole collection" in {
    val monitor = new UptimeMonitor()
    val now = 1700000000000L
    val filterDocument = monitor.hydrateFilter(now).toBsonDocument()

    filterDocument.isEmpty shouldBe false
    filterDocument.getDocument("bucket").getDateTime("$gte").getValue shouldBe
      (UptimeMonitor.bucketTimestamp(now) - UptimeMonitor.MaxBuckets.toLong * UptimeMonitor.BucketDurationMs)
  }

  // Bucket granularity is 15 minutes: two records up to 14 min apart share a
  // bucket, and the retained window stays 24h (MaxBuckets * BucketDurationMs).
  "bucket granularity" should "span 15 minutes and keep a 24h retained window" in {
    val b0 = UptimeMonitor.bucketTimestamp(1700000000000L)
    UptimeMonitor.bucketTimestamp(b0 + 14 * 60 * 1000L) shouldBe b0
    UptimeMonitor.bucketTimestamp(b0 + 15 * 60 * 1000L) shouldBe (b0 + 15 * 60 * 1000L)
    UptimeMonitor.MaxBuckets.toLong * UptimeMonitor.BucketDurationMs shouldBe 24L * 60 * 60 * 1000L
  }

  // The persisted-bucket TTL must outlive the 24h display window by exactly one
  // bucket (15 min) so the OLDEST slot the /uptime page renders isn't expired out
  // from under it mid-window. Guards against the TTL drifting back to a flat 24h
  // or MaxBuckets changing without the TTL following.
  "the bucket TTL" should "outlive the 24h display window by one bucket (24h15m)" in {
    UptimeMonitor.BucketTtlSeconds shouldBe (UptimeMonitor.MaxBuckets + 1).toLong * (UptimeMonitor.BucketDurationMs / 1000L)
    UptimeMonitor.BucketTtlSeconds shouldBe (24L * 60 * 60) + (15 * 60) // 87300s
  }

  // Per-service tags are a generic per-row label channel (the /uptime client
  // markers ride it). Without Mongo, tagService stores in memory and the snapshot
  // reflects it; an empty set clears the row.
  "service tags" should "round-trip per service in memory and be cleared by an empty set" in {
    val monitor = new UptimeMonitor()
    monitor.tagService("Kino Rialto", Set("custom:RialtoClient"))
    monitor.tagService("Helios Posnania", Set("shared:HeliosClient"))

    monitor.serviceTagsSnapshot()("Kino Rialto")    shouldBe Set("custom:RialtoClient")
    monitor.serviceTagsSnapshot()("Helios Posnania") shouldBe Set("shared:HeliosClient")

    monitor.tagService("Kino Rialto", Set.empty)
    monitor.serviceTagsSnapshot()("Kino Rialto") shouldBe Set.empty
  }

  "BucketSnapshot.status" should "be green when all succeed" in {
    UptimeMonitor.BucketSnapshot(0, successes = 5, failures = 0, zeroes = 0, Seq.empty).status shouldBe "green"
  }

  it should "be red when all fail" in {
    UptimeMonitor.BucketSnapshot(0, successes = 0, failures = 3, zeroes = 0, Seq("err")).status shouldBe "red"
  }

  it should "be yellow when mixed" in {
    UptimeMonitor.BucketSnapshot(0, successes = 2, failures = 1, zeroes = 0, Seq("err")).status shouldBe "yellow"
  }

  it should "be empty when no data" in {
    UptimeMonitor.BucketSnapshot(0, successes = 0, failures = 0, zeroes = 0, Seq.empty).status shouldBe "empty"
  }

  it should "be zero when only empty-handed calls, no failures and no successes" in {
    UptimeMonitor.BucketSnapshot(0, successes = 0, failures = 0, zeroes = 2, Seq.empty).status shouldBe "zero"
  }

  it should "stay green when a real success and an empty call share a slot" in {
    UptimeMonitor.BucketSnapshot(0, successes = 1, failures = 0, zeroes = 1, Seq.empty).status shouldBe "green"
  }

  it should "be yellow when a failure and an empty call share a slot" in {
    UptimeMonitor.BucketSnapshot(0, successes = 0, failures = 1, zeroes = 1, Seq("err")).status shouldBe "yellow"
  }

  "recordEmpty" should "increment the zeroes dimension and surface a zero-status bucket" in {
    val monitor = new UptimeMonitor()
    monitor.recordEmpty("svc", 42L)
    val bucket = monitor.history("svc").head
    bucket.zeroes    shouldBe 1
    bucket.successes shouldBe 0
    bucket.failures  shouldBe 0
    bucket.status    shouldBe "zero"
    // The empty round-trip still timed, so its latency feeds the average.
    monitor.averageMsTotal("svc") shouldBe Some(42L)
  }

  // A scrape the Filmweb fallback served counts as a success (the user got
  // showtimes — green bar) but also marks the bucket `fallback`, so the /uptime
  // page can flag the bar "served via Filmweb" instead of a plain green.
  "recordFallbackSuccess" should "count as a success, mark the bucket fallback, and stay green" in {
    val monitor = new UptimeMonitor()
    monitor.recordFallbackSuccess("Kino Praha", 55L)
    val bucket = monitor.history("Kino Praha").head
    bucket.successes shouldBe 1
    bucket.failures  shouldBe 0
    bucket.fallback  shouldBe true
    bucket.status    shouldBe "green"
    monitor.averageMsTotal("Kino Praha") shouldBe Some(55L)
  }

  it should "carry the fallback flag into the flushed BucketWrite" in {
    val monitor = new UptimeMonitor()
    monitor.recordFallbackSuccess("Kino Praha", 30L)
    val w = monitor.drainDirty().head
    w.successes shouldBe 1
    w.fallback  shouldBe true
  }

  it should "leave fallback false for an ordinary (non-fallback) success" in {
    val monitor = new UptimeMonitor()
    monitor.recordSuccess("TMDB", 10L)
    monitor.history("TMDB").head.fallback shouldBe false
    monitor.drainDirty().head.fallback    shouldBe false
  }

  // The web process learns a cinema is on Filmweb fallback only via the worker's
  // bucket write it polls — so the flag must survive applyExternalUpdate, and a
  // flip in the flag must be treated as a change so the /uptime SSE updates.
  "an external bucket update" should "propagate the fallback flag and fire listeners when it flips" in {
    val monitor = new UptimeMonitor()
    var notifications = 0
    monitor.addListener((_, _) => notifications += 1)
    val ts = UptimeMonitor.bucketTimestamp(System.currentTimeMillis())

    monitor.applyExternalUpdate("Kino Praha", ts,
      successes = 2, failures = 0, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty, fallback = true)
    monitor.history("Kino Praha").head.fallback shouldBe true
    notifications shouldBe 1

    // Same counts, fallback flipped off (primary recovered) — the flag change
    // alone must register as a change and fire.
    monitor.applyExternalUpdate("Kino Praha", ts,
      successes = 2, failures = 0, zeroes = 0, durationSumMs = 0L, durationCount = 0, errors = Seq.empty, fallback = false)
    monitor.history("Kino Praha").head.fallback shouldBe false
    notifications shouldBe 2
  }
}
