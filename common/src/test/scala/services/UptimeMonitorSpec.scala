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

  it should "not let a closed-client Mongo write break recording" in {
    val monitor = new UptimeMonitor(Some(closedClientDb))

    // Before the fix these throw IllegalStateException synchronously; the
    // in-memory bucket must still update regardless of the Mongo write.
    noException should be thrownBy {
      monitor.recordSuccess("Kino Rialto")
      monitor.recordFailure("Kino Rialto", "TimeoutException: boom")
    }

    val history = monitor.history("Kino Rialto").head
    history.successes shouldBe 1
    history.failures shouldBe 1
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
