package services.freshness

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class FreshnessStoreSpec extends AnyFlatSpec with Matchers {
  import FreshnessKind._

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  "isFresh" should "be false for a key never marked" in {
    val store = new InMemoryFreshnessStore
    store.isFresh("scrape|kino-x", CinemaScrape, t0) shouldBe false
  }

  it should "be true within the kind's TTL and false past it" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("scrape|kino-x", CinemaScrape, t0) // 15min default TTL
    store.isFresh("scrape|kino-x", CinemaScrape, t0.plusSeconds(14 * 60)) shouldBe true
    store.isFresh("scrape|kino-x", CinemaScrape, t0.plusSeconds(16 * 60)) shouldBe false
  }

  it should "use the per-kind TTL — a 4h rating stays fresh long after a 15min scrape would be stale" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("imdb|movie", ImdbRating, t0) // 4h TTL
    store.isFresh("imdb|movie", ImdbRating, t0.plus(3, java.time.temporal.ChronoUnit.HOURS)) shouldBe true
    store.isFresh("imdb|movie", ImdbRating, t0.plus(5, java.time.temporal.ChronoUnit.HOURS)) shouldBe false
  }

  it should "treat a permanent kind (TMDB resolve) as fresh forever once stamped" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("tmdb|movie", TmdbResolve, t0)
    store.isFresh("tmdb|movie", TmdbResolve, t0.plus(3650, java.time.temporal.ChronoUnit.DAYS)) shouldBe true
  }

  "lastFetchedAt" should "return the recorded instant" in {
    val store = new InMemoryFreshnessStore
    store.lastFetchedAt("k") shouldBe None
    store.markFresh("k", DetailEnrich, t0)
    store.lastFetchedAt("k") shouldBe Some(t0)
  }

  "invalidate" should "drop a stamp so the work reads as stale again" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("imdb|tmdb:42", ImdbRating, t0)
    store.isFresh("imdb|tmdb:42", ImdbRating, t0.plusSeconds(60)) shouldBe true
    store.invalidate("imdb|tmdb:42")
    store.lastFetchedAt("imdb|tmdb:42") shouldBe None
    store.isFresh("imdb|tmdb:42", ImdbRating, t0.plusSeconds(60)) shouldBe false
  }

  it should "be a harmless no-op for an unknown key" in {
    val store = new InMemoryFreshnessStore
    noException should be thrownBy store.invalidate("never|stamped")
  }

  "Freshness.ttlFor" should "give 4h for every rating source and a permanent (None) TMDB resolve" in {
    Freshness.ttlFor(ImdbRating)    shouldBe Some(4.hours)
    Freshness.ttlFor(FilmwebRating) shouldBe Some(4.hours)
    Freshness.ttlFor(RtRating)      shouldBe Some(4.hours)
    Freshness.ttlFor(McRating)      shouldBe Some(4.hours)
    Freshness.ttlFor(CinemaScrape)  shouldBe Some(15.minutes) // default; tunable via KINOWO_SCRAPE_FRESHNESS_MINUTES
    Freshness.ttlFor(DetailEnrich)  shouldBe Some(6.hours)
    Freshness.ttlFor(TmdbResolve)   shouldBe None
  }

  it should "let KINOWO_SCRAPE_FRESHNESS_MINUTES override the scrape TTL" in {
    val key = "KINOWO_SCRAPE_FRESHNESS_MINUTES"
    val previous = Option(System.getProperty(key))
    try {
      System.setProperty(key, "45")
      Freshness.ttlFor(CinemaScrape) shouldBe Some(45.minutes)
      // a non-positive / unparseable value falls back to the 15min default
      System.setProperty(key, "0")
      Freshness.ttlFor(CinemaScrape) shouldBe Some(15.minutes)
    } finally previous match {
      case Some(v) => System.setProperty(key, v)
      case None    => System.clearProperty(key)
    }
  }

  "FreshnessKind labels" should "be unique and round-trip via byLabel" in {
    FreshnessKind.all.map(_.label).distinct should have size FreshnessKind.all.size
    FreshnessKind.all.foreach(k => FreshnessKind.byLabel(k.label) shouldBe Some(k))
  }

  "whenReady" should "be already complete for the in-memory store (nothing to hydrate)" in {
    new InMemoryFreshnessStore().whenReady(CinemaScrape).isCompleted shouldBe true
  }

  // Fix 2: the scrape stamps load first and signal readiness BEFORE the rest of
  // the (much larger) corpus hydrates, so the ScrapeReaper's gate opens off the
  // small scrape query, not behind thousands of detail/rating stamps.
  "MongoFreshnessStore.hydrateInPhases" should "signal scrape-ready after the scrape phase, while the rest is still loading" in {
    import scala.concurrent.Promise
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    val ready         = Promise[Unit]()
    val restStarted   = new CountDownLatch(1)
    val restMayFinish = new CountDownLatch(1)
    @volatile var order = List.empty[String]
    val t = new Thread(() => MongoFreshnessStore.hydrateInPhases(
      loadScrape  = () => synchronized { order = order :+ "scrape" },
      scrapeReady = ready,
      loadRest    = () => { restStarted.countDown(); restMayFinish.await(2, TimeUnit.SECONDS); synchronized { order = order :+ "rest" } }
    ))
    t.setDaemon(true); t.start()
    restStarted.await(2, TimeUnit.SECONDS) shouldBe true
    ready.future.isCompleted shouldBe true // scrape-ready fired though rest is still blocked
    restMayFinish.countDown()
    t.join(2000)
    order shouldBe List("scrape", "rest")
  }

  it should "still signal scrape-ready if the scrape phase fails, so the reaper never wedges" in {
    import scala.concurrent.Promise
    import scala.util.Try
    val ready = Promise[Unit]()
    Try(MongoFreshnessStore.hydrateInPhases(
      loadScrape  = () => throw new RuntimeException("mongo down"),
      scrapeReady = ready,
      loadRest    = () => ()
    ))
    ready.future.isCompleted shouldBe true
  }
}
