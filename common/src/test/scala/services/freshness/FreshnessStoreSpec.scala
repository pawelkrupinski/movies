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
    store.markFresh("scrape|kino-x", CinemaScrape, t0) // 30min default TTL
    store.isFresh("scrape|kino-x", CinemaScrape, t0.plusSeconds(29 * 60)) shouldBe true
    store.isFresh("scrape|kino-x", CinemaScrape, t0.plusSeconds(31 * 60)) shouldBe false
  }

  it should "use the per-kind TTL — a 4h rating stays fresh long after a 30min scrape would be stale" in {
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
    Freshness.ttlFor(CinemaScrape)  shouldBe Some(30.minutes) // default; tunable via KINOWO_SCRAPE_FRESHNESS_MINUTES
    Freshness.ttlFor(DetailEnrich)  shouldBe Some(6.hours)
    Freshness.ttlFor(TmdbResolve)   shouldBe None
  }

  it should "let KINOWO_SCRAPE_FRESHNESS_MINUTES override the scrape TTL" in {
    val key = "KINOWO_SCRAPE_FRESHNESS_MINUTES"
    val previous = Option(System.getProperty(key))
    try {
      System.setProperty(key, "45")
      Freshness.ttlFor(CinemaScrape) shouldBe Some(45.minutes)
      // a non-positive / unparseable value falls back to the 30min default
      System.setProperty(key, "0")
      Freshness.ttlFor(CinemaScrape) shouldBe Some(30.minutes)
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

  "isReady" should "mirror whenReady's completion (gate for the detail/rating reapers)" in {
    import scala.concurrent.{Future, Promise}
    new InMemoryFreshnessStore().isReady(DetailEnrich) shouldBe true // nothing to hydrate
    val stillHydrating = new InMemoryFreshnessStore {
      override def whenReady(kind: FreshnessKind): Future[Unit] = Promise[Unit]().future
    }
    stillHydrating.isReady(DetailEnrich) shouldBe false
  }

  // The detail/rating stamps load in the REST phase, after the scrape phase. Their
  // reapers gate on `restReady`, which must stay pending until `loadRest` has run —
  // otherwise the first post-deploy tick reads the empty mirror as "all stale" and
  // re-enqueues the whole corpus (the recurring per-deploy spike).
  it should "signal rest-ready only AFTER loadRest has run" in {
    import scala.concurrent.Promise
    val scrapeReady = Promise[Unit]()
    val restReady   = Promise[Unit]()
    var sawRestPendingDuringLoad = false
    MongoFreshnessStore.hydrateInPhases(
      loadScrape  = () => true,
      scrapeReady = scrapeReady,
      loadRest    = () => { sawRestPendingDuringLoad = !restReady.future.isCompleted },
      restReady   = restReady,
      sleep       = _ => ()
    )
    sawRestPendingDuringLoad shouldBe true   // not signalled before the load finished
    restReady.future.isCompleted shouldBe true // signalled once it did
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
      loadScrape  = () => { synchronized { order = order :+ "scrape" }; true },
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

  // Boot-storm fix: a transient scrape-hydrate failure (a slow/throttled Mongo —
  // exactly what an in-progress storm causes) must NOT signal ready against an
  // empty mirror. If it did, the ScrapeReaper would see every cinema as stale and
  // re-scrape all ~300, throttling Mongo so the next restart hydrated slower still
  // and stormed again. Readiness is withheld and the load retried; it fires only
  // once a retry succeeds.
  it should "withhold readiness through a transient hydrate timeout and signal it only once a retry loads the stamps" in {
    import scala.concurrent.Promise
    val ready  = Promise[Unit]()
    var attempts = 0
    var sleeps   = 0
    MongoFreshnessStore.hydrateInPhases(
      loadScrape        = () => { attempts += 1; attempts >= 3 }, // two timeouts, then success
      scrapeReady       = ready,
      loadRest          = () => (),
      maxScrapeAttempts = 5,
      sleep             = _ => { sleeps += 1 }
    )
    attempts shouldBe 3 // stopped retrying the moment a load succeeded
    sleeps   shouldBe 2 // one wait between each of the three attempts
    ready.future.isCompleted shouldBe true
  }

  // The anti-wedge guarantee survives the retry: a genuinely-down Mongo (every
  // attempt fails) still signals ready once the bounded budget is spent, so the
  // reaper resumes (its per-tick cap bounds the burst) rather than never scraping.
  it should "still signal scrape-ready after the retry budget is spent if every attempt fails, so the reaper never wedges" in {
    import scala.concurrent.Promise
    val ready  = Promise[Unit]()
    var attempts = 0
    MongoFreshnessStore.hydrateInPhases(
      loadScrape        = () => { attempts += 1; throw new RuntimeException("mongo down") },
      scrapeReady       = ready,
      loadRest          = () => (),
      maxScrapeAttempts = 4,
      sleep             = _ => ()
    )
    attempts shouldBe 4 // retried the whole budget, not a single fail-open attempt
    ready.future.isCompleted shouldBe true
  }
}
