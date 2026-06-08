package services.tasks

import models.{CinemaCityChain, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.{DetailEnricher, FakeDetailEnricher, FilmDetail}
import services.events.InProcessEventBus
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import java.time.LocalDateTime

class EnrichDetailsHandlerSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  /** A cache pre-seeded with one (KinoApollo, title) row whose slot carries
   *  showtimes but no detail — exactly what a bare scrape leaves behind. */
  private def seededCache(title: String) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus())
    val bare = CinemaMovie(Movie(title), KinoApollo, posterUrl = None, filmUrl = Some("http://ref"),
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    cache.recordCinemaScrape(KinoApollo, Seq(bare))
    cache
  }

  private def taskFor(group: String, cache: CaffeineMovieCache, title: String, enricher: DetailEnricher) = {
    val key = cache.keyOf(title, None)
    Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.dedupKey(group, key),
      EnrichDetailsTasks.payload(enricher, key, "http://ref"), attempts = 1)
  }

  private val EnrichmentService = UptimeMonitor.enrichmentService(KinoApollo.displayName)
  private def successes(m: UptimeMonitor, service: String) = m.history(service).map(_.successes).sum
  private def failures(m: UptimeMonitor, service: String)  = m.history(service).map(_.failures).sum

  "EnrichDetailsHandler" should "merge fetched detail into the cinema slot, preserving showtimes, and mark fresh" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor()
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("A great film"), cast = Seq("Zendaya"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    val slot = cache.get(cache.keyOf("Dune", None)).flatMap(_.data.get(KinoApollo))
    slot.flatMap(_.synopsis) shouldBe Some("A great film")
    slot.map(_.cast)         shouldBe Some(Seq("Zendaya"))
    slot.map(_.showtimes.size) shouldBe Some(1) // showtimes from the scrape preserved
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich) shouldBe true
    successes(uptime, EnrichmentService) shouldBe 1 // recorded under "<cinema>|enrichment"
  }

  it should "write a chain enricher's detail into its shared network source, leaving venue slots untouched, so every venue shows it" in {
    // Two Cinema City venues scrape the same film (bare: showtimes only, no detail).
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus())
    def bareAt(venue: models.Cinema) = CinemaMovie(Movie("Dune"), venue, posterUrl = None,
      filmUrl = Some("http://ref"), synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    cache.recordCinemaScrape(CinemaCityPoznanPlaza, Seq(bareAt(CinemaCityPoznanPlaza)))
    cache.recordCinemaScrape(CinemaCityKinepolis, Seq(bareAt(CinemaCityKinepolis)))

    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor()
    val detail   = FilmDetail(synopsis = Some("Spice must flow"), cast = Seq("Zendaya"), genres = Seq("Sci-Fi"))
    // A chain enricher: one shared group, detail written to the CinemaCityChain
    // network source, health under one global name.
    val enricher = new FakeDetailEnricher(CinemaCityPoznanPlaza, "cinema-city", Some(detail),
      target = Some(CinemaCityChain), uptimeOverride = Some("Globalne: Cinema City"))
    val h        = new EnrichDetailsHandler(Map("cinema-city" -> enricher), cache, fresh, uptime)
    val task     = taskFor("cinema-city", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    val record = cache.get(cache.keyOf("Dune", None)).get
    // Detail landed in the shared network slot (created on demand — no venue scrapes it).
    record.data.get(CinemaCityChain).flatMap(_.synopsis) shouldBe Some("Spice must flow")
    record.data.get(CinemaCityChain).map(_.genres)       shouldBe Some(Seq("Sci-Fi"))
    // Venue slots keep their showtimes and gained no detail of their own.
    record.data.get(CinemaCityPoznanPlaza).map(_.showtimes.size) shouldBe Some(1)
    record.data.get(CinemaCityPoznanPlaza).flatMap(_.synopsis)   shouldBe None
    record.data.get(CinemaCityKinepolis).flatMap(_.synopsis)     shouldBe None
    // Film-level merged accessors surface the shared detail for the whole row.
    record.synopsis shouldBe Some("Spice must flow")
    record.genres   shouldBe Seq("Sci-Fi")
    // Health recorded once under the global name, not per venue.
    successes(uptime, "Globalne: Cinema City") shouldBe 1
    uptime.services should not contain UptimeMonitor.enrichmentService(CinemaCityPoznanPlaza.displayName)
  }

  it should "skip without fetching when the detail is already fresh, recording no uptime" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor()
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("x"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich)

    h.handle(task) shouldBe Skipped
    enricher.calls shouldBe 0
    uptime.services shouldBe empty // a skip did no work, so nothing recorded
  }

  it should "drop a task whose detail group has no enricher" in {
    val cache = seededCache("Dune")
    val h     = new EnrichDetailsHandler(Map.empty, cache, new InMemoryFreshnessStore, new UptimeMonitor())
    val task  = taskFor("gone", cache, "Dune", new FakeDetailEnricher(KinoApollo, "gone", None))
    h.handle(task) shouldBe Done
  }

  it should "record a failure and stay stale when the fetch yields nothing (so the next scrape retries)" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor()
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", None) // fetch failed/absent
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich) shouldBe false
    cache.get(cache.keyOf("Dune", None)).flatMap(_.data.get(KinoApollo)).flatMap(_.synopsis) shouldBe None
    failures(uptime, EnrichmentService) shouldBe 1 // red/yellow on the enrichment bar
  }

  // Requirement: a task with exactly the same definition (enrich a specific film
  // for a specific cinema/group) must be rejected as a duplicate.
  "the queue" should "reject a duplicate EnrichDetails task for the same (group, film)" in {
    val cache    = seededCache("Dune")
    val queue    = new InMemoryTaskQueue
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail()))
    val key      = cache.keyOf("Dune", None)
    val dk       = EnrichDetailsTasks.dedupKey("kino-apollo", key)
    queue.enqueue(TaskType.EnrichDetails, dk, EnrichDetailsTasks.payload(enricher, key, "http://ref")) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.EnrichDetails, dk, EnrichDetailsTasks.payload(enricher, key, "http://ref")) shouldBe EnqueueResult.Duplicate
  }
}
