package services.tasks

import models.{CinemaCityChain, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, CinemaShowing, KinoApollo, Movie, MovieRecord, Showtime, Source, SourceData}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.cinemas.FakeDetailEnricher
import services.events.{DomainEvent, EventBus, InProcessEventBus, MovieDetailsComplete}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.UptimeMonitor
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.cinemas.common.{DetailEnricher, FilmDetail}

import java.time.LocalDateTime
import scala.concurrent.duration._

class EnrichDetailsHandlerSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  // The shared schedule the reaper enqueues on and this handler re-gates on. A
  // just-fetched detail is not yet due, so the "skip when fresh" case still holds.
  private val dueWindow = new DueWindow(6.hours)

  /** Records what the handler publishes, so a test can assert the
   *  detail-complete → MovieDetailsComplete re-trigger (and its absence). */
  private class CapturingBus extends EventBus {
    val published = scala.collection.mutable.ListBuffer.empty[DomainEvent]
    def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = ()
    def publish(event: DomainEvent): Unit = { published += event; () }
  }
  private val noBus = new CapturingBus

  /** A cache pre-seeded with one (KinoApollo, title) row whose slot carries
   *  showtimes but no detail — exactly what a bare scrape leaves behind. */
  private def seededCache(title: String) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
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
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    // Per-(cinema,title) slot: read via `cinemaData` (the listing slot the detail
    // merged into is `CinemaShowing`-keyed, not the bare cinema).
    val slot = cache.get(cache.keyOf("Dune", None)).flatMap(_.cinemaData.get(KinoApollo))
    slot.flatMap(_.synopsis) shouldBe Some("A great film")
    slot.map(_.cast)         shouldBe Some(Seq("Zendaya"))
    slot.map(_.showtimes.size) shouldBe Some(1) // showtimes from the scrape preserved
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich) shouldBe true
    successes(uptime, EnrichmentService) shouldBe 1 // recorded under "<cinema>|enrichment"
  }

  it should "merge detail into a decorated edition's EXISTING slot, not fabricate a base-title phantom slot" in {
    // A decorated edition ("Kino Konesera: Dune") folded onto the base "Dune" row:
    // the row is keyed by the base title, but its KinoApollo listing slot is keyed
    // by the decorated shown title. The EnrichDetails task carries the base title.
    val decorated = CinemaShowing(KinoApollo, "decorateddune") // != sanitize("Dune")
    val cache     = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    // Seed the base "Dune" row directly (bypassing repo title-re-derivation) with only
    // the decorated KinoApollo slot, as the fold would leave it.
    cache.put(cache.keyOf("Dune", None), MovieRecord(data = Map(decorated -> SourceData(
      title = Some("Kino Konesera: Dune"),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book")))))))
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(synopsis = Some("Spice"), director = Seq("Denis Villeneuve"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(), noBus, dueWindow)

    h.handle(taskFor("kino-apollo", cache, "Dune", enricher)) shouldBe Done
    val row = cache.get(cache.keyOf("Dune", None)).get
    // Detail merged INTO the decorated slot (showtimes preserved) — not lost to a phantom.
    row.data.get(decorated).flatMap(_.synopsis)     shouldBe Some("Spice")
    row.data.get(decorated).map(_.director)         shouldBe Some(Seq("Denis Villeneuve"))
    row.data.get(decorated).map(_.showtimes.size)   shouldBe Some(1)
    // No base-title phantom fabricated: still exactly one KinoApollo slot.
    row.data.keys.count(s => Source.cinemaOf(s).contains(KinoApollo)) shouldBe 1
    row.data.get(CinemaShowing.keyFor(KinoApollo, "Dune"))            shouldBe None
  }

  it should "land detail on EVERY programme-edition slot of a cinema (no phantom) when the film runs as several editions" in {
    // Kino Atlantic runs "Ojczyzna" as three programme editions — each its own card.
    val a = CinemaShowing(KinoApollo, "poradlaseniorao")
    val b = CinemaShowing(KinoApollo, "zadrzwiamio")
    val c = CinemaShowing(KinoApollo, "opokazprzedpremierowy")
    def slot(t: String) = SourceData(title = Some(t),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    cache.put(cache.keyOf("Ojczyzna", None),
      MovieRecord(data = Map(a -> slot("Pora dla seniora: Ojczyzna"), b -> slot("Za drzwiami: Ojczyzna"), c -> slot("Ojczyzna przedpremierowo"))))
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(director = Seq("Jan Komasa"))))
    val h = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(), noBus, dueWindow)

    h.handle(taskFor("kino-apollo", cache, "Ojczyzna", enricher)) shouldBe Done
    val row = cache.get(cache.keyOf("Ojczyzna", None)).get
    // Every edition slot gained the film's director; showtimes preserved.
    Seq(a, b, c).foreach { s => row.data.get(s).map(_.director) shouldBe Some(Seq("Jan Komasa")) }
    // Still exactly three slots — no base-title phantom fabricated.
    row.data.keys.count(s => Source.cinemaOf(s).contains(KinoApollo)) shouldBe 3
    row.data.get(CinemaShowing.keyFor(KinoApollo, "Ojczyzna")) shouldBe None
  }

  it should "clear detailPending and publish MovieDetailsComplete (the TMDB re-trigger) once a held-back row's detail lands" in {
    val cache    = seededCache("Hamnet")
    val key      = cache.keyOf("Hamnet", None)
    cache.putIfPresent(key, _.copy(detailPending = true)) // held back awaiting its detail
    val bus      = new CapturingBus
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(synopsis = Some("..."), director = Seq("Chloé Zhao"), originalTitle = Some("Hamnet"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(), bus, dueWindow)

    h.handle(taskFor("kino-apollo", cache, "Hamnet", enricher)) shouldBe Done
    // Released from the read-model / TMDB gate now that the detail is in.
    cache.get(key).map(_.detailPending) shouldBe Some(false)
    // The TMDB re-trigger carries the detail-page director + original title — the
    // hints a director-less first scrape lacked.
    bus.published.toList shouldBe List(MovieDetailsComplete("Hamnet", None, Some("Hamnet"), Some("Chloé Zhao")))
  }

  it should "NOT re-trigger TMDB when refreshing a row that wasn't awaiting detail (no detailPending)" in {
    val cache    = seededCache("Dune") // detailPending defaults false — a plain refresh
    val bus      = new CapturingBus
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("x"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(), bus, dueWindow)

    h.handle(taskFor("kino-apollo", cache, "Dune", enricher)) shouldBe Done
    bus.published shouldBe empty // a periodic detail refresh mustn't churn the TMDB stage
  }

  it should "write a chain enricher's detail into its shared network source, leaving venue slots untouched, so every venue shows it" in {
    // Two Cinema City venues scrape the same film (bare: showtimes only, no detail).
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
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
      target = Some(CinemaCityChain), uptimeOverride = Some("Cinema City Enrichment"))
    val h        = new EnrichDetailsHandler(Map("cinema-city" -> enricher), cache, fresh, uptime, noBus, dueWindow)
    val task     = taskFor("cinema-city", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    val record = cache.get(cache.keyOf("Dune", None)).get
    // Detail landed in the shared network slot (created on demand — no venue scrapes it).
    record.data.get(CinemaCityChain).flatMap(_.synopsis) shouldBe Some("Spice must flow")
    record.data.get(CinemaCityChain).map(_.genres)       shouldBe Some(Seq("Sci-Fi"))
    // Venue slots (per-title `CinemaShowing` keys) keep their showtimes and gained
    // no detail of their own.
    record.cinemaData.get(CinemaCityPoznanPlaza).map(_.showtimes.size) shouldBe Some(1)
    record.cinemaData.get(CinemaCityPoznanPlaza).flatMap(_.synopsis)   shouldBe None
    record.cinemaData.get(CinemaCityKinepolis).flatMap(_.synopsis)     shouldBe None
    // Film-level merged accessors surface the shared detail for the whole row.
    record.synopsis shouldBe Some("Spice must flow")
    record.genres   shouldBe Seq("Sci-Fi")
    // Health recorded once under the global name, not per venue.
    successes(uptime, "Cinema City Enrichment") shouldBe 1
    uptime.services should not contain UptimeMonitor.enrichmentService(CinemaCityPoznanPlaza.displayName)
  }

  it should "skip without fetching when the detail is already fresh, recording no uptime" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor()
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("x"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich)

    h.handle(task) shouldBe Skipped
    enricher.calls shouldBe 0
    uptime.services shouldBe empty // a skip did no work, so nothing recorded
  }

  it should "drop a task whose detail group has no enricher" in {
    val cache = seededCache("Dune")
    val h     = new EnrichDetailsHandler(Map.empty, cache, new InMemoryFreshnessStore, new UptimeMonitor(), noBus, dueWindow)
    val task  = taskFor("gone", cache, "Dune", new FakeDetailEnricher(KinoApollo, "gone", None))
    h.handle(task) shouldBe Done
  }

  it should "record a failure and stay stale when the fetch yields nothing (so the next scrape retries)" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor()
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", None) // fetch failed/absent
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow)
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
