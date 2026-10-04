package services.tasks

import models.{CinemaCityChain, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, CinemaShowing, KinoApollo, Movie, MovieRecord, Showtime, Source, SourceData}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.cinemas.FakeDetailEnricher
import services.events.RecordingEventBus
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.UptimeMonitor
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import tools.HttpStatusException

import java.time.{Instant, LocalDateTime}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.venuepages.{InMemoryVenuePageStore, VenuePage, VenuePageKey}

class EnrichDetailsHandlerSpec extends AnyFlatSpec with Matchers {

  // Handlers and caches run at one fixed instant, so "two days on" is measured on it.
  private val specClock = java.time.Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  import HandlerOutcome._

  // The shared schedule the reaper enqueues on and this handler re-gates on. A
  // just-fetched detail is not yet due, so the "skip when fresh" case still holds.
  private val dueWindow = new DueWindow(6.hours)

  private val noBus = new RecordingEventBus

  /** The handler takes its country's language from the composition root; these specs are Poland's. */
  private val polish = models.Country.Poland.language

  /** A cache pre-seeded with one (KinoApollo, title) row whose slot carries
   *  showtimes but no detail — exactly what a bare scrape leaves behind. */
  private def seededCache(title: String, listedYear: Option[Int] = None, listedCountries: Seq[String] = Nil) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val bare = CinemaMovie(Movie(title, releaseYear = listedYear, countries = listedCountries), KinoApollo, posterUrl = None, filmUrl = Some("http://ref"),
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    services.movies.ListingSeed.land(cache, KinoApollo, Seq(bare))
    cache
  }

  private def taskFor(group: String, cache: CaffeineMovieCache, title: String, enricher: DetailEnricher,
                      year: Option[Int] = None) = {
    val key = cache.keyOf(title, year)
    Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.dedupKey(group, key),
      EnrichDetailsTasks.payload(enricher, key, "http://ref"), attempts = 1)
  }

  private val EnrichmentService = UptimeMonitor.enrichmentService(KinoApollo.displayName)
  private def successes(m: UptimeMonitor, service: String) = m.history(service).map(_.successes).sum
  private def failures(m: UptimeMonitor, service: String)  = m.history(service).map(_.failures).sum

  "EnrichDetailsHandler" should "merge fetched detail into the cinema slot, preserving showtimes, and mark fresh" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("A great film"), cast = Seq("Zendaya"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    // Per-(cinema,title) slot: read via `cinemaData` (the listing slot the detail
    // merged into is `CinemaShowing`-keyed, not the bare cinema).
    val slot = cache.get(cache.keyOf("Dune", None)).flatMap(_.cinemaData.get(KinoApollo))
    slot.flatMap(_.synopsis) shouldBe Some("A great film")
    slot.map(_.cast)         shouldBe Some(Seq("Zendaya"))
    slot.map(_.showtimes.size) shouldBe Some(1) // showtimes from the scrape preserved
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant()) shouldBe true
    successes(uptime, EnrichmentService) shouldBe 1 // recorded under "<cinema>|enrichment"
  }

  it should "merge detail into a decorated edition's EXISTING slot, not fabricate a base-title phantom slot" in {
    // A decorated edition ("Kino Konesera: Dune") folded onto the base "Dune" row:
    // the row is keyed by the base title, but its KinoApollo listing slot is keyed
    // by the decorated shown title. The EnrichDetails task carries the base title.
    val decorated = CinemaShowing(KinoApollo, "decorateddune") // != sanitize("Dune")
    val cache     = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    // Seed the base "Dune" row directly (bypassing repo title-re-derivation) with only
    // the decorated KinoApollo slot, as the fold would leave it.
    cache.put(cache.keyOf("Dune", None), MovieRecord(data = Map(decorated -> SourceData(
      title = Some("Kino Konesera: Dune"),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book")))))))
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(synopsis = Some("Spice"), director = Seq("Denis Villeneuve"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)

    h.handle(taskFor("kino-apollo", cache, "Dune", enricher)) shouldBe Done
    val row = cache.get(cache.keyOf("Dune", None)).get
    // Detail merged INTO the decorated slot (showtimes preserved) — not lost to a phantom.
    row.data.get(decorated).flatMap(_.synopsis)     shouldBe Some("Spice")
    row.data.get(decorated).map(_.director)         shouldBe Some(Seq("Denis Villeneuve"))
    row.data.get(decorated).map(_.showtimes.size)   shouldBe Some(1)
    // No base-title phantom fabricated: still exactly one KinoApollo slot.
    row.data.keys.count(s => Source.cinemaOf(s).contains(KinoApollo)) shouldBe 1
    row.data.get(CinemaShowing.keyFor(KinoApollo, "Dune", titleNormalizer))            shouldBe None
  }

  it should "land detail on EVERY programme-edition slot of a cinema (no phantom) when the film runs as several editions" in {
    // Kino Atlantic runs "Ojczyzna" as three programme editions — each its own card.
    val a = CinemaShowing(KinoApollo, "poradlaseniorao")
    val b = CinemaShowing(KinoApollo, "zadrzwiamio")
    val c = CinemaShowing(KinoApollo, "opokazprzedpremierowy")
    def slot(t: String) = SourceData(title = Some(t),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("Ojczyzna", None),
      MovieRecord(data = Map(a -> slot("Pora dla seniora: Ojczyzna"), b -> slot("Za drzwiami: Ojczyzna"), c -> slot("Ojczyzna przedpremierowo"))))
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(director = Seq("Jan Komasa"))))
    val h = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)

    h.handle(taskFor("kino-apollo", cache, "Ojczyzna", enricher)) shouldBe Done
    val row = cache.get(cache.keyOf("Ojczyzna", None)).get
    // Every edition slot gained the film's director; showtimes preserved.
    Seq(a, b, c).foreach { s => row.data.get(s).map(_.director) shouldBe Some(Seq("Jan Komasa")) }
    // Still exactly three slots — no base-title phantom fabricated.
    row.data.keys.count(s => Source.cinemaOf(s).contains(KinoApollo)) shouldBe 3
    row.data.get(CinemaShowing.keyFor(KinoApollo, "Ojczyzna", titleNormalizer)) shouldBe None
  }

  // Kinoteka runs "Rozważna i romantyczna" as two programme editions — "| Kino dla
  // rodzica" and "| Kino przy herbatce" — each with its OWN detail page (112 vs 131
  // minutes, a different synopsis and billing). One detail task serves the whole row, on
  // the representative slot's url; landing that one page on every edition made the
  // re-read (authoritative) overwrite the sibling edition with a page that is not its
  // own, so the herbatce slot changed between two days of identical listings.
  it should "leave an edition slot that has its OWN detail page alone, even on a re-read" in {
    val parents = CinemaShowing(KinoApollo, "rozwaznairomantycznakinodlarodzica")
    val tea     = CinemaShowing(KinoApollo, "rozwaznairomantycznakinoprzyherbatce")
    def slot(title: String, url: String, runtime: Int) = SourceData(title = Some(title), filmUrl = Some(url),
      runtimeMinutes = Some(runtime), showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 11, 0), Some("https://book"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key   = cache.keyOf("Rozważna i romantyczna", Some(2026))
    cache.put(key, MovieRecord(data = Map(
      parents -> slot("Rozważna i romantyczna | Kino dla rodzica", "http://ref", 112),
      tea     -> slot("Rozważna i romantyczna | Kino przy herbatce", "http://tea", 131))))
    val fresh    = new InMemoryFreshnessStore
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(runtimeMinutes = Some(112))))
    val task     = taskFor("kino-apollo", cache, "Rozważna i romantyczna", enricher, year = Some(2026))
    def handle() = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
      .handle(task) shouldBe Done

    handle()
    // The next day's re-read — authoritative over what the slots hold.
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant().minus(2, ChronoUnit.DAYS))
    handle()

    val row = cache.get(key).get
    row.data.get(parents).flatMap(_.runtimeMinutes) shouldBe Some(112)
    withClue("the herbatce edition's own page says 131; the parents' page is not its to overwrite: ")(
      row.data.get(tea).flatMap(_.runtimeMinutes) shouldBe Some(131))
  }

  it should "write a chain enricher's detail into its shared network source, leaving venue slots untouched, so every venue shows it" in {
    // Two Cinema City venues scrape the same film (bare: showtimes only, no detail).
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    def bareAt(venue: models.Cinema) = CinemaMovie(Movie("Dune"), venue, posterUrl = None,
      filmUrl = Some("http://ref"), synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    services.movies.ListingSeed.land(cache, CinemaCityPoznanPlaza, Seq(bareAt(CinemaCityPoznanPlaza)))
    services.movies.ListingSeed.land(cache, CinemaCityKinepolis, Seq(bareAt(CinemaCityKinepolis)))

    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val detail   = FilmDetail(synopsis = Some("Spice must flow"), cast = Seq("Zendaya"), genres = Seq("Sci-Fi"))
    // A chain enricher: one shared group, detail written to the CinemaCityChain
    // network source, health under one global name.
    val enricher = new FakeDetailEnricher(CinemaCityPoznanPlaza, "cinema-city", Some(detail),
      target = Some(CinemaCityChain), uptimeOverride = Some("Cinema City Enrichment"))
    val h        = new EnrichDetailsHandler(Map("cinema-city" -> enricher), cache, fresh, uptime, noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
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
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("x"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant())

    h.handle(task) shouldBe Skipped
    enricher.calls shouldBe 0
    uptime.services shouldBe empty // a skip did no work, so nothing recorded
  }

  it should "drop a task whose detail group has no enricher" in {
    val cache = seededCache("Dune")
    val h     = new EnrichDetailsHandler(Map.empty, cache, new InMemoryFreshnessStore, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
    val task  = taskFor("gone", cache, "Dune", new FakeDetailEnricher(KinoApollo, "gone", None))
    h.handle(task) shouldBe Done
  }

  it should "record a failure and stay stale when the fetch yields nothing (so the next scrape retries)" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", None) // fetch failed/absent
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant()) shouldBe false
    cache.get(cache.keyOf("Dune", None)).flatMap(_.data.get(KinoApollo)).flatMap(_.synopsis) shouldBe None
    failures(uptime, EnrichmentService) shouldBe 1 // red/yellow on the enrichment bar
  }

  // A page the cinema has taken down is not a failing fetch, it is a settled
  // answer. Left stale it never gets a freshness stamp, `DueWindow.isDue` is then
  // unconditionally true, and DetailReaper re-enqueues the film every tick —
  // once a minute, forever, each pass burning an /uptime failure. Two such films
  // held prod's "Cinema City Enrichment" row at ~90% failures.
  it should "stamp a DURABLY gone detail page, so the film stops being retried every tick" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      failure = Some(new HttpStatusException(404, "GET", "http://ref", None)))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant()) shouldBe true
    failures(uptime, EnrichmentService) shouldBe 1 // still reported — once per window, not once a minute
    // Stamping is "we asked", never "we have data": no detail is invented for the row.
    cache.get(cache.keyOf("Dune", None)).flatMap(_.cinemaData.get(KinoApollo)).flatMap(_.synopsis) shouldBe None

    // The stamp is what closes the loop: the next pass inside the window is
    // skipped at pickup instead of re-fetching a page that will not come back.
    h.handle(task) shouldBe Skipped
    enricher.calls shouldBe 1
  }

  it should "leave a TRANSIENTLY failed detail stale, so the next tick still retries it" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    // 503 describes the moment, not the url — the every-tick retry is correct here.
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      failure = Some(new HttpStatusException(503, "GET", "http://ref", None)))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, uptime, noBus, dueWindow, clock = specClock, enrichmentLanguage = polish)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant()) shouldBe false
    failures(uptime, EnrichmentService) shouldBe 1
    h.handle(task) shouldBe Done // still due, fetched again
    enricher.calls shouldBe 2
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

  // Kino Kolory's page lists "Biograficzny/Muzyczny" as one genre and Kino Scena Kultura's poster has raw
  // spaces: a detail page's fields land by the listing's own rules (`SlotFields`), and a re-read of the
  // page as it was changes nothing.
  it should "land a detail page's genres and poster as the listing's land" in {
    val cache    = seededCache("Mariinka")
    val pages    = new InMemoryVenuePageStore
    val fresh    = new InMemoryFreshnessStore
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(genres = Seq("Biograficzny/Muzyczny"),
      posterUrl = Some("https://kino.pl/plakaty/Czas, który nie nadszedł.jpg"))))
    val task     = taskFor("kino-apollo", cache, "Mariinka", enricher)
    def handle() = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned),
      noBus, dueWindow, clock = specClock, pages = pages).handle(task) shouldBe Done
    def slot = cache.get(cache.keyOf("Mariinka", None)).flatMap(_.cinemaData.get(KinoApollo))

    handle()
    slot.map(_.genres) shouldBe Some(Seq("Biograficzny", "Muzyczny"))
    slot.flatMap(_.posterUrl) shouldBe Some("https://kino.pl/plakaty/Czas,%20który%20nie%20nadszedł.jpg")
    val landed = slot
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant().minus(2, ChronoUnit.DAYS))
    handle()
    withClue("a re-read of an unchanged page must not rewrite the slot: ")(slot shouldBe landed)
  }

  // End to end: the venue reused its URL for a different film, which is what Kino
  // Pionier did to `pionier1907.pl/event/lalka` — Wojciech Has's 1968 picture,
  // then the 2026 one. `DetailReaper` re-reads that page every 6h, but the
  // fill-only merge had nothing left to fill, so the first film's year survived
  // every re-read and kept a whole row keyed `lalka|1968`.
  it should "correct a slot when a re-fetch of the same URL returns a different film" in {
    val cache = seededCache("Lalka")
    val fresh = new InMemoryFreshnessStore
    val had   = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(releaseYear = Some(1968), runtimeMinutes = Some(151), director = Seq("Wojciech Has"))))
    val task  = taskFor("kino-apollo", cache, "Lalka", had)
    val pages = new InMemoryVenuePageStore

    new EnrichDetailsHandler(Map("kino-apollo" -> had), cache, fresh, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish, pages = pages)
      .handle(task) shouldBe Done
    cache.get(cache.keyOf("Lalka", None)).flatMap(_.cinemaData.get(KinoApollo))
      .flatMap(_.releaseYear) shouldBe Some(1968)

    // Two days on, the reaper re-reads that URL — and it is a different film now.
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant().minus(2, ChronoUnit.DAYS))
    val has = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(releaseYear = Some(2026), runtimeMinutes = Some(162), director = Seq("Maciej Kawalski"))))
    new EnrichDetailsHandler(Map("kino-apollo" -> has), cache, fresh, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish, pages = pages)
      .handle(task) shouldBe Done

    val slot = cache.get(cache.keyOf("Lalka", None)).flatMap(_.cinemaData.get(KinoApollo))
    slot.flatMap(_.releaseYear)    shouldBe Some(2026)
    slot.flatMap(_.runtimeMinutes) shouldBe Some(162)
    slot.map(_.director)           shouldBe Some(Seq("Maciej Kawalski"))
    withClue("the listing's showtimes must survive a refresh: ")(slot.map(_.showtimes.size) shouldBe Some(1))
  }

  // A 404 is not a read. The `Gone` branch stamps the task's own freshness key to stop
  // the re-enqueue livelock, and reading that back as "we have seen this page" made a
  // recovered page's FIRST real read authoritative — so the detail page could overwrite
  // the listing's year and re-key the row, which is exactly what the fill-only first
  // read exists to prevent.
  it should "treat the first successful read as a first read even after the page 404ed" in {
    // The LISTING already states 2026. A first read must not be able to overrule it.
    val cache = seededCache("Lalka", listedYear = Some(2026))
    val fresh = new InMemoryFreshnessStore
    val gone  = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      failure = Some(new HttpStatusException(404, "GET", "http://ref", None)))
    val task  = taskFor("kino-apollo", cache, "Lalka", gone, year = Some(2026))
    val pages = new InMemoryVenuePageStore

    new EnrichDetailsHandler(Map("kino-apollo" -> gone), cache, fresh, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish, pages = pages)
      .handle(task) shouldBe Done

    // Days later the page comes back. Age the 404's own stamp so the due gate lets
    // this through — that stamp is the livelock guard, not evidence of a read.
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant().minus(2, ChronoUnit.DAYS))
    // This is the page's FIRST read, so it must FILL, not overwrite — the listing's
    // own year has to survive.
    val back = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(releaseYear = Some(1968), runtimeMinutes = Some(151), director = Seq("Wojciech Has"))))
    new EnrichDetailsHandler(Map("kino-apollo" -> back), cache, fresh, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish, pages = pages)
      .handle(task) shouldBe Done

    val slot = cache.get(cache.keyOf("Lalka", Some(2026))).flatMap(_.cinemaData.get(KinoApollo))
    withClue(s"slot=$slot: ")(slot.flatMap(_.releaseYear) shouldBe Some(2026))
  }

  // Convergence run 37165536574: the next day re-read every venue page, and each re-read
  // rewrote the slot with the page's own words over the listing's — Kino Atlantic's
  // canonical "Holandia" back to the page's "Niderlandy", undone by the next listing build,
  // redone by the next re-read. A page that says what it said before has told us nothing.
  it should "leave the listing's own fields alone when a re-read finds the page unchanged" in {
    val cache = seededCache("Mariinka", listedCountries = Seq("Belgia", "Niderlandy", "Niemcy"))
    val fresh = new InMemoryFreshnessStore
    val pages = new InMemoryVenuePageStore
    val page  = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(countries = Seq("Belgia", "Niderlandy", "Niemcy"), genres = Seq("Dokument"), runtimeMinutes = Some(95))))
    val task  = taskFor("kino-apollo", cache, "Mariinka", page)
    def handler = new EnrichDetailsHandler(Map("kino-apollo" -> page), cache, fresh,
      new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish, pages = pages)
    def slot = cache.get(cache.keyOf("Mariinka", None)).flatMap(_.cinemaData.get(KinoApollo))

    // The listing's countries as the slot builder canonicalised them — not the page's spelling.
    val listed = slot.map(_.countries)
    listed should not be Some(Seq("Belgia", "Niderlandy", "Niemcy"))
    handler.handle(task) shouldBe Done
    slot.map(_.countries)          shouldBe listed
    slot.flatMap(_.runtimeMinutes) shouldBe Some(95)

    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant().minus(2, ChronoUnit.DAYS))
    handler.handle(task) shouldBe Done
    withClue("an unchanged page must not overrule the listing: ")(slot.map(_.countries) shouldBe listed)
  }

  // Poland's convergence leg: a page filling a slot whose listing carried no countries stored them
  // as the page spells them — Kino Atlantic's "Niderlandy" — where every listing-built slot
  // (`CinemaSlotBuilder`) holds the canonical "Holandia": one film, two spellings across venues,
  // flipped by the next canonicalised write.
  it should "canonicalise the countries a page fills, in the country's enrichment language" in {
    def filled(language: java.util.Locale) = {
      val cache = seededCache("Mariinka")
      val page  = new FakeDetailEnricher(KinoApollo, "kino-apollo",
        Some(FilmDetail(countries = Seq("Niderlandy", "Belgia", "Holandia"))))
      new EnrichDetailsHandler(Map("kino-apollo" -> page), cache, new InMemoryFreshnessStore,
        new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock,
        enrichmentLanguage = language).handle(taskFor("kino-apollo", cache, "Mariinka", page)) shouldBe Done
      cache.get(cache.keyOf("Mariinka", None)).flatMap(_.cinemaData.get(KinoApollo)).map(_.countries)
    }
    filled(polish) shouldBe Some(Seq("Holandia", "Belgia"))
    filled(java.util.Locale.UK)                      shouldBe Some(Seq("Netherlands", "Belgium"))
  }

  it should "canonicalise the countries a re-read overrules the slot with" in {
    val cache = seededCache("Mariinka")
    val fresh = new InMemoryFreshnessStore
    val pages = new InMemoryVenuePageStore
    def read(countries: String*) = {
      val page = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(countries = countries)))
      val task = taskFor("kino-apollo", cache, "Mariinka", page)
      fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich, specClock.instant().minus(2, ChronoUnit.DAYS))
      new EnrichDetailsHandler(Map("kino-apollo" -> page), cache, fresh,
        new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish, pages = pages)
        .handle(task) shouldBe Done
      cache.get(cache.keyOf("Mariinka", None)).flatMap(_.cinemaData.get(KinoApollo)).map(_.countries)
    }
    read("Francja")              shouldBe Some(Seq("Francja"))
    read("Niderlandy", "Belgia") shouldBe Some(Seq("Holandia", "Belgia"))
    withClue("a page re-spelling the same countries says nothing new: ")(read("Holandia", "Belgia") shouldBe Some(Seq("Holandia", "Belgia")))
  }

  // A cut-over country asks per PAGE, and the page's read stamp is written by the read itself, before
  // the merge: read back as "seen before", it made a page's FIRST read authoritative over the listing.
  it should "fill, not overrule, on a page's first read when asked per page" in {
    val cache = seededCache("Lalka", listedYear = Some(2026))
    val page  = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      Some(FilmDetail(releaseYear = Some(1968), runtimeMinutes = Some(151))))
    val key   = cache.keyOf("Lalka", Some(2026))
    val task  = Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.pageDedupKey("kino-apollo", "http://ref"),
      EnrichDetailsTasks.payload(page, key, "http://ref"), attempts = 1)
    new EnrichDetailsHandler(Map("kino-apollo" -> page), cache, new InMemoryFreshnessStore,
      new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish).handle(task) shouldBe Done

    val slot = cache.get(key).flatMap(_.cinemaData.get(KinoApollo))
    withClue(s"slot=$slot: ")(slot.flatMap(_.releaseYear) shouldBe Some(2026))
    slot.flatMap(_.runtimeMinutes) shouldBe Some(151)
  }

  // Convergence run 37165536574: two films titled "Lalka" with no year — keys `lalka|` and
  // `lalka~1164|` — and the venue's listing on the second. The task carried only the title and
  // year, so the handler re-derived `lalka|`, found no slot of the venue on THAT film, and
  // fabricated one there: one listing held by two films.
  it should "land the page on the row the task was asked for, not on another film of the same title" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val showing = CinemaShowing.keyFor(KinoApollo, "Lalka", titleNormalizer)
    val bare    = services.movies.CacheKey.stored("Lalka", "lalka|")
    val second  = services.movies.CacheKey.stored("Lalka", "lalka~1164|")
    cache.put(bare, MovieRecord(data = Map(CinemaShowing.keyFor(CinemaCityKinepolis, "Lalka", titleNormalizer) -> SourceData(title = Some("Lalka")))))
    cache.put(second, MovieRecord(data = Map(showing -> SourceData(title = Some("Lalka"), filmUrl = Some("http://ref"),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book")))))))
    val page = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(runtimeMinutes = Some(170))))
    val task = Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.pageDedupKey("kino-apollo", "http://ref"),
      EnrichDetailsTasks.payload(page, second, "http://ref"), attempts = 1)
    new EnrichDetailsHandler(Map("kino-apollo" -> page), cache, new InMemoryFreshnessStore,
      new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish).handle(task) shouldBe Done

    cache.get(second).flatMap(_.data.get(showing)).flatMap(_.runtimeMinutes) shouldBe Some(170)
    withClue("the other film must not gain the venue's listing: ")(cache.get(bare).map(_.data.contains(showing)) shouldBe Some(false))
  }

  // A task queued by the previous build carries no stored key: it must still land, by title and year,
  // rather than fail or stall the queue across the deploy.
  it should "still land a task queued without the row's stored key" in {
    val cache    = seededCache("Dune")
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(director = Seq("Denis Villeneuve"))))
    val current  = taskFor("kino-apollo", cache, "Dune", enricher)
    val queued   = current.copy(payload = current.payload - EnrichDetailsTasks.RowKey)
    new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, new InMemoryFreshnessStore,
      new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow, clock = specClock, enrichmentLanguage = polish).handle(queued) shouldBe Done
    cache.get(cache.keyOf("Dune", None)).flatMap(_.cinemaData.get(KinoApollo)).map(_.director) shouldBe Some(Seq("Denis Villeneuve"))
    withClue("the stored key must not split the dedup key: an old and a new task for one film are one task: ")(
      queued.dedupKey shouldBe current.dedupKey)
  }

  // venue_pages is the ONE place a page's facts are written: the handler reads the page through it,
  // so the identity model can read the page before any film row holds it.
  it should "write the page it read to venue_pages, read or gone" in {
    val read     = new InMemoryVenuePageStore
    val detail   = FilmDetail(director = Seq("Denis Villeneuve"), runtimeMinutes = Some(155), releaseYear = Some(2021))
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(detail))
    val dune     = seededCache("Dune")
    new EnrichDetailsHandler(Map("kino-apollo" -> enricher), dune, new InMemoryFreshnessStore, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow,
      clock = specClock, enrichmentLanguage = polish, pages = read).handle(taskFor("kino-apollo", dune, "Dune", enricher)) shouldBe Done
    read.get(VenuePageKey("kino-apollo", "http://ref")) shouldBe
      Some(VenuePage(VenuePageKey("kino-apollo", "http://ref"), VenuePage.Read(detail), specClock.instant()))

    val goneStore = new InMemoryVenuePageStore
    val gone      = new FakeDetailEnricher(KinoApollo, "kino-apollo", failure = Some(new HttpStatusException(404, "GET", "http://ref", None)))
    val lalka     = seededCache("Lalka")
    new EnrichDetailsHandler(Map("kino-apollo" -> gone), lalka, new InMemoryFreshnessStore, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow,
      clock = specClock, enrichmentLanguage = polish, pages = goneStore).handle(taskFor("kino-apollo", lalka, "Lalka", gone)) shouldBe Done
    goneStore.get(VenuePageKey("kino-apollo", "http://ref")).map(_.outcome) shouldBe Some(VenuePage.Gone(404))
  }

  it should "write nothing to venue_pages for a fetch that failed for now" in {
    val store    = new InMemoryVenuePageStore
    val failing  = new FakeDetailEnricher(KinoApollo, "kino-apollo", None)
    val cache    = seededCache("Dune")
    new EnrichDetailsHandler(Map("kino-apollo" -> failing), cache, new InMemoryFreshnessStore, new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), noBus, dueWindow,
      clock = specClock, enrichmentLanguage = polish, pages = store).handle(taskFor("kino-apollo", cache, "Dune", failing)) shouldBe Done
    store.get(VenuePageKey("kino-apollo", "http://ref")) shouldBe None
  }
}
