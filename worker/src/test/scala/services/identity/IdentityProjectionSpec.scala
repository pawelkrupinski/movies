package services.identity

import tools.SpecTimeouts

import models.{Cinema, CinemaMovie, CinemaShowing, Helios, KinoApollo, KinoMuza, Movie, Multikino, Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, InMemoryMovieRepository, SingleCountryNormalizer}
import services.scrapes.InMemoryScrapeArchiveRepository

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * The identity projection wired over the real cache and an in-memory store: what a cut-over
 * country's worker writes, and that a second projection over the same listings writes nothing (P2).
 * The lookups know no film, so every cluster is concluded unmatched — the identity decisions
 * themselves are the resolver's and the plan's specs'.
 */
class IdentityProjectionSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def film(cinema: Cinema, title: String, year: Option[Int], hours: Int*): CinemaMovie =
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, None, None, Nil, Nil, hours.map(h => Showtime(start.plusHours(h.toLong), None)))

  private val programme: Map[Cinema, Seq[CinemaMovie]] = Map(
    Multikino  -> Seq(film(Multikino, "Lalka", Some(2026), 0, 3), film(Multikino, "Obcy", Some(1979), 1)),
    Helios     -> Seq(film(Helios, "Lalka", Some(2026), 2), film(Helios, "Diuna", Some(2021), 5)),
    KinoApollo -> Seq(film(KinoApollo, "Lalka", Some(2026), 4)),
    Rialto     -> Seq(film(Rialto, "Obcy", Some(1979), 6)),
    KinoMuza   -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8)))

  private def World(repository: InMemoryMovieRepository = new InMemoryMovieRepository(normalizer = normalizer),
                    venues: Seq[Cinema] = programme.keys.toSeq, listingsRead: () => Unit = () => (),
                    announceFails: Boolean = false, whole: Boolean = false,
                    archive: InMemoryScrapeArchiveRepository = new InMemoryScrapeArchiveRepository,
                    accepted: InMemoryScrapeArchiveRepository = new InMemoryScrapeArchiveRepository,
                    filmIds: InMemoryFilmIdCounterStore = new InMemoryFilmIdCounterStore,
                    fingerprints: VenueSlotFingerprints = new InMemoryVenueSlotFingerprints,
                    resolveOverride: Option[Seq[Listing] => Option[IdentityProjection.Resolved]] = None): ProjectionWorld =
    new ProjectionWorld(repository, venues, clock, resolve = resolveOverride.fold(ProjectionWorld.unmatched)(resolve => read => resolve(read())),
      listingsRead = listingsRead, announceFails = announceFails, archive = archive, accepted = accepted, filmIds = filmIds,
      fingerprints = fingerprints, scopedBetweenWhole = if (whole) 0 else IdentityProjection.ScopedBetweenWhole)

  private val allShowtimes: Set[(String, LocalDateTime)] =
    programme.values.flatten.flatMap(cm => cm.showtimes.map(s => cm.cinema.displayName -> s.dateTime)).toSet

  "A cut-over country's first projection" should "store one film per title, every showtime on it, and announce each" in {
    val w = World()
    w.scrape(programme)
    val tick = w.projection.tick()
    tick.refused shouldBe None
    w.repository.findAll().map(_.title).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.showtimes shouldBe allShowtimes
    w.repository.findAll().forall(_.record.readyToProject) shouldBe true
    w.cache.snapshot().map(_.id).toSet shouldBe w.repository.findAll().map(_.id).toSet
    w.announced.map(_.cleanTitle).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.filmIds.allChecked().required.map(_.filmId).toSet shouldBe w.repository.findAll().map(_.id.value).toSet
  }

  "A projection's failures" should "be logged WITH their stacks, the announce naming its film, and a failed projection counted" in {
    val name   = classOf[IdentityProjection].getName
    val warned = (body: ProjectionWorld => Unit) => {
      val w = World(announceFails = true)
      w.scrape(programme)
      tools.LogCapture.thisThread(name)(body(w)).filter(_.getLevel == ch.qos.logback.classic.Level.WARN)
    }
    val announces = warned(_.projection.tick()).filter(_.getFormattedMessage.contains("announcing"))
    announces should have size 3
    announces.map(e => Option(e.getThrowableProxy).map(_.getMessage)).flatten.sorted shouldBe
      Seq("bus down for Diuna", "bus down for Lalka", "bus down for Obcy")

    val down   = World(listingsRead = () => throw new IllegalStateException("archive down"))
    val failed = tools.LogCapture.thisThread(name)(down.projection.tickQuietly()).filter(_.getLevel == ch.qos.logback.classic.Level.WARN)
    failed.map(e => Option(e.getThrowableProxy).map(_.getMessage)) shouldBe Seq(Some("archive down"))
    down.refusals.toSeq shouldBe Seq(IdentityProjectionMetrics.Refusal.Failed) // and counted: the stored films keep serving
  }

  // The two refusals a resolve itself can call for: a model not ready yet (a boot before its first build) and a
  // constraint edge crossing families. Either leaves every stored film serving, writes nothing and is counted.
  it should "refuse, writing nothing, while the identity model is not ready or a constraint crosses families" in {
    val first = World()
    first.scrape(programme)
    first.projection.tick().refused shouldBe None
    val stored = first.repository.findAll().map(r => r.id -> r.record).toMap

    val notReady = World(first.repository, archive = first.archive, accepted = first.accepted, filmIds = first.filmIds,
      resolveOverride = Some(_ => None))
    notReady.scrape(programme - KinoMuza)
    val waiting = notReady.projection.tick()
    waiting.refused shouldBe Some("the identity model is not ready")
    waiting.written shouldBe 0
    notReady.refusals.toSeq shouldBe Seq(IdentityProjectionMetrics.Refusal.NotReady)

    val crossing = World(first.repository, archive = first.archive, accepted = first.accepted, filmIds = first.filmIds,
      resolveOverride = Some(_ => throw new IdentityResolver.FamilyCrossing(2, "2 constraint edges cross a family")))
    crossing.scrape(programme - KinoMuza)
    crossing.projection.tick().refused shouldBe Some("2 constraint edges cross a family")
    crossing.refusals.toSeq shouldBe Seq(IdentityProjectionMetrics.Refusal.Crossing)

    first.repository.findAll().map(r => r.id -> r.record).toMap shouldBe stored // the stored films keep serving, untouched
  }

  // The venue slot fingerprints are a saving: a store that cannot take them must not fail the projection, and the
  // fingerprints it missed are written by the next one.
  it should "project through a fingerprint store that fails, and record the fingerprints once it answers again" in {
    var down = true
    val inner = new InMemoryVenueSlotFingerprints
    val flaky = new VenueSlotFingerprints {
      def all(): Set[Long] = inner.all()
      def update(add: Set[Long], remove: Set[Long]): Unit =
        if (down) throw new IllegalStateException("fingerprints down") else inner.update(add, remove)
    }
    val w = World(fingerprints = flaky)
    w.scrape(programme)
    w.projection.tick().refused shouldBe None
    w.repository.findAll().map(_.title).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    inner.all() shouldBe empty

    down = false
    w.projection.tick().refused shouldBe None
    inner.all() should not be empty
  }

  // One at a time, a US boot's first projection waited on ~2,250 films' round-trips in a row.
  it should "write films nothing else holds or takes side by side" in {
    val inFlight = new java.util.concurrent.atomic.AtomicInteger
    val most     = new java.util.concurrent.atomic.AtomicInteger
    val slow = new InMemoryMovieRepository(normalizer = normalizer) {
      override def upsert(film: services.movies.FilmId, key: CacheKey, e: models.MovieRecord): services.movies.WriteOutcome = {
        most.accumulateAndGet(inFlight.incrementAndGet(), math.max)
        try { Thread.sleep(50); super.upsert(film, key, e) } finally { inFlight.decrementAndGet(); () }
      }
    }
    val w = World(slow)
    w.scrape(programme)
    w.projection.tick().written shouldBe 3
    w.repository.findAll().map(_.title).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.showtimes shouldBe allShowtimes
    most.get should be > 1
  }

  "The films a projection writes side by side" should "be only those no other film holds or takes" in {
    val w = World()
    w.scrape(programme)
    val first   = w.projection.tick().plan.get
    val stored  = w.repository.findAll()
    val renamed = first.films.map(f => if (f.title == "Obcy") f.copy(id = services.movies.FilmId("new-obcy")) else f)
    // Every film already stored: none is new. A fresh id under a key a stored film holds is not free either.
    IdentityProjection.independent(first.films, first.films, stored, normalizer) shouldBe empty
    IdentityProjection.independent(renamed, renamed, stored, normalizer) shouldBe empty
    // New to an empty store, each under its own key: all of them — unless two of them take one key.
    IdentityProjection.independent(first.films, first.films, Nil, normalizer) shouldBe first.films.map(_.id).toSet
    val twice = first.films :+ first.films.head.copy(id = services.movies.FilmId("twin"))
    IdentityProjection.independent(twice, twice, Nil, normalizer) shouldBe first.films.tail.map(_.id).toSet
  }

  "A projection" should "say how long each of its phases took and what it allocated" in {
    val w = World()
    w.scrape(programme)
    val phases = w.projection.tick().phases
    phases.map(_.name) shouldBe Seq("listings", "snapshot", "resolve", "agreement", "seed", "index", "draft", "details", "finish", "fingerprints", "guard",
      "compare", "writes")
    phases.foreach(p => (p.seconds >= 0 && p.allocatedBytes >= 0) shouldBe true)
    phases.map(_.allocatedBytes).sum should be > 0L
  }

  // The first projection builds every slot over no stored film, and from then on only what moves is rebuilt. The
  // slots it wrote are the next projection's prior slots, which a slot carries detail forward from: built over
  // them, each comes out as it was written, so they are no reason to build it again — a US tick rebuilt 300–500
  // slots a second time as "priors moved" for the ~200 whose rows had. These are projections of the whole corpus — a
  // boot's, an hourly reconciliation's: every film drafted, its slots memoised.
  "A projection of the whole corpus over unchanged listings" should "build no venue slot again, reusing the last projection's" in {
    val w = World(whole = true)
    w.scrape(programme)
    val first = w.projection.tick()
    first.slotsReused shouldBe 0
    first.slotsBuilt should be > 0
    val again = w.projection.tick()
    again.slotsBuilt shouldBe 0
    again.slotsReused shouldBe first.slotsBuilt
    again.wroteNothing shouldBe true
  }

  it should "rebuild only the venue whose listing changed, and write that film with every showtime" in {
    val w = World(whole = true)
    w.scrape(programme)
    val first = w.projection.tick()
    // Another showtime, and a synopsis — a field the slot carries forward from its prior slot.
    val later = film(KinoMuza, "Diuna", Some(2021), 7, 8, 9).copy(synopsis = Some("Paul Atryda przybywa na Arrakis."))
    w.scrape(Map(KinoMuza -> Seq(later)))
    val tick = w.projection.tick()
    tick.slotsBuilt shouldBe 1
    tick.slotsReused shouldBe first.slotsBuilt - 1
    tick.slotMisses shouldBe ((1, 0, 0))   // the one venue's rows moved; no prior slot, no new listing
    tick.written shouldBe 1
    w.showtimes shouldBe allShowtimes + (KinoMuza.displayName -> start.plusHours(9))
    // What it wrote is now that venue's prior slot, synopsis and all: what the slot was built INTO, not a reason to
    // build it again.
    val next = w.projection.tick()
    next.slotsBuilt shouldBe 0
    next.wroteNothing shouldBe true
  }

  // A worker restarts on every deploy, and its memo with it: the first projection after a boot rebuilt every venue
  // slot of the country (a US boot: ~24 s, ~7.3 GB, against ~6 s, ~760 MB a steady tick).
  it should "build no venue slot again after a restart, when nothing moved while the worker was down" in {
    val w = World(whole = true)
    w.scrape(programme)
    val first = w.projection.tick()
    w.projection.tick().slotsBuilt shouldBe 0
    val booted = w.restarted
    val tick   = booted.projection.tick()
    tick.slotsBuilt shouldBe 0
    tick.slotsReused shouldBe first.slotsBuilt
    tick.wroteNothing shouldBe true
    booted.rowsRead shouldBe empty
    booted.showtimes shouldBe allShowtimes
    booted.projection.tick().slotsBuilt shouldBe 0
  }

  it should "rebuild after a restart only the venue whose listing changed while the worker was down" in {
    val w = World(whole = true)
    w.scrape(programme)
    val first = w.projection.tick()
    w.projection.tick()
    val booted = w.restarted
    booted.scrape(Map(KinoMuza -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8, 9))))
    val tick = booted.projection.tick()
    tick.slotsBuilt shouldBe 1
    tick.slotsReused shouldBe first.slotsBuilt - 1
    tick.written shouldBe 1
    booted.showtimes shouldBe allShowtimes + (KinoMuza.displayName -> start.plusHours(9))
  }

  it should "rebuild after a restart a venue whose stored slot moved while the worker was down, though its listing did not" in {
    val w = World(whole = true)
    w.scrape(programme)
    w.projection.tick(); w.projection.tick(); w.projection.tick()
    // Something else wrote the stored film's slot at one venue: what it holds is no longer what the listing builds.
    val obcy = w.repository.findAll().find(_.title == "Obcy").get
    w.repository.upsert(obcy.id, CacheKey.stored(obcy.title, obcy.key(normalizer)), obcy.record.copy(data = obcy.record.data.map {
      case (source @ CinemaShowing(Rialto, _), slot) => source -> slot.copy(filmUrl = Some("https://example.org/obcy"))
      case other => other
    }))
    val booted = w.restarted
    val tick   = booted.projection.tick()
    tick.slotsBuilt shouldBe 1
    tick.written shouldBe 1
  }

  it should "write in full a film whose stored showtimes went astray, though its slots came from the memo" in {
    val w = World(whole = true)
    w.scrape(programme)
    w.projection.tick(); w.projection.tick()
    // The stored film loses its showtimes behind the projection's back; nothing it is built from moves.
    val obcy = w.repository.findAll().find(_.title == "Obcy").get
    w.repository.upsert(obcy.id, CacheKey.stored(obcy.title, obcy.key(normalizer)),
      obcy.record.copy(data = obcy.record.data.map { case (source, slot) => source -> slot.copy(showtimes = Nil) }))
    w.cache.rehydrate()
    w.showtimes should not be allShowtimes
    val tick = w.projection.tick()
    tick.slotsBuilt shouldBe 0
    tick.written shouldBe 1
    w.showtimes shouldBe allShowtimes
  }

  // One film at many venues: each venue's key read the film's prior slots, so a film showing at N venues was
  // scanned N times a tick — quadratic in the venues, and a US release shows at hundreds.
  it should "read a widely shown film's prior slots once a tick, not once per venue" in {
    val venues = Cinema.all.distinct.take(400)
    val w      = World(venues = venues)
    w.scrape(venues.map(c => c -> Seq(film(c, "Diuna", Some(2021), 1, 2, 3))).toMap)
    w.projection.tick(); w.projection.tick()
    val tick  = w.projection.tick()
    val draft = tick.phases.find(_.name == "draft").get
    info(f"draft over one film at ${venues.size} venues: ${draft.seconds}%.2fs, ${draft.allocatedBytes / 1e6}%.1f MB")
    tick.slotsBuilt shouldBe 0
    draft.allocatedBytes should be < 4_000_000L   // 7.2 MB scanning the film per venue, 2.2 MB once
  }

  it should "rebuild, writing a widely shown film that changed at one venue, only that venue — and serve every showtime" in {
    val venues = Cinema.all.distinct.take(400)
    // As production stores a film: its showtimes in `screenings`, its slots in `movie_slots`, the cache's stripped.
    val split  = new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = normalizer)
    val w      = World(split, venues = venues)
    w.scrape(venues.map(c => c -> Seq(film(c, "Diuna", Some(2021), 1, 2, 3))).toMap)
    w.projection.tick(); w.projection.tick()
    w.scrape(Map(venues.head -> Seq(film(venues.head, "Diuna", Some(2021), 1, 2, 3, 4))))
    val tick    = w.projection.tick()
    val compare = tick.phases.find(_.name == "compare").get
    info(f"compare, one of ${venues.size} venues changed: ${compare.seconds}%.2fs, ${compare.allocatedBytes / 1e6}%.1f MB")
    compare.allocatedBytes should be < 1_000_000L   // 1.9 MB rebuilding all 400 venues, 0.1 MB the one
    tick.written shouldBe 1
    w.showtimes shouldBe venues.flatMap(c => Seq(1, 2, 3).map(h => c.displayName -> start.plusHours(h.toLong))).toSet +
      (venues.head.displayName -> start.plusHours(4))
  }

  // A US projection held every listing's showtimes (1.7M, ~560 MB live with their rows) and again in every film's
  // built slots, through the whole tick: 18 back-to-back full GCs a projection, on a heap of 853 MB.
  "A scoped projection" should "draft no film when nothing moved since the last one wrote" in {
    val w = World()
    w.scrape(programme)
    w.projection.tick().scoped shouldBe false
    val again = w.projection.tick()
    again.scoped shouldBe true
    (again.slotsBuilt, again.slotsReused) shouldBe ((0, 0))
    again.wroteNothing shouldBe true
  }

  it should "draft only the film whose listing changed, rebuilding only the venue that moved, and write it with every showtime" in {
    val w = World()
    w.scrape(programme)
    w.projection.tick()
    // Diuna drafted once over what the first projection stored (the first was over no stored film, so its venues'
    // prior slots all moved with that projection's writes).
    w.scrape(Map(Helios -> Seq(film(Helios, "Lalka", Some(2026), 2), film(Helios, "Diuna", Some(2021), 5, 6))))
    w.projection.tick().written shouldBe 1
    w.scrape(Map(KinoMuza -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8, 9))))
    val tick = w.projection.tick()
    tick.scoped shouldBe true
    (tick.slotsBuilt, tick.slotsReused) shouldBe ((1, 0))   // Kino Muza built; Helios kept as last drafted, not even looked up
    tick.written shouldBe 1
    w.showtimes shouldBe allShowtimes + (Helios.displayName -> start.plusHours(6)) + (KinoMuza.displayName -> start.plusHours(9))
  }

  "A projection" should "hold no listing's showtimes, nor any in the films it plans, and read rows only for the venues it builds" in {
    // As production stores a film: its showtimes in `screenings`, its slots in `movie_slots`, the cache's stripped.
    val w = World(new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = normalizer), whole = true)
    w.scrape(programme)
    val first = w.projection.tick()
    first.plan.get.films.flatMap(_.record.data.values).flatMap(_.showtimes) shouldBe empty
    w.showtimes shouldBe allShowtimes
    w.rowsRead.toSet shouldBe programme.keySet
    w.projection.tick()
    w.rowsRead.clear()
    w.projection.tick().wroteNothing shouldBe true
    w.rowsRead shouldBe empty
    w.scrape(Map(KinoMuza -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8, 9))))
    w.projection.tick().written shouldBe 1
    w.rowsRead.toSet shouldBe Set(KinoMuza)
  }

  "A second projection over the same listings" should "write nothing (P2)" in {
    val w = World()
    w.scrape(programme)
    w.projection.tick()
    val before = w.repository.findAll().map(r => (r.id, r.key(normalizer), r.record)).toSet
    val again  = w.projection.tick()
    again.wroteNothing shouldBe true
    again.plan.get.regroupings.isEmpty shouldBe true
    w.repository.findAll().map(r => (r.id, r.key(normalizer), r.record)).toSet shouldBe before
  }

  "A projection that would take a film off the site" should "be refused for the guard's grace, then written" in {
    val w = World()
    w.scrape(programme)
    w.projection.tick()
    // Rialto and Multikino stop listing "Obcy": the film vanishes — a third of the site.
    w.scrape(Map(Rialto -> Seq(film(Rialto, "Diuna", Some(2021), 9)), Multikino -> programme(Multikino).take(1)))
    (1 to ProjectionGuard.Grace).foreach(_ => w.projection.tick().refused should not be empty)
    w.repository.findAll().map(_.title) should contain("Obcy")
    w.projection.tick().refused shouldBe None
    w.repository.findAll().map(_.title) should not contain "Obcy"
    w.cache.snapshot().map(_.title) should not contain "Obcy"
  }

  it should "be written at once when the films leaving have no showtime still to come, as they are on no card" in {
    val w = World()
    // "Obcy" screened yesterday (start − 40 h is before the clock) and is listed nowhere now.
    w.scrape(programme.map { case (c, fs) => c -> fs.map(f => if (f.movie.title == "Obcy") film(c, "Obcy", Some(1979), -40) else f) })
    w.projection.tick()
    w.scrape(Map(Rialto -> Seq(film(Rialto, "Diuna", Some(2021), 9)), Multikino -> programme(Multikino).take(1)))
    w.projection.tick().refused shouldBe None
    w.repository.findAll().map(_.title) should not contain "Obcy"
  }

  it should "name the films it would take off the site" in {
    val w = World()
    w.scrape(programme)
    w.projection.tick()
    w.scrape(Map(Rialto -> Seq(film(Rialto, "Diuna", Some(2021), 9)), Multikino -> programme(Multikino).take(1)))
    w.projection.tick().refused.get should include("Obcy")
  }

  /** TMDB down while a film is first matched: its details (`external_ids` among them) fail, so it is written with its
   *  TMDB id and nothing TMDB says about it. Announced then, its IMDb-id recovery searched with no original title and
   *  no director, missed, and was never asked again — the details landing later changed neither its TMDB nor its IMDb
   *  id. A film is announced once its TMDB answer is whole, and only then. */
  "A film whose TMDB details failed" should "be announced once they arrive, not while it lacks them" in {
    var tmdbDown = true
    val matched: (() => Seq[Listing]) => Option[IdentityProjection.Resolved] = read => ProjectionWorld.unmatched(read).map { r =>
      val decisions = r.resolution.decisions.zipWithIndex.map { case (d, i) =>
        ResolverDecision(d.members, Some(500 + i), 0.9, ResolverDecision.Basis.OwnMatch, Nil)() }
      r.copy(resolution = r.resolution.copy(decisions = decisions))
    }
    val w = new ProjectionWorld(new InMemoryMovieRepository(normalizer = normalizer), programme.keys.toSeq, clock, matched,
      details = (record, _) => Option.unless(tmdbDown)(record.copy(
        data = record.data + (models.Tmdb -> models.SourceData(title = Some("Original"))))))
    w.scrape(programme)
    val down = w.projection.tick()
    w.repository.findAll().flatMap(_.record.tmdbId) should have size 3
    w.projection.settled(down) shouldBe false
    w.announced shouldBe empty

    tmdbDown = false
    w.projection.settled(w.projection.tick()) shouldBe true
    w.announced.map(_.cleanTitle).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.projection.tick()
    w.announced should have size 3 // announced once, not again by a projection that changes nothing
  }

  /** A country's first projection fetches the TMDB details of every film it matched (~2,250 on a US
   *  boot): one at a time, it waited on each in a row. */
  "A projection's TMDB details" should "be fetched side by side, each onto its own draft" in {
    val drafts = (1 to 4).map(i => FilmDraft(i.toLong, None, Nil, models.MovieRecord(tmdbId = Some(100 + i)), s"Film $i")) :+
      FilmDraft(5L, None, Nil, models.MovieRecord(), "Unmatched")
    // Each fetch waits for another to be under way: fetched one at a time, the first never returns.
    val together = new java.util.concurrent.CyclicBarrier(2)
    val fetched  = IdentityProjection.detailed(drafts, (record, film) => {
      if (film <= 102) together.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
      Some(record.copy(imdbId = Some(s"tt$film")))
    })
    fetched.map(_.record.imdbId) shouldBe Seq(Some("tt101"), Some("tt102"), Some("tt103"), Some("tt104"), None)
    fetched.map(_.anchor) shouldBe drafts.map(_.anchor)
  }
}
