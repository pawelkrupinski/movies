package services.movies

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import controllers.MovieControllerService
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaCityClient, HeliosClient, MultikinoClient}
import services.events.{InProcessEventBus, MovieDetailsComplete}
import tools.RoutingHttpFetch

/**
 * Regression: "Diabeł ubiera się u Prady 2" disappears from the main page
 * when the date filter is set to "all days". The film is screened by
 * Multikino, both CinemaCity sites (Poznań Plaza and Kinepolis), and
 * Helios — yet under some scrape orderings the cache ends up without a
 * visible row for it.
 *
 * Replays the actual fixture payloads through the live cinema parsers
 * (`MultikinoClient`, `CinemaCityClient`, `HeliosClient`), runs them
 * through the production enrichment pipeline (recordCinemaScrape +
 * `MovieService.onMovieDetailsComplete` on the bus), and asserts the
 * post-pipeline cache holds exactly ONE row for the film — with
 * `cinemaShowings` spanning every cinema that screened it.
 */
class DiabelPradaDisappearanceSpec extends AnyFlatSpec with Matchers {

  // TMDB returns "Diabeł ubiera się u Prady 2" (id 928344, imdb tt12340108)
  // for any title search. The film's real TMDB id is unimportant for this
  // test — we just need the stub to resolve every variant to the same id
  // so identity collapse can fire.
  private val PradaSearch =
    """{"results":[{"id":928344,"title":"Diabeł ubiera się u Prady 2","original_title":"The Devil Wears Prada 2",""" +
    """"release_date":"2026-05-01","popularity":120.0}]}"""
  private val PradaExternalIds = """{"id":928344,"imdb_id":"tt12340108"}"""
  // `/movie/928344/credits` — `resolveTmdb` now verifies a title hit against the
  // row's reported director (Multikino reports "David Frankel" for this film),
  // keeping resolution order-independent. The credit must be present for the
  // verification to pass, exactly as the recorded fixture corpus carries it.
  private val PradaCredits = """{"crew":[{"job":"Director","name":"David Frankel"}]}"""

  private def tmdbStub() = new TmdbClient(
    http = new RoutingHttpFetch(Map(
      "/search/movie" -> PradaSearch,
      "/external_ids" -> PradaExternalIds,
      "/credits"      -> PradaCredits
    ), getOnly = true),
    apiKey = Some("stub")
  )

  // ── Fixture extracts ──────────────────────────────────────────────────────

  private val PradaTitle = "Diabeł ubiera się u Prady 2"

  private val multikinoPrada =
    new MultikinoClient(new FakeHttpFetch("multikino")).fetch()
      .find(_.movie.title == PradaTitle).get
  private val ccPlazaPrada =
    new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza"))
      .fetch("1078", CinemaCityPoznanPlaza)
      .find(_.movie.title == PradaTitle).get
  private val ccKinepolisPrada =
    new CinemaCityClient(new FakeHttpFetch("cinema-city-kinepolis"))
      .fetch("1081", CinemaCityKinepolis)
      .find(_.movie.title == PradaTitle).get
  private val heliosPrada =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment")).fetch()
      .find(_.movie.title == PradaTitle).get

  "cinema clients" should "all fetch Diabeł ubiera się u Prady 2 from their fixtures (with showtimes)" in {
    multikinoPrada.movie.title         shouldBe PradaTitle
    multikinoPrada.movie.releaseYear   shouldBe None         // parser drops Polish theatrical date
    multikinoPrada.showtimes           should not be empty

    ccPlazaPrada.movie.title           shouldBe PradaTitle
    ccPlazaPrada.movie.releaseYear     shouldBe Some(2026)   // CC's API uses String "2026" — parser now handles it
    ccPlazaPrada.showtimes             should not be empty

    ccKinepolisPrada.movie.title       shouldBe PradaTitle
    ccKinepolisPrada.movie.releaseYear shouldBe Some(2026)
    ccKinepolisPrada.showtimes         should not be empty

    heliosPrada.movie.title            shouldBe PradaTitle
    heliosPrada.movie.releaseYear      shouldBe Some(2026)   // yearOfProduction
    heliosPrada.showtimes              should not be empty
  }

  // ── Parametrized scrape-order regression ──────────────────────────────────
  //
  // Same invariant as the MK II spec: after the production pipeline runs
  // (recordCinemaScrape + MovieDetailsComplete on the bus), exactly one row in the
  // cache must hold the cinema slots — anything more is a duplicate card
  // on screen, anything less makes the film vanish from the home-page
  // list. Every permutation of the four scrape orders gets its own case
  // so a future regression that only fires for one specific order can't
  // hide behind another order's coincidental success.

  private case class Scrape(cinema: Cinema, title: String, year: Option[Int], cm: CinemaMovie)
  private def scrapes = Seq(
    Scrape(Multikino,             PradaTitle, None,       multikinoPrada),
    Scrape(CinemaCityPoznanPlaza, PradaTitle, Some(2026), ccPlazaPrada),
    Scrape(CinemaCityKinepolis,   PradaTitle, Some(2026), ccKinepolisPrada),
    Scrape(Helios,                PradaTitle, Some(2026), heliosPrada)
  )

  for (ordering <- scrapes.permutations.toList) {
    val label = ordering.map(_.cinema.getClass.getSimpleName.stripSuffix("$")).mkString(" → ")
    s"scrape order $label" should "leave exactly one visible Diabeł u Prady 2 row carrying every cinema's showtimes" in {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
      val bus   = new InProcessEventBus
      val svc   = new MovieService(cache, bus, tmdbStub())

      // First scrape resolves the row synchronously so subsequent
      // cinemas' MovieDetailsComplete events find a sibling with a tmdbId.
      val first = ordering.head
      cache.recordCinemaScrape(first.cinema, Seq(first.cm))
      svc.reEnrichSync(first.title, first.year)

      bus.subscribe(svc.onMovieDetailsComplete)
      for (s <- ordering.tail) {
        cache.recordCinemaScrape(s.cinema, Seq(s.cm))
        bus.publish(MovieDetailsComplete(s.title, s.year, s.cm.movie.originalTitle, if (s.cm.director.nonEmpty) Some(s.cm.director.mkString(", ")) else None))
      }

      def isPrada(e: MovieRecord): Boolean =
        e.tmdbId.contains(928344) || e.imdbId.contains("tt12340108")

      val visibleRows = cache.snapshot().filter { case StoredMovieRecord(_, _, e) =>
        isPrada(e) && e.cinemaData.nonEmpty
      }
      visibleRows.size shouldBe 1

      val visible = visibleRows.head.record
      visible.cinemaData.keySet shouldBe
        Set(Multikino, CinemaCityPoznanPlaza, CinemaCityKinepolis, Helios)
      // Each cinema's showtimes survive the pipeline (the disappearance
      // mode of the original bug was empty cinemaShowings on the visible
      // row even when the cinema slots were present).
      visible.cinemaData.values.foreach(_.showtimes should not be empty)

      // ── Controller-level reproduction ────────────────────────────────
      // The cache says the row is there with full slots, but the user's
      // symptom is "appears on /debug but NOT on the main /". /debug reads
      // the raw snapshot; / runs the snapshot through
      // `MovieController.toSchedules`, which filters by future showtimes
      // relative to `now`. Pin `now` to the day BEFORE the earliest
      // fixture showtime so every showtime is in the future — if the film
      // still vanishes from the schedule output, the bug is in the
      // read path itself, not in the data.
      // Project the cache into the read model and serve from it, as the web does.
      val readModel = services.readmodel.TestReadModel.fromRecords(
        cache.snapshot().map(r => (r.title, r.year, r.record)))
      val ctrl = new MovieControllerService(readModel)
      val firstShowtime: java.time.LocalDateTime =
        cache.snapshot().filter(r => isPrada(r.record))
          .flatMap(_.record.cinemaData.values.flatMap(_.showtimes.map(_.dateTime)))
          .min
      val pinnedNow = firstShowtime.minusDays(1)

      // The read model drops tmdbId/imdbId, so identify Prada via the cache and
      // match the rendered rows by film id.
      val pradaFilmIds = cache.snapshot().filter(r => isPrada(r.record))
        .map(services.readmodel.ReadModelProjection.filmId).toSet
      val rendered = ctrl.toSchedules(Poznan, pinnedNow)
      val pradaSchedules = rendered.filter(s => pradaFilmIds.contains(s.resolved._id))

      pradaSchedules.size shouldBe 1
      val schedule = pradaSchedules.head
      schedule.showings should not be empty
      // The schedule's per-date listing carries slots from every cinema
      // that fed it through the pipeline.
      val cinemasInSchedule = schedule.showings.flatMap(_._2.map(_.cinema)).toSet
      cinemasInSchedule shouldBe Set(Multikino, CinemaCityPoznanPlaza, CinemaCityKinepolis, Helios)
    }
  }

  // Regression for the user's "Diabeł in debug but not on main screen"
  // observation: a legacy production row that pre-dated the cross-script
  // merge block had Latin + Cyrillic variants both in its cinemaTitles.
  // `preferredDisplay`'s old uppercase-count tiebreaker picked the all-caps
  // Cyrillic spelling as the displayed title, so the card on / read
  // "ДИЯВОЛ НОСИТЬ ПРАДА 2" while /debug still showed the row's Polish
  // cleanTitle — making the Polish row look "missing" from the main page.
  "displayTitle" should "prefer the Latin / mixed-case variant over an all-caps Cyrillic one" in {
    val e = MovieRecord(
      imdbId = Some("tt33612209"),
      tmdbId = Some(1314481),
      data = Map[Source, SourceData](
        Multikino             -> SourceData(title = Some("Diabeł ubiera się u Prady 2"), releaseYear = Some(2026)),
        CinemaCityPoznanPlaza -> SourceData(title = Some("Diabeł ubiera się u prady 2"), releaseYear = Some(2026)),
        Helios                -> SourceData(title = Some("ДИЯВОЛ НОСИТЬ ПРАДА 2"),       releaseYear = Some(2026))
      )
    )
    e.displayTitle("Diabeł ubiera się u Prady 2") shouldBe "Diabeł ubiera się u Prady 2"
  }

  it should "still pick the proper-cased variant when only Latin forms compete" in {
    val e = MovieRecord(
      data = Map[Source, SourceData](
        Multikino             -> SourceData(title = Some("Top Gun: Maverick"), releaseYear = Some(2022)),
        CinemaCityPoznanPlaza -> SourceData(title = Some("TOP GUN: MAVERICK"), releaseYear = Some(2022)),
        Helios                -> SourceData(title = Some("top gun: maverick"), releaseYear = Some(2022))
      )
    )
    e.displayTitle("Top Gun: Maverick") shouldBe "Top Gun: Maverick"
  }

  // ── Regression: prevent the duplicate from being created in the first place ──
  //
  // Production was logging an `IdentityMerger.mergeAll` collapse for the same
  // ~17 (title, year) pairs on every restart — `Bez wyjścia`, `Drzewo magii`,
  // `Wartość sentymentalna`, `Diabeł ubiera się u Prady 2`, …, all where two
  // cinemas reported the same film with different years (one with year=None,
  // one with year=Some(YYYY)). recordCinemaScrape's redirect already attached
  // both cinemas' slots to a single row, but the scrape published
  // a `MovieDetailsComplete` event for the RAW (title, year) reported
  // by each cinema. The TMDB stage ran independently for each raw key, and
  // when the in-flight TMDB call for the first row hadn't completed yet,
  // `hasResolvedSiblingByTitle` returned false and the second TMDB call
  // proceeded to write its own row at (title, year-of-the-second-cinema).
  // mergeAll then collapsed the pair on the next startup, and the cycle
  // repeated next deploy.
  //
  // Fix: recordCinemaScrape returns the *canonical* CacheKey it actually
  // wrote each slot to (the redirect target when one applies, the raw key
  // otherwise). CinemaScrapeRunner publishes MovieDetailsComplete using those
  // canonical keys, so both cinemas' bus events name the same key and the
  // TMDB stage runs exactly once. No phantom row, no startup merge.

  "recordCinemaScrape" should "collapse Multikino + Helios onto one canonical key (year-bearing wins) when they report Diabeł Prada with different years" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)

    // Multikino lands first with year=None — creates the row at its own key.
    val mkTouched = cache.recordCinemaScrape(Multikino, Seq(multikinoPrada))
    mkTouched                 should have size 1
    mkTouched.head._1         shouldBe multikinoPrada
    mkTouched.head._2         shouldBe cache.keyOf(PradaTitle, None)

    // Helios reports year=Some(2026). The redirect absorbs Helios's slot into
    // the existing row, then `canonicalRank` promotes the WHOLE row onto the
    // year-bearing key — so the stored identity is a pure function of the
    // reported variants, not arrival order. The slot lands on (PradaTitle, 2026).
    val helTouched = cache.recordCinemaScrape(Helios, Seq(heliosPrada))
    helTouched                should have size 1
    helTouched.head._1        shouldBe heliosPrada
    helTouched.head._2        shouldBe cache.keyOf(PradaTitle, Some(2026))

    // Still exactly one row — no phantom at the yearless key it was promoted off.
    cache.snapshot().size     shouldBe 1
    cache.get(cache.keyOf(PradaTitle, None)) shouldBe None
    cache.get(cache.keyOf(PradaTitle, Some(2026))) should not be None
  }

  // End-to-end via the bus-driven pipeline: confirms that publishing events
  // with the canonical keys (returned by recordCinemaScrape) prevents the
  // TMDB stage from creating a second row for the year=Some(2026) variant.
  // svc.stop() drains the worker pool so the assertion is deterministic.
  "bus-driven scrape pipeline" should "produce exactly one TMDB-resolved row when two cinemas report Diabeł Prada with different years" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
    val bus   = new InProcessEventBus
    val svc   = new MovieService(cache, bus, tmdbStub())
    bus.subscribe(svc.onMovieDetailsComplete)

    // Drive the same flow `cinemaScrapeRunner.run` does: publish a
    // MovieDetailsComplete for each canonical key returned by recordCinemaScrape.
    // The event's director hint is omitted here; `resolveTmdb` still verifies the
    // title hit against the director the row already carries (via `/credits`),
    // which the stub now exposes — the duplicate-prevention contract under test
    // is unaffected either way.
    def scrape(cinema: Cinema, cm: CinemaMovie): Unit = {
      val touched = cache.recordCinemaScrape(cinema, Seq(cm))
      touched.foreach { case (m, key, isNew) =>
        if (isNew)
          bus.publish(MovieDetailsComplete(key.cleanTitle, key.year, m.movie.originalTitle, None))
      }
    }

    scrape(Multikino, multikinoPrada)
    scrape(Helios,    heliosPrada)

    // Drain the TMDB worker so any in-flight resolutions land before we read.
    svc.stop()

    val pradaRows = cache.snapshot().filter { case StoredMovieRecord(_, _, e) =>
      e.tmdbId.contains(928344) || e.imdbId.contains("tt12340108")
    }
    pradaRows.size shouldBe 1
    pradaRows.head.record.cinemaData.keySet shouldBe Set(Multikino, Helios)
  }
}
