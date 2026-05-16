package services.movies

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaCityClient, HeliosClient, MultikinoClient}
import services.events.{EventBus, MovieRecordCreated}
import tools.HttpFetch

import scala.collection.mutable

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
 * `MovieService.onMovieRecordCreated` on the bus), and asserts the
 * post-pipeline cache holds exactly ONE row for the film — with
 * `cinemaShowings` spanning every cinema that screened it.
 */
class DiabelPradaDisappearanceSpec extends AnyFlatSpec with Matchers {

  // ── Stubs ──────────────────────────────────────────────────────────────────

  private class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  // TMDB returns "Diabeł ubiera się u Prady 2" (id 928344, imdb tt12340108)
  // for any title search. The film's real TMDB id is unimportant for this
  // test — we just need the stub to resolve every variant to the same id
  // so identity collapse can fire.
  private val PradaSearch =
    """{"results":[{"id":928344,"title":"Diabeł ubiera się u Prady 2","original_title":"The Devil Wears Prada 2",""" +
    """"release_date":"2026-05-01","popularity":120.0}]}"""
  private val PradaExternalIds = """{"id":928344,"imdb_id":"tt12340108"}"""

  private def tmdbStub() = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie" -> PradaSearch,
      "/external_ids" -> PradaExternalIds
    )),
    apiKey = Some("stub")
  )

  // Mirrors `MovieRepo`'s `docId = normalize(title)|year` keying so
  // upserts under different raw titles for the same film collapse to one
  // persisted row, matching real Mongo behaviour.
  private class FakeRepo extends MovieRepo {
    private def docId(t: String, y: Option[Int]): String =
      s"${MovieService.normalize(t)}|${y.map(_.toString).getOrElse("")}"
    private val store = mutable.LinkedHashMap.empty[String, (String, Option[Int], MovieRecord)]
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], MovieRecord)] = store.values.toSeq
    override def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = {
      store.put(docId(t, y), (t, y, e)); ()
    }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove(docId(t, y)); () }
  }

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
  // (recordCinemaScrape + MovieRecordCreated on the bus), exactly one row in the
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
      val cache = new MovieCache(new FakeRepo)
      val bus   = new EventBus
      val svc   = new MovieService(cache, bus, tmdbStub())

      // First scrape resolves the row synchronously so subsequent
      // cinemas' MovieRecordCreated events find a sibling with a tmdbId.
      val first = ordering.head
      cache.recordCinemaScrape(first.cinema, Seq(first.cm))
      svc.reEnrichSync(first.title, first.year)

      bus.subscribe(svc.onMovieRecordCreated)
      for (s <- ordering.tail) {
        cache.recordCinemaScrape(s.cinema, Seq(s.cm))
        bus.publish(MovieRecordCreated(s.title, s.year, s.cm.movie.originalTitle, s.cm.director))
      }

      def isPrada(e: MovieRecord): Boolean =
        e.tmdbId.contains(928344) || e.imdbId.contains("tt12340108")

      val visibleRows = cache.snapshot().filter { case (_, _, e) =>
        isPrada(e) && e.cinemaShowings.nonEmpty
      }
      visibleRows.size shouldBe 1

      val (_, _, visible) = visibleRows.head
      visible.cinemaShowings.keySet shouldBe
        Set(Multikino, CinemaCityPoznanPlaza, CinemaCityKinepolis, Helios)
      // Each cinema's showtimes survive the pipeline (the disappearance
      // mode of the original bug was empty cinemaShowings on the visible
      // row even when the cinema slots were present).
      visible.cinemaShowings.values.foreach(_.showtimes should not be empty)

      // ── Controller-level reproduction ────────────────────────────────
      // The cache says the row is there with full slots, but the user's
      // symptom is "appears on /debug but NOT on the main /". /debug reads
      // the raw snapshot; / runs the snapshot through
      // `MovieController.toSchedules`, which filters by future showtimes
      // relative to `now`. Pin `now` to the day BEFORE the earliest
      // fixture showtime so every showtime is in the future — if the film
      // still vanishes from the schedule output, the bug is in the
      // read path itself, not in the data.
      import controllers.MovieController
      import play.api.{Environment, Mode}
      val ctrl = new MovieController(
        cc           = play.api.test.Helpers.stubControllerComponents(),
        movieService = svc,
        env          = Environment.simple(mode = Mode.Test)
      )
      val firstShowtime: java.time.LocalDateTime =
        cache.snapshot().filter { case (_, _, e) => isPrada(e) }
          .flatMap { case (_, _, e) => e.cinemaShowings.values.flatMap(_.showtimes.map(_.dateTime)) }
          .min
      val pinnedNow = firstShowtime.minusDays(1)

      val rendered = ctrl.toSchedules(pinnedNow)
      val pradaSchedules = rendered.filter(_.enrichment.exists(isPrada))

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
      imdbId        = Some("tt33612209"),
      imdbRating    = None,
      metascore     = None,
      originalTitle = None,
      tmdbId        = Some(1314481),
      cinemaScrapes = Set(
        CinemaScrape(Multikino,             "Diabeł ubiera się u Prady 2", Some(2026)),
        CinemaScrape(CinemaCityPoznanPlaza, "Diabeł ubiera się u prady 2", Some(2026)),
        CinemaScrape(Helios,                "ДИЯВОЛ НОСИТЬ ПРАДА 2",       Some(2026))
      )
    )
    e.displayTitle("Diabeł ubiera się u Prady 2") shouldBe "Diabeł ubiera się u Prady 2"
  }

  it should "still pick the proper-cased variant when only Latin forms compete" in {
    val e = MovieRecord(
      imdbId        = None,
      imdbRating    = None,
      metascore     = None,
      originalTitle = None,
      cinemaScrapes = Set(
        CinemaScrape(Multikino,             "Top Gun: Maverick", Some(2022)),
        CinemaScrape(CinemaCityPoznanPlaza, "TOP GUN: MAVERICK", Some(2022)),
        CinemaScrape(Helios,                "top gun: maverick", Some(2022))
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
  // both cinemas' slots to a single row, but `ShowtimeCache.refreshOne`
  // published a `MovieRecordCreated` event for the RAW (title, year) reported
  // by each cinema. The TMDB stage ran independently for each raw key, and
  // when the in-flight TMDB call for the first row hadn't completed yet,
  // `hasResolvedSiblingByTitle` returned false and the second TMDB call
  // proceeded to write its own row at (title, year-of-the-second-cinema).
  // mergeAll then collapsed the pair on the next startup, and the cycle
  // repeated next deploy.
  //
  // Fix: recordCinemaScrape returns the *canonical* CacheKey it actually
  // wrote each slot to (the redirect target when one applies, the raw key
  // otherwise). ShowtimeCache publishes MovieRecordCreated using those
  // canonical keys, so both cinemas' bus events name the same key and the
  // TMDB stage runs exactly once. No phantom row, no startup merge.

  "recordCinemaScrape" should "return Helios's canonical key as Multikino's row when both report Diabeł Prada with different years" in {
    val cache = new MovieCache(new FakeRepo)

    // Multikino lands first with year=None.
    val mkTouched = cache.recordCinemaScrape(Multikino, Seq(multikinoPrada))
    mkTouched                 should have size 1
    mkTouched.head._1         shouldBe multikinoPrada
    mkTouched.head._2         shouldBe cache.keyOf(PradaTitle, None)

    // Helios reports year=Some(2026). The redirect absorbs Helios's slot into
    // Multikino's row, and recordCinemaScrape returns that as the canonical
    // key — NOT cache.keyOf(PradaTitle, Some(2026)).
    val helTouched = cache.recordCinemaScrape(Helios, Seq(heliosPrada))
    helTouched                should have size 1
    helTouched.head._1        shouldBe heliosPrada
    helTouched.head._2        shouldBe cache.keyOf(PradaTitle, None)

    // Only one row exists in the cache — no (PradaTitle, Some(2026)) phantom.
    cache.snapshot().size     shouldBe 1
    cache.get(cache.keyOf(PradaTitle, Some(2026))) shouldBe None
  }

  // End-to-end via the bus-driven pipeline: confirms that publishing events
  // with the canonical keys (returned by recordCinemaScrape) prevents the
  // TMDB stage from creating a second row for the year=Some(2026) variant.
  // svc.stop() drains the worker pool so the assertion is deterministic.
  "bus-driven scrape pipeline" should "produce exactly one TMDB-resolved row when two cinemas report Diabeł Prada with different years" in {
    val cache = new MovieCache(new FakeRepo)
    val bus   = new EventBus
    val svc   = new MovieService(cache, bus, tmdbStub())
    bus.subscribe(svc.onMovieRecordCreated)

    // Drive the same wiring `ShowtimeCache.refreshOne` does: publish a
    // MovieRecordCreated for each canonical key returned by recordCinemaScrape.
    // The director hint is deliberately omitted — `verifyByDirector` would
    // otherwise call `/credits`, which the stub doesn't expose, and the
    // duplicate-prevention contract under test doesn't depend on it.
    def scrape(cinema: Cinema, cm: CinemaMovie): Unit = {
      val touched = cache.recordCinemaScrape(cinema, Seq(cm))
      touched.foreach { case (m, key, isNew) =>
        if (isNew)
          bus.publish(MovieRecordCreated(key.cleanTitle, key.year, m.movie.originalTitle, None))
      }
    }

    scrape(Multikino, multikinoPrada)
    scrape(Helios,    heliosPrada)

    // Drain the TMDB worker so any in-flight resolutions land before we read.
    svc.stop()

    val pradaRows = cache.snapshot().filter { case (_, _, e) =>
      e.tmdbId.contains(928344) || e.imdbId.contains("tt12340108")
    }
    pradaRows.size shouldBe 1
    pradaRows.head._3.cinemaShowings.keySet shouldBe Set(Multikino, Helios)
  }
}
