package services.movies

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaCityClient, HeliosClient, MultikinoClient}
import services.events.{InProcessEventBus, MovieRecordCreated}
import tools.HttpFetch

/**
 * Reproduction of the "Mortal Kombat II disappears" report.
 *
 * Three cinemas serve the same film under different `(cleanTitle, year)`
 * tuples after their parsers run:
 *
 *   Multikino    → ("Mortal Kombat 2",  None)         — MultikinoParser
 *                  drops the Polish theatrical date on purpose.
 *   CinemaCity   → ("Mortal Kombat II", Some(2026))   — releaseYear comes
 *                  through as a String ("2026") in the API; the client
 *                  parses it to Int (see CinemaCityClient — was a bug fix).
 *   Helios       → ("Mortal Kombat II", Some(2025))   — yearOfProduction
 *                  is the production year (2025), not the 2026 release.
 *
 * Three distinct CacheKey shapes (different titles AND different years),
 * but all three normalise their cleanTitles to "mortalkombatii". The
 * `recordCinemaScrape` redirect collapses them onto whichever key landed
 * first — so the test invariant is "one visible row with all three
 * cinema slots", not "Multikino + CinemaCity collapse and Helios is
 * separate" (that was the pre-CC-year-fix world). The TMDB stage's
 * `hasResolvedSiblingByTitle` short-circuit prevents subsequent bus
 * events from creating phantom rows at the raw `(title, year)` keys; the
 * log line in production is:
 *
 *   Sister-row match: Mortal Kombat II (2025) → tmdbId=931285 imdbId=tt17490712 via aliases=mortalkombatii
 *
 * The bug this spec pins down: `runTmdbStageSync` builds a fresh
 * `MovieRecord(...)` from scratch with the defaulted `cinemaShowings =
 * Map.empty` and writes it via `cache.put`, which preserves `cinemaTitles`
 * but NOT `cinemaShowings`. That wipes whatever cinema slot
 * `recordCinemaScrape` just landed on the row. The row lingers in
 * `snapshot()` with zero showtimes, and `MovieController.toSchedules`
 * filters it out — the film disappears from the home-page list.
 */
class MortalKombatDisappearanceSpec extends AnyFlatSpec with Matchers {

  // ── Stubs ──────────────────────────────────────────────────────────────────

  private class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  // TMDB returns Mortal Kombat II (tmdbId 931285) for any title search.
  // /external_ids → tt17490712.
  private val Mk2Search =
    """{"results":[{"id":931285,"title":"Mortal Kombat II","original_title":"Mortal Kombat II",""" +
    """"release_date":"2026-05-06","popularity":223.66}]}"""
  private val Mk2ExternalIds = """{"id":931285,"imdb_id":"tt17490712"}"""

  private def tmdbStub() = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie" -> Mk2Search,
      "/external_ids" -> Mk2ExternalIds
    )),
    apiKey = Some("stub")
  )

  // ── Step 1: clients DO fetch the film from their fixtures ─────────────────
  //
  // Each cinema's fixture is the same recorded payload that drives the
  // production scrape — replaying it through the parsers proves the
  // "disappears" report isn't an upstream-missing case.

  private val multikinoMk =
    new MultikinoClient(new FakeHttpFetch("multikino")).fetch()
      .find(_.movie.title == "Mortal Kombat 2").get
  private val cinemaCityMk =
    new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza"))
      .fetch("1078", CinemaCityPoznanPlaza)
      .filter(_.movie.title == "Mortal Kombat II")
      .head
  private val heliosMk =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment")).fetch()
      .filter(_.movie.title == "Mortal Kombat II")
      .head

  "cinema clients" should "all fetch the film from their fixtures (each cinema reports it differently)" in {
    multikinoMk.movie.title       shouldBe "Mortal Kombat 2"
    multikinoMk.movie.releaseYear shouldBe None
    multikinoMk.showtimes         should not be empty

    cinemaCityMk.movie.title       shouldBe "Mortal Kombat II"
    // CinemaCity's payload encodes "releaseYear":"2026" as a string; the
    // client parses both String and Int forms (was a bug fix — `asOpt[Int]`
    // silently dropped every CC year before).
    cinemaCityMk.movie.releaseYear shouldBe Some(2026)
    cinemaCityMk.showtimes         should not be empty

    heliosMk.movie.title           shouldBe "Mortal Kombat II"
    heliosMk.movie.releaseYear     shouldBe Some(2025)
    heliosMk.showtimes             should not be empty
  }

  // ── Step 2: the TMDB stage wipes the cinema slot ──────────────────────────
  //
  // Smallest reproduction. One cinema (Multikino), one scrape, one
  // synchronous TMDB resolution — the slot we just wrote is gone.

  it should "preserve Multikino's slot when the TMDB stage resolves the row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
    val svc   = new MovieService(cache, new InProcessEventBus, tmdbStub())

    cache.recordCinemaScrape(Multikino, Seq(multikinoMk))

    val key = cache.keyOf("Mortal Kombat 2", None)
    cache.get(key).get.cinemaShowings.keySet shouldBe Set(Multikino)

    // Same code path the MovieRecordCreated-driven async wrapper invokes — bypasses
    // the worker pool for a deterministic single-thread reproduction.
    svc.reEnrichSync("Mortal Kombat 2", None)

    val row = cache.get(key).get
    row.tmdbId shouldBe Some(931285)
    row.imdbId shouldBe Some("tt17490712")
    // Currently FAILS: runTmdbStageSync constructs a fresh MovieRecord(...)
    // with the default empty cinemaShowings and writes it through
    // cache.put, which only re-folds cinemaTitles — the Multikino slot is
    // wiped. The view layer (MovieController.toSchedules) then drops this
    // row because allShowtimes is empty.
    row.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  // ── Scrape-order regression: every ordering produces a single visible row ──
  //
  // Steps 3 and 4 used to drive IdentityMerger explicitly to collapse the
  // three Mortal Kombat II rows into one. After the canonical-key fix in
  // recordCinemaScrape + the cinemaScrapes provenance check, no extra rows
  // are created at scrape time, so the merger isn't needed and was removed.
  // The remaining invariant — exactly one row carries the cinema slots,
  // regardless of which cinema scrapes first — is now enforced entirely by
  // the scrape layer + the bus-driven TMDB stage's sibling short-circuit.

  private case class Scrape(cinema: Cinema, title: String, year: Option[Int], cm: CinemaMovie)
  private def scrapes = Seq(
    Scrape(Multikino,             "Mortal Kombat 2",  None,         multikinoMk),
    Scrape(CinemaCityPoznanPlaza, "Mortal Kombat II", Some(2026),   cinemaCityMk),
    Scrape(Helios,                "Mortal Kombat II", Some(2025),   heliosMk)
  )

  for (ordering <- scrapes.permutations.toList) {
    val label = ordering.map(_.cinema.getClass.getSimpleName.stripSuffix("$")).mkString(" → ")
    s"scrape order $label" should "leave exactly one visible Mortal Kombat II row (no merger run)" in {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
      val bus   = new InProcessEventBus
      val svc   = new MovieService(cache, bus, tmdbStub())

      // First scrape resolves the row synchronously so subsequent cinemas
      // can find a sibling with a tmdbId — that's what
      // `hasResolvedSiblingByTitle` needs to short-circuit the TMDB stage.
      val first = ordering.head
      cache.recordCinemaScrape(first.cinema, Seq(first.cm))
      svc.reEnrichSync(first.title, first.year)

      // Subsequent cinemas follow the production flow: recordCinemaScrape
      // first (redirect folds the slot into the existing sibling row),
      // then publish the MovieRecordCreated `ShowtimeCache` would publish. The
      // bus listener's `scheduleTmdbStage` must short-circuit via
      // `hasResolvedSiblingByTitle` (normalize-match) so no phantom row
      // is created at the raw (title, year) key.
      bus.subscribe(svc.onMovieRecordCreated)
      for (s <- ordering.tail) {
        cache.recordCinemaScrape(s.cinema, Seq(s.cm))
        bus.publish(MovieRecordCreated(s.title, s.year, s.cm.movie.originalTitle, s.cm.director))
      }

      def isMk2(e: MovieRecord): Boolean =
        e.tmdbId.contains(931285) || e.imdbId.contains("tt17490712")

      val visibleRows = cache.snapshot().filter { case (_, _, e) =>
        isMk2(e) && e.cinemaShowings.nonEmpty
      }
      visibleRows.size shouldBe 1
      val (_, _, visible) = visibleRows.head
      visible.cinemaShowings.keySet shouldBe Set(Multikino, CinemaCityPoznanPlaza, Helios)
    }
  }
}
