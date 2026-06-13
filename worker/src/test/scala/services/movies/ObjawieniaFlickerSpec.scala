package services.movies

import clients.TmdbClient
import models.{Cinema, CinemaMovie, Helios, Movie, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.RoutingHttpFetch

import java.time.LocalDateTime

/**
 * Regression for the flickering "Dzień objawienia" — a film reported WITH a
 * release year by some cinemas (Helios → TMDB-resolved) and WITHOUT one by
 * others (Multikino). A concurrent scrape can strand the yearless reports in
 * their own `(title, None)` cache row beside the resolved `(title, Some(year))`
 * one; the yearless row is unresolved so the read-model projector holds it back
 * (`readyToProject == false`) — its cinemas vanish and the page shows only
 * Helios, until the periodic settle folds them back (then the next scrape
 * re-strands them → the flicker).
 *
 * Two fixes, both exercised here:
 *   - PART B: a later scrape of an already-concluded film lands its slot
 *     straight on the concluded row (`concludedKeyFor`, matched by title +
 *     optional year), so no held-back yearless variant is ever (re)spawned.
 *   - PART A: the moment TMDB resolution concludes (hit OR miss), the film
 *     settles its own `sanitize(title)` group in ONE merged write
 *     (`settleResolved`), folding any already-stranded sibling — so the row's
 *     first `readyToProject` upsert already carries every cinema (the read model
 *     never sees the single-cinema split).
 */
class ObjawieniaFlickerSpec extends AnyFlatSpec with Matchers {

  private val Title = "Dzień objawienia"
  private val When  = LocalDateTime.of(2026, 6, 14, 18, 0)

  private def slot(cinema: Cinema, year: Option[Int]): (Source, SourceData) =
    (cinema: Source) -> SourceData(
      title       = Some(Title),
      releaseYear = year,
      director    = Seq("Steven Spielberg"),
      showtimes   = Seq(Showtime(When, bookingUrl = None))
    )

  private def scrape(cinema: Cinema, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = Title, releaseYear = year),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Seq("Steven Spielberg"),
      showtimes = Seq(Showtime(When, bookingUrl = None))
    )

  // What the read model would actually project: only `readyToProject` rows
  // reach `web_movies` (ReadModelProjector holds the rest back), so this is the
  // set of cinemas a user would SEE for the film.
  private def projectedCinemas(cache: MovieCache): Set[Cinema] =
    cache.snapshot().filter(_.record.readyToProject).flatMap(_.record.cinemaData.keys).toSet

  private def tmdbHit() = new TmdbClient(
    http = new RoutingHttpFetch(Map(
      "/search/movie" -> ("""{"results":[{"id":1275779,"title":"Dzień objawienia",""" +
        """"original_title":"The Revelation","release_date":"2026-01-01","popularity":99.0}]}"""),
      "/external_ids" -> """{"id":1275779,"imdb_id":"tt15047880"}""",
      "/credits"      -> """{"crew":[{"job":"Director","name":"Steven Spielberg"}]}"""
    ), getOnly = true),
    apiKey = Some("stub")
  )

  private def tmdbMiss() = new TmdbClient(
    http = new RoutingHttpFetch(Map("/search/movie" -> """{"results":[]}"""), getOnly = true),
    apiKey = Some("stub")
  )

  private def service(cache: MovieCache, tmdb: TmdbClient): MovieService =
    new MovieService(cache, new InProcessEventBus, tmdb)

  // ── PART B: a re-scrape folds into the concluded row, by title + opt. year ──

  "a later yearless scrape" should "fold into an already-resolved sibling instead of stranding a held-back row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)

    // The resolved, year-bearing row (Helios reported 2026, TMDB resolved it) …
    cache.put(cache.keyOf(Title, Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"), data = Map(slot(Helios, Some(2026)))))
    // … and a stranded yearless, still-unresolved row a race left behind.
    cache.put(cache.keyOf(Title, None),
      MovieRecord(data = Map(slot(Multikino, None))))

    // A fresh Multikino tick (still yearless). With two same-title rows present,
    // the unique-match redirect gives up — `concludedKeyFor` must still land the
    // slot on the resolved row.
    cache.recordCinemaScrape(Multikino, Seq(scrape(Multikino, None)))

    val resolved = cache.get(cache.keyOf(Title, Some(2026))).getOrElse(fail("resolved row vanished"))
    resolved.cinemaData.keySet shouldBe Set(Helios, Multikino)
    // The user sees both cinemas — not just Helios.
    projectedCinemas(cache) shouldBe Set(Helios, Multikino)
  }

  // ── PART A: conclusion settles the film's group in one merged write ─────────

  "a TMDB HIT" should "fold a stranded yearless sibling onto the resolved row at conclusion" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
    val svc   = service(cache, tmdbHit())

    // Unresolved pair, as a concurrent scrape leaves it: yeared Helios + yearless Multikino.
    cache.put(cache.keyOf(Title, Some(2026)), MovieRecord(data = Map(slot(Helios, Some(2026)))))
    cache.put(cache.keyOf(Title, None),       MovieRecord(data = Map(slot(Multikino, None))))

    svc.reEnrichSync(Title, Some(2026))

    val rows = cache.snapshot()
    withClue(s"expected ONE row after conclusion, got ${rows.map(r => (r.title, r.year))}\n") {
      rows.size shouldBe 1
    }
    rows.head.record.tmdbId            shouldBe Some(1275779)
    rows.head.record.cinemaData.keySet shouldBe Set(Helios, Multikino)
    projectedCinemas(cache)            shouldBe Set(Helios, Multikino)
  }

  "a TMDB MISS" should "still fold a stranded yearless sibling onto the concluded row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
    val svc   = service(cache, tmdbMiss())

    cache.put(cache.keyOf(Title, Some(2026)), MovieRecord(data = Map(slot(Helios, Some(2026)))))
    cache.put(cache.keyOf(Title, None),       MovieRecord(data = Map(slot(Multikino, None))))

    // The miss conclusion (tmdbNoMatch) lives in `resolveTmdbOnce`, the
    // production ResolveTmdb handler path — `reEnrichSync` only handles hits.
    svc.resolveTmdbOnce(Title, Some(2026), None, None, force = true)

    val rows = cache.snapshot()
    withClue(s"expected ONE row after a concluded miss, got ${rows.map(r => (r.title, r.year))}\n") {
      rows.size shouldBe 1
    }
    rows.head.record.tmdbConcluded     shouldBe true
    rows.head.record.cinemaData.keySet shouldBe Set(Helios, Multikino)
    projectedCinemas(cache)            shouldBe Set(Helios, Multikino)
  }
}
