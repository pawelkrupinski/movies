package services.movies

import clients.TmdbClient
import models.{CinemaShowings, Helios, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieRecordCreated}
import tools.GetOnlyHttpFetch

/**
 * Regression tests for the "Kurozając i świątynia świstaka" class of bug:
 * a film whose Polish title has no entry on TMDB resolves only via the
 * `directorWalk` path. When the first cinema to scrape it doesn't report a
 * director (CinemaCity, Charlie Monroe), the TMDB stage misses and
 * `cache.markMissing(key)` poisons the negative cache. The next cinema
 * (Helios, Multikino) DOES report a director — but two paths drop that hint:
 *
 *   1. `scheduleTmdbStage` early-returns on `isNegative`, never letting the
 *      new hint reach `runTmdbStage`.
 *   2. The daily `retryUnresolvedTmdb` walks unresolved rows but runs
 *      `runTmdbStage(k)` blind — the row's accumulated cinemaShowings.director
 *      is ignored.
 *
 * Both paths trap the row at tmdbId=None for up to 24h. The fixes:
 *
 *   1. Skip the negative-cache short-circuit when a fresh `director` or
 *      `originalTitle` hint is present.
 *   2. Pull `director` + `cinemaOriginalTitle` from the existing row and pass
 *      them to `runTmdbStage` during the retry.
 *
 * Both specs use the real `directorWalk` chain end-to-end (search returns
 * nothing → `findPerson` → `personDirectorCredits` → `imdbId`) against a
 * stubbed TmdbClient. The film modelled is real (Kurozając, tmdb=1215532,
 * imdb=tt31260224, dir. Benjamin Mousquet, year 2025) — gives the diff
 * reviewer a concrete reference.
 */
class MovieServiceTmdbHintsSpec extends AnyFlatSpec with Matchers {

  private val Title    = "Kurozając i Świątynia Świstaka"
  private val Year     = Some(2025)
  private val Director = "Benjamin Mousquet"
  private val TmdbId   = 1215532
  private val PersonId = 2905749
  private val ImdbId   = "tt31260224"

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  // TMDB stub modelling the real Kurozając resolution chain:
  //   - title search returns nothing (TMDB has no Polish title for this film)
  //   - `findPerson("Benjamin Mousquet")` → personId 2905749
  //   - `personDirectorCredits(2905749)` → one credit with releaseYear=2025
  //     pointing at tmdbId 1215532
  //   - `imdbId(1215532)` → tt31260224
  private def kurozajacTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Benjamin Mousquet","known_for_department":"Directing"}]}""",
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":$TmdbId,"title":"Hopper et le Secret de la Marmotte","original_title":"Chickenhare and the Secret of the Groundhog",
        | "release_date":"2025-08-13","department":"Directing","popularity":4.0}
        |]}""".stripMargin,
      s"/movie/$TmdbId/external_ids" -> s"""{"id":$TmdbId,"imdb_id":"$ImdbId"}"""
    )),
    apiKey = Some("stub")
  )

  // Helios slot shape — minimal CinemaShowings with director populated.
  private val heliosSlot = CinemaShowings(
    filmUrl = None, posterUrl = None, synopsis = None, cast = None,
    director = Some(Director), runtimeMinutes = None, releaseYear = None,
    showtimes = Seq.empty
  )

  // ── Fix 1 — bus path: new director hint must bypass the negative cache ────

  "scheduleTmdbStage" should "bypass the isNegative short-circuit when a fresh director hint arrives" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val bus   = new InProcessEventBus()
    val svc   = new MovieService(cache, bus, kurozajacTmdb())
    bus.subscribe(svc.onMovieRecordCreated)

    val key = cache.keyOf(Title, Year)
    // Simulate the state a CC-first scrape leaves behind: cache says we've
    // already tried this key and TMDB returned no hit.
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true

    // Helios-style event: same canonical key, but now carrying a director
    // hint the prior attempt never had. With the bug this is dropped on the
    // isNegative early-return; with the fix `directorWalk` resolves it.
    bus.publish(MovieRecordCreated(Title, Year, originalTitle = None, director = Some(Director)))
    svc.stop()  // drains the worker pool — sync wait for runTmdbStage to land

    val row = cache.get(key)
    row.flatMap(_.tmdbId) shouldBe Some(TmdbId)
    row.flatMap(_.imdbId) shouldBe Some(ImdbId)
  }

  // Sanity: when the event carries no fresh hint, we DO still honour the
  // negative cache. Without this the fix would turn every redundant
  // scrape-tick into a TMDB hammer for known misses.
  it should "still short-circuit on isNegative when the event carries no new hints" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val bus   = new InProcessEventBus()
    // Tmdb stub that throws on any access — proves we never tried.
    val tmdb  = new TmdbClient(http = new GetOnlyHttpFetch {
      override def get(url: String): String =
        throw new RuntimeException(s"TMDB should not be called: $url")
    }, apiKey = Some("stub"))
    val svc   = new MovieService(cache, bus, tmdb)
    bus.subscribe(svc.onMovieRecordCreated)

    val key = cache.keyOf(Title, Year)
    cache.markMissing(key)

    // No director, no originalTitle — re-publish under the same conditions
    // that produced the miss. Must short-circuit, NOT hammer TMDB.
    noException should be thrownBy bus.publish(MovieRecordCreated(Title, Year, None, None))
    svc.stop()
  }

  // ── Fix 2 — retry path: hints must be sourced from cinemaShowings ─────────

  "retryUnresolvedTmdb" should "pass cinemaShowings-derived director as a hint so directorWalk fires" in {
    // Pre-seeded row: a previous scrape tick wrote the Helios slot (which
    // carries the director) but TMDB resolution was poisoned earlier. tmdbId
    // is None; the daily retry tick needs to recover.
    val seeded = MovieRecord(
      imdbId         = None, imdbRating = None, metascore = None,
      originalTitle  = None,                              // TMDB never resolved
      cinemaShowings = Map(Helios -> heliosSlot)          // but Helios reported director
    )
    val repo  = new InMemoryMovieRepo(Seq((Title, Year, seeded)))
    val cache = new CaffeineMovieCache(repo)
    val svc   = new MovieService(cache, new InProcessEventBus(), kurozajacTmdb())

    svc.retryUnresolvedTmdb()
    svc.stop()  // drain the worker pool

    val row = cache.get(cache.keyOf(Title, Year))
    row.flatMap(_.tmdbId) shouldBe Some(TmdbId)
    row.flatMap(_.imdbId) shouldBe Some(ImdbId)
  }
}
