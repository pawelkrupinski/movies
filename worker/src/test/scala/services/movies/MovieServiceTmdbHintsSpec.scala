package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData}
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

  // Helios slot shape — minimal SourceData with director populated.
  private val heliosSlot = SourceData(
    title    = Some(Title),
    director = Seq(Director)
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
      // TMDB never resolved; Helios reported director
      data = Map[Source, SourceData](Helios -> heliosSlot)
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

  // ── Festival/preview "decorated" titles resolve independently of siblings ──
  // "Opętanie | ŻUŁAWSKI. KINO EKSTAZY", "Ojczyzna (pokaz przedpremierowy)" and
  // the like don't match TMDB by their decorated title, so they used to resolve
  // only by copying a tmdbId from a relative listing that happened to resolve
  // first. Under the parallel enrichment cascade that ordering is
  // nondeterministic, which made whole-corpus snapshots flaky. The row now
  // resolves on its own: search the cinema-provided original title + each side
  // of the "X | Y" pipe + the de-parenthesised title.

  "searchTitleCandidates" should "offer the original title, each pipe side, and the de-parenthesised title" in {
    MovieService.searchTitleCandidates("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", Some("Possession")) should
      contain allOf ("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", "Possession", "Opętanie", "ŻUŁAWSKI. KINO EKSTAZY")
    MovieService.searchTitleCandidates("Ojczyzna (pokaz przedpremierowy)", None) should contain ("Ojczyzna")
    MovieService.searchTitleCandidates("Plain Title", None) shouldBe Seq("Plain Title")
  }

  it should "also draw on the row's other reported titles (cinemaTitles + slot originals), de-decorated" in {
    // Every title the cinemas reported for the row becomes a search candidate.
    MovieService.searchTitleCandidates(
      title = "KINO SENIORA | Opętanie", originalTitle = None,
      extraTitles = Seq("Opętanie (pokaz)", "Possession")
    ) should contain allOf ("Opętanie", "Possession")
  }

  "resolveTmdb" should "resolve a decorated title from its own original-title search candidate (no sibling needed)" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val bus   = new InProcessEventBus()
    // The decorated title finds nothing on TMDB; searching the cinema's original
    // title "Possession" finds the film. No sibling row exists and the event
    // carries no director, so the ONLY way this resolves is the originalTitle
    // search candidate — exactly the path that removes the sister-timing race.
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "query=Possession"          -> """{"results":[{"id":21484,"title":"Possession","original_title":"Possession","release_date":"1981-05-27","popularity":9.0}]}""",
      "/search/movie"             -> """{"results":[]}""",
      "/movie/21484/external_ids" -> """{"id":21484,"imdb_id":"tt0082933"}"""
    )), apiKey = Some("stub"))
    val svc = new MovieService(cache, bus, tmdb)
    bus.subscribe(svc.onMovieRecordCreated)

    bus.publish(MovieRecordCreated("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", Some(2026), originalTitle = Some("Possession"), director = None))
    svc.stop()

    val row = cache.get(cache.keyOf("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", Some(2026)))
    row.flatMap(_.tmdbId) shouldBe Some(21484)
    row.flatMap(_.imdbId) shouldBe Some("tt0082933")
  }
}
