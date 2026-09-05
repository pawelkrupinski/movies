package services.movies

import clients.TmdbClient
import models.{Filmweb, Helios, Imdb, KinoMuza, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import tools.GetOnlyHttpFetch
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Regression tests for the "Kurozając i świątynia świstaka" class of bug:
 * a film the TMDB title SEARCH doesn't return resolves only via the
 * `directorWalk` path. When the first cinema to scrape it doesn't report a
 * director (CinemaCity, Charlie Monroe), the TMDB stage misses and
 * `cache.markMissing(key)` poisons the negative cache. The next cinema
 * (Helios, Multikino) DOES report a director — but two paths drop that hint:
 *
 *   1. `needsTmdbResolution` early-returns on `isNegative`, never dispatching
 *      the new hint to `resolveTmdbOnce`.
 *   2. The daily `retryUnresolvedTmdb` walks unresolved rows but dispatches
 *      them blind — the row's accumulated cinemaShowings.director is ignored.
 *
 * Both paths trap the row at tmdbId=None for up to 24h. The fixes:
 *
 *   1. Skip the negative-cache short-circuit when a fresh `director` or
 *      `originalTitle` hint is present.
 *   2. Pull `cinemaDirector` + `cinemaOriginalTitle` from the existing row and
 *      pass them to `resolveTmdbOnce` during the retry.
 *
 * Both specs use the real `directorWalk` chain end-to-end (search returns
 * nothing → `findPerson` → `personDirectorCredits` → `imdbId`) against a
 * stubbed TmdbClient. The film modelled is real (Kurozając, tmdb=1215532,
 * imdb=tt31260224, directory. Benjamin Mousquet, year 2025) — gives the diff
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
  //   - the title SEARCH returns nothing, so the director walk is the only way in
  //   - `findPerson("Benjamin Mousquet")` → personId 2905749
  //   - `personDirectorCredits(2905749)` → one credit with releaseYear=2025
  //     pointing at tmdbId 1215532
  //   - `imdbId(1215532)` → tt31260224
  //
  // The credit carries the Polish `title` beside the French `original_title`,
  // which is what TMDB's pl-PL credits actually return for this film (verified
  // against the live API and the recorded corpus). The stub used to give only the
  // foreign titles, from back when TMDB had no Polish entry for it — harmless
  // while a year-pinned credit was accepted on the year alone, but a credit must
  // now agree with the cinema's title on at least one distinctive word, so a stale
  // fake would model a resolution that reality no longer needs the year for.
  private def kurozajacTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Benjamin Mousquet","known_for_department":"Directing"}]}""",
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":$TmdbId,"title":"Kurozając i świątynia Świstaka","original_title":"Hopper et le secret de la marmotte",
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

  "needsTmdbResolution (bus path)" should "bypass the isNegative short-circuit when a fresh director hint arrives" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val bus   = new InProcessEventBus()
    val service   = new MovieService(cache, bus, kurozajacTmdb())
    bus.subscribe(service.onMovieDetailsComplete)

    val key = cache.keyOf(Title, Year)
    // Simulate the state a CC-first scrape leaves behind: cache says we've
    // already tried this key and TMDB returned no hit.
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true

    // Helios-style event: same canonical key, but now carrying a director
    // hint the prior attempt never had. With the bug this is dropped on the
    // isNegative early-return; with the fix `directorWalk` resolves it.
    bus.publish(MovieDetailsComplete(Title, Year, originalTitle = None, director = Some(Director)))
    service.stop()  // drains the inline executionContext pool — sync wait for resolveTmdbOnce to land

    val row = cache.get(key)
    row.flatMap(_.tmdbId) shouldBe Some(TmdbId)
    row.flatMap(_.imdbId) shouldBe Some(ImdbId)
  }

  // Sanity: when the event carries no fresh hint, we DO still honour the
  // negative cache. Without this the fix would turn every redundant
  // scrape-tick into a TMDB hammer for known misses.
  it should "still short-circuit on isNegative when the event carries no new hints" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val bus   = new InProcessEventBus()
    // Tmdb stub that throws on any access — proves we never tried.
    val tmdb  = new TmdbClient(http = new GetOnlyHttpFetch {
      override def get(url: String): String =
        throw new RuntimeException(s"TMDB should not be called: $url")
    }, apiKey = Some("stub"))
    val service   = new MovieService(cache, bus, tmdb)
    bus.subscribe(service.onMovieDetailsComplete)

    val key = cache.keyOf(Title, Year)
    cache.markMissing(key)

    // No director, no originalTitle — re-publish under the same conditions
    // that produced the miss. Must short-circuit, NOT hammer TMDB.
    noException should be thrownBy bus.publish(MovieDetailsComplete(Title, Year, None, None))
    service.stop()
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
    val repository  = new InMemoryMovieRepository(Seq((Title, Year, seeded)))
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val service   = new MovieService(cache, new InProcessEventBus(), kurozajacTmdb())

    service.retryUnresolvedTmdb()
    service.stop()  // drain the worker pool

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

  // A banner is joined with a dash as often as a pipe. "500 mil" is the worked
  // example: TMDB's Polish title is exactly "500 mil", so the film should resolve
  // on its title alone — but the whole decorated string was the only candidate, so
  // it fell through to the year-pinned branch instead. Both dash forms occur.
  it should "split a dash-joined programme banner, without touching hyphenated words" in {
    MovieService.searchTitleCandidates("Filmoczule Dla Edukacji z Odn i WZiSS Ump – 500 mil", None) should contain ("500 mil")
    MovieService.searchTitleCandidates("Ladies Night - Narodziny gwiazdy", None) should contain ("Narodziny gwiazdy")
    MovieService.searchTitleCandidates("Spider-Man", None) shouldBe Seq("Spider-Man")
  }

  it should "also draw on the row's other reported titles (cinemaTitles + slot originals), de-decorated" in {
    // Every title the cinemas reported for the row becomes a search candidate.
    MovieService.searchTitleCandidates(
      title = "KINO SENIORA | Opętanie", originalTitle = None,
      extraTitles = Seq("Opętanie (pokaz)", "Possession")
    ) should contain allOf ("Opętanie", "Possession")
  }

  "resolveTmdb" should "resolve a decorated title from its own original-title search candidate (no sibling needed)" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
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
    val service = new MovieService(cache, bus, tmdb)
    bus.subscribe(service.onMovieDetailsComplete)

    bus.publish(MovieDetailsComplete("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", Some(2026), originalTitle = Some("Possession"), director = None))
    service.stop()

    val row = cache.get(cache.keyOf("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", Some(2026)))
    row.flatMap(_.tmdbId) shouldBe Some(21484)
    row.flatMap(_.imdbId) shouldBe Some("tt0082933")
  }

  // ── Staging path: candidates come from the PASSED row, not the cache ──────────
  // `resolveStagingRecord` runs on a `pending_movies` row that is NOT in the
  // MovieCache, so `resolveTmdb` can no longer mine its search candidates from
  // `cache.get(...)`. It must mine them from the `existing` record handed in. The
  // bug this guards (the real "Orły republiki" miss): the bare key title didn't
  // match TMDB, only a cinema-reported title did — and with the candidates sourced
  // from an empty cache, that cinema title never became a query, so the 5-cinema
  // variant resolved to a no-match while the corpus-wide direct path resolved it.
  "resolveStagingRecord" should "mine search candidates from the passed row's cinema titles (cache-free)" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb  = new TmdbClient(http = new StubFetch(Map(
      // The bare staging title finds nothing; the cinema-reported title does.
      "query=Backrooms"           -> """{"results":[{"id":1083381,"title":"Backrooms","original_title":"Backrooms","release_date":"2026-01-01","popularity":9.0}]}""",
      "/search/movie"             -> """{"results":[]}""",
      "/movie/1083381/external_ids" -> """{"id":1083381,"imdb_id":"tt9999999"}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // The staging row's key title ("Premiera") misses TMDB; only the Helios slot's
    // reported title ("Backrooms") matches. The row is NOT written to the cache.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Backrooms"))))
    val resolved = service.resolveStagingRecord("Premiera", Some(2026), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1083381)
    resolved.flatMap(_.imdbId) shouldBe Some("tt9999999")
    cache.get(cache.keyOf("Premiera", Some(2026))) shouldBe None // never touched the cache
  }

  /**
   * A resolution must never be corroborated by its OWN output.
   *
   * `resolveTmdbId` mined its director hints from `row.data.values` — EVERY slot,
   * including the derived `Tmdb`/`Imdb`/`Filmweb` ones the previous resolution
   * stamped on. So a row that resolved to the wrong film grew a second "reported"
   * director: the wrong film's. The hints are `.sorted` and the walk takes the
   * FIRST that hits, so which film the row re-resolved to came down to alphabetical
   * order between the cinema's director and the mis-resolution's own.
   *
   * Poland's Kino Malta "Dreams" is the worked example. The cinema reports Michel
   * Franco (its `/movies/dreams` page names him, with the Mexican ballet-dancer
   * synopsis); the row had resolved to Dag Johan Haugerud's Norwegian "Drømmer"
   * (tmdb 1228682, tt30810787), so the Tmdb/Imdb/Filmweb slots all said Haugerud.
   * `["Dag Johan Haugerud", "Michel Franco"].sorted` walks Haugerud first, his 2024
   * credit pins on the key year the mis-resolution itself had set, and the wrong
   * answer re-confirms itself — Michel Franco is never tried at all. The row served
   * a chimera: Franco's director + synopsis beside Drømmer's cast, original title
   * and ratings.
   *
   * `cinemaOriginalTitle` is already cinema-only for exactly this reason; the
   * director hint is its missing sibling.
   */
  it should "mine director hints from CINEMA slots only, never from the derived TMDB/IMDb/Filmweb slots" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    // Title search is useless here (as in production: "Dreams" is ambiguous), so
    // directorWalk is the only path that can resolve — which director it walks
    // FIRST is the whole test.
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"                    -> """{"results":[]}""",
      "query=Michel+Franco"              -> """{"results":[{"id":5000,"name":"Michel Franco","known_for_department":"Directing"}]}""",
      "query=Dag+Johan+Haugerud"         -> """{"results":[{"id":6000,"name":"Dag Johan Haugerud","known_for_department":"Directing"}]}""",
      "/person/5000/movie_credits"       -> """{"crew":[
        |{"id":1134463,"title":"Dreams","original_title":"Dreams: Sueños",
        | "release_date":"2025-07-10","department":"Directing","popularity":6.2}
        |]}""".stripMargin,
      "/person/6000/movie_credits"       -> """{"crew":[
        |{"id":1228682,"title":"Sny o miłości","original_title":"Drømmer",
        | "release_date":"2024-10-04","department":"Directing","popularity":8.0}
        |]}""".stripMargin,
      "/movie/1134463/external_ids"      -> """{"id":1134463,"imdb_id":"tt31710990"}""",
      "/movie/1228682/external_ids"      -> """{"id":1228682,"imdb_id":"tt30810787"}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // Production's row shape: ONE cinema slot naming Michel Franco, and three
    // derived slots carrying the previous (wrong) resolution's director.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios  -> SourceData(title = Some("Dreams"), director = Seq("Michel Franco")),
      Tmdb    -> SourceData(director = Seq("Dag Johan Haugerud"), releaseYear = Some(2024)),
      Imdb    -> SourceData(director = Seq("Dag Johan Haugerud")),
      Filmweb -> SourceData(director = Seq("Dag Johan Haugerud"))
    ))
    // Year 2024 is the mis-resolution's own year, baked into the canonical key —
    // it must not be able to pin Haugerud's same-year credit either.
    val resolved = service.resolveStagingRecord("Dreams", Some(2024), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1134463)
    resolved.flatMap(_.imdbId) shouldBe Some("tt31710990")
  }

  /**
   * A year a CINEMA reported must reach the search, even when the row itself has none.
   *
   * The staging row's `year` column is fixed when the newcomer is diverted, before any
   * detail page has been fetched; the detail then merges `releaseYear` onto the SLOT and
   * nothing back-fills the column. Resolution read only the column, so a film whose
   * detail page states the year plainly still reached TMDB year-less — and a year-less,
   * director-less search is one `resolveTmdbId` correctly refuses to guess at.
   *
   * Poland's "Pokój 666" is the worked example: production's Kino Iluzjon slot carries
   * `releaseYear=1982` and `director=Wim Wenders`, both parsed from the very detail page
   * the replay had recorded, replayed and merged — and the leg still logged
   * `'Pokój 666' (?) → no match`.
   *
   * `ImdbIdResolver.resolve` already reads the cinema-reported years this way; this
   * brings TMDB resolution in line with it.
   */
  it should "use a year a cinema slot reported when the row itself carries none" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb  = new TmdbClient(http = new StubFetch(Map(
      // Year-scoped search finds the film; the year-less one is ambiguous and refused.
      "year=1982"                 -> """{"results":[{"id":118257,"title":"Room 666","original_title":"Chambre 666","release_date":"1982-05-01","popularity":5.0}]}""",
      "/search/movie"             -> """{"results":[{"id":1,"title":"A"},{"id":2,"title":"B"}]}""",
      "/movie/118257/external_ids" -> """{"id":118257,"imdb_id":"tt0083727"}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // No row year — exactly what a diverted newcomer has. The year lives on the slot,
    // put there by the detail fetch.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Pokój 666"), releaseYear = Some(1982))))
    val resolved = service.resolveStagingRecord("Pokój 666", None, existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(118257)
  }

  /**
   * A title a PREVIOUS resolution wrote must never outrank the title the CINEMAS
   * published — the original-title half of the hint leak the director test above
   * closes.
   *
   * `resolveTmdbId` deliberately folds every slot's `originalTitle` into the
   * director-walk's candidate set, derived Tmdb/Imdb/Filmweb slots included: that
   * is how a film TMDB doesn't index under its Polish title gets found once
   * Filmweb supplies the original (`MovieRecord.resolverOriginalTitles`). But the
   * walk treated all candidates as one flat set and broke ties by LOWEST TMDB ID,
   * so a mis-resolution's own original title competed with the cinemas' title on
   * equal terms — and won whenever the wrong film happened to carry the lower id.
   *
   * Poland's "Mistyczka" is the worked example. Every cinema calls it "Mistyczka"
   * (Jan Sobierajski, 2026); the row had drifted onto Sobierajski's OTHER 2026
   * film, "Maryja. Matka papieża" (tmdb 1646379), so the Filmweb and IMDb slots
   * both said "Maryja. Matka Papieża". Once TMDB listed the real film
   * (tmdb 1731866) both credits matched a candidate exactly — and 1646379 < 1731866,
   * so the row re-confirmed the wrong film every cycle and served its original
   * title, ratings and Filmweb URL.
   *
   * The fix ranks a credit matching a CINEMA-reported title above one matching a
   * derived title; the derived titles still resolve when no cinema title matches
   * any credit, which is the case they were added for.
   */
  it should "prefer a credit matching a CINEMA title over one matching a derived slot's original title" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"              -> """{"results":[]}""",
      "query=Jan+Sobierajski"      -> """{"results":[{"id":9001,"name":"Jan Sobierajski","known_for_department":"Directing"}]}""",
      "/person/9001/movie_credits" -> """{"crew":[
        |{"id":1646379,"title":"Maryja. Matka papieża","original_title":"Maryja. Matka papieża",
        | "release_date":"2026-04-17","department":"Directing","popularity":2.0},
        |{"id":1731866,"title":"Mistyczka","original_title":"Mistyczka",
        | "release_date":"2026-09-11","department":"Directing","popularity":1.0}
        |]}""".stripMargin,
      "/movie/1646379/external_ids" -> """{"id":1646379,"imdb_id":"tt42003610"}""",
      "/movie/1731866/external_ids" -> """{"id":1731866,"imdb_id":null}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // Production's row shape: the cinemas report only "Mistyczka"; the derived
    // slots carry the previous (wrong) resolution's original title.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios  -> SourceData(title = Some("Mistyczka"), director = Seq("Jan Sobierajski")),
      Imdb    -> SourceData(originalTitle = Some("Maryja. Matka Papieza")),
      Filmweb -> SourceData(originalTitle = Some("Maryja. Matka Papieża"))
    ))
    val resolved = service.resolveStagingRecord("Mistyczka", Some(2026), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1731866)
  }

  /**
   * When a row really is holding TWO films, both of their titles are cinema-reported
   * and the walk's lowest-id tie-break decides between them by accident.
   *
   * Prod's "Mistyczka" row: 38 venues list "Mistyczka" (tmdb 1731866), and Kino Klaps
   * lists "DOBRE Kino - Maryja. Matka Papieża" — Jan Sobierajski's OTHER 2026 film
   * (tmdb 1646379), whose own row merged in when both had wrongly resolved to it.
   * Both credits then match a CINEMA title exactly, so the cinema-first ranking
   * cannot separate them and 1646379 < 1731866 handed the row to the single venue.
   *
   * The count is the evidence the set of titles threw away: prefer the credit the
   * MOST venues name. Lowest id still breaks a real tie, which is the TMDB
   * adjacent-year duplicate it was written for.
   */
  it should "prefer the credit the MOST cinemas name when a row holds two films" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"              -> """{"results":[]}""",
      "query=Jan+Sobierajski"      -> """{"results":[{"id":9001,"name":"Jan Sobierajski","known_for_department":"Directing"}]}""",
      "/person/9001/movie_credits" -> """{"crew":[
        |{"id":1646379,"title":"Maryja. Matka papieża","original_title":"Maryja. Matka papieża",
        | "release_date":"2026-04-17","department":"Directing","popularity":2.0},
        |{"id":1731866,"title":"Mistyczka","original_title":"Mistyczka",
        | "release_date":"2026-09-11","department":"Directing","popularity":1.0}
        |]}""".stripMargin,
      "/movie/1646379/external_ids" -> """{"id":1646379,"imdb_id":"tt42003610"}""",
      "/movie/1731866/external_ids" -> """{"id":1731866,"imdb_id":null}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // Two venues call it "Mistyczka"; one lists the other film under a programme
    // banner. Both titles are cinema-published, so only the COUNT tells them apart.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios     -> SourceData(title = Some("Mistyczka"), director = Seq("Jan Sobierajski")),
      Multikino  -> SourceData(title = Some("Mistyczka")),
      KinoMuza   -> SourceData(title = Some("DOBRE Kino - Maryja. Matka Papieża"))
    ))
    val resolved = service.resolveStagingRecord("Mistyczka", Some(2026), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1731866)
  }

  /**
   * The venue that names the film through an ACCESSIBILITY-decorated listing still
   * counts as naming it.
   *
   * `cinemaCandidates` is built with `apiQuery`, which strips that decoration
   * ("Kino bez barier: Freak Show (AD + CC + PJM)" → "Freak Show"); the weight map
   * was built without it, so such a venue put its credit in the cinema tier and then
   * scored ZERO — and on a row holding two films the other film's single venue could
   * outvote it. Both sides read the same query forms now.
   */
  it should "count a venue that names the film only through its accessibility-decorated title" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"              -> """{"results":[]}""",
      "query=Ada+Reg"              -> """{"results":[{"id":7100,"name":"Ada Reg","known_for_department":"Directing"}]}""",
      "/person/7100/movie_credits" -> """{"crew":[
        |{"id":100,"title":"Freak Show","original_title":"Freak Show",
        | "release_date":"2026-03-01","department":"Directing","popularity":1.0},
        |{"id":900,"title":"Inna rzecz","original_title":"Inna rzecz",
        | "release_date":"2026-05-01","department":"Directing","popularity":9.0}
        |]}""".stripMargin,
      "/movie/100/external_ids" -> """{"id":100,"imdb_id":null}""",
      "/movie/900/external_ids" -> """{"id":900,"imdb_id":null}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // Two venues name "Freak Show", both only through the accessibility banner; one
    // names the director's other film outright. Lowest id already favours 100, so the
    // WEIGHT is what this pins — make the wrong film the lower id.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios    -> SourceData(title = Some("Kino bez barier: Freak Show (AD + CC + PJM)"), director = Seq("Ada Reg")),
      Multikino -> SourceData(title = Some("Kino bez barier: Freak Show (AD + CC + PJM)")),
      KinoMuza  -> SourceData(title = Some("Inna rzecz"))
    ))
    val resolved = service.resolveStagingRecord("Freak Show", Some(2026), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(100)
  }
}
