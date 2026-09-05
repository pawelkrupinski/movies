package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import tools.GetOnlyHttpFetch
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Regression guard: a director-less first scrape of a same-title, same-year TMDB
 * ambiguity resolves to the most-popular exact match — but when the real director
 * arrives and doesn't verify against that film's credits, the row re-resolves via
 * director-walk to the correct film.
 *
 * Modelled on a real same-title, SAME-YEAR TMDB ambiguity (captured from the
 * live API), where the year can't disambiguate — only the director can:
 *   - tmdb 881487  "The Visitor" (2022), director Justin P. Lange, imdb tt15558152.
 *     PL title "Gość", original_title "The Visitor", popularity 1.41. The most-
 *     popular exact match for a director-less "The Visitor"/2022 query.
 *   - tmdb 1026057 "The Visitor" (2022), director Itay Gordon (person 3706395),
 *     popularity 0.15 — the film the cinema is actually showing.
 *
 * CinemaCity scrapes first WITHOUT a director → resolves to the popular decoy 881487.
 * Helios then reports the real director "Itay Gordon"; the verify step finds "Itay
 * Gordon" absent from 881487's credits (Justin P. Lange) → re-resolves via
 * director-walk to the CORRECT 1026057 the cinema is showing.
 */
class TmdbMisresolveSpec extends AnyFlatSpec with Matchers {

  private val Title    = "The Visitor"
  private val Year     = Some(2022)
  private val Decoy    = 881487    // Justin P. Lange — what TMDB returns without a director
  private val Correct  = 1026057   // Itay Gordon — the film the cinema is actually showing
  private val Director = "Itay Gordon"
  private val PersonId = 3706395

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  // `fullDetails` (/movie/{id}?…append_to_response=credits) is intentionally
  // unstubbed — `runTmdbStageSync` tolerates its failure and falls back to the
  // search-hit shape, so the stub only needs the resolution-path endpoints.
  private def visitorTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      // Year-restricted title search returns BOTH 2022 "The Visitor" films, the
      // more-popular (Justin P. Lange) entry FIRST so `pickBest` lands on it.
      "/search/movie" -> s"""{"results":[
        |{"id":$Decoy,"title":"Gość","original_title":"The Visitor","release_date":"2022-10-07","popularity":1.413},
        |{"id":$Correct,"title":"The Visitor","original_title":"The Visitor","release_date":"2022-06-01","popularity":0.145}
        |]}""".stripMargin,
      // Decoy credits: Justin P. Lange, NOT Itay Gordon → verifyByDirector rejects it.
      s"/movie/$Decoy/credits"      -> """{"crew":[{"id":63306,"name":"Justin P. Lange","job":"Director"}]}""",
      s"/movie/$Decoy/external_ids" -> s"""{"id":$Decoy,"imdb_id":"tt15558152"}""",
      // Director-walk recovery for "Itay Gordon" → 1026057 (his 2022 credit).
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Itay Gordon","known_for_department":"Directing"}]}""",
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":$Correct,"title":"The Visitor","release_date":"2022-06-01","department":"Directing"},
        |{"id":1026079,"title":"The Visitor Part 2","release_date":"2024-01-01","department":"Directing"}
        |]}""".stripMargin,
      s"/movie/$Correct/external_ids" -> s"""{"id":$Correct,"imdb_id":""}"""
    )),
    apiKey = Some("stub")
  )

  "a film mis-resolved against a director-less first scrape" should
    "be corrected once the real director arrives" in {
    // CinemaCity scraped it first, no director reported.
    val seed  = MovieRecord(data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(Title))))
    val repository  = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val bus   = new InProcessEventBus()
    val service   = new MovieService(cache, bus, visitorTmdb())
    val key   = cache.keyOf(Title, Year)

    // 1. Resolve against the director-less row — no director to pick between the two
    //    same-year "The Visitor" films, so searchYearExactTop picks the most-popular
    //    exact match: the decoy (881487, Justin P. Lange).
    service.reEnrichSync(Title, Year)
    cache.get(key).flatMap(_.tmdbId) shouldBe Some(Decoy)

    // 2. Helios now reports the real director "Itay Gordon".
    cache.putIfPresent(key, r =>
      r.copy(data = r.data + (Helios -> SourceData(title = Some(Title), director = Seq(Director)))))

    // 3. Helios's MovieDetailsComplete fires — the hint that would fix the row.
    //    The async TMDB stage runs on `service`'s pool; `service.stop()` drains it.
    bus.subscribe(service.onMovieDetailsComplete)
    bus.publish(MovieDetailsComplete(Title, Year, originalTitle = None, director = Some(Director)))
    service.stop()

    // The director hint triggers a re-verify: "Itay Gordon" is not in 881487's
    // credits (Justin P. Lange is) → the row re-resolves via director-walk to
    // the correct film, 1026057.
    cache.get(key).flatMap(_.tmdbId) shouldBe Some(Correct)
  }

  // Prod 2026-09-05. TMDB's title search for "Vivaldi i ja" returned a
  // STABAT MATER concert short — 18 minutes — and nothing checked it against the
  // 110 minutes all 46 venues were advertising, so the row carried that short's
  // year, poster and ratings. Cinemas round and pad their runtimes, but nothing
  // in that noise turns 110 into 18.
  private val Concert = 1667002   // "STABAT MATER RV621 … Jakub Józef Orliński" (2023), 18 min
  private val Feature = 1200001   // the 110-minute feature the cinemas are screening

  private def vivaldiTmdb(featureRuntime: Int): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      "/search/movie" -> s"""{"results":[
        |{"id":$Concert,"title":"Vivaldi i ja","original_title":"Vivaldi i ja","release_date":"2023-04-01","popularity":9.0},
        |{"id":$Feature,"title":"Vivaldi i ja","original_title":"Vivaldi et moi","release_date":"2023-05-01","popularity":0.4}
        |]}""".stripMargin,
      s"/movie/$Concert?"  -> s"""{"id":$Concert,"title":"Vivaldi i ja","release_date":"2023-04-01","runtime":18}""",
      s"/movie/$Feature?"  -> s"""{"id":$Feature,"title":"Vivaldi i ja","release_date":"2023-05-01","runtime":$featureRuntime}""",
      s"/movie/$Concert/external_ids" -> s"""{"id":$Concert,"imdb_id":""}""",
      s"/movie/$Feature/external_ids" -> s"""{"id":$Feature,"imdb_id":""}"""
    )),
    apiKey = Some("stub")
  )

  private def resolveVivaldi(tmdb: TmdbClient): Option[Int] = {
    // Every venue advertises a feature-length running time.
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some("Vivaldi i ja"), runtimeMinutes = Some(110)),
      Helios                -> SourceData(title = Some("Vivaldi i ja"), runtimeMinutes = Some(112))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Vivaldi i ja", Some(2023), seed))),
      normalizer = titleNormalizer)
    new MovieService(cache, new InProcessEventBus(), tmdb).reEnrichSync("Vivaldi i ja", Some(2023))
    cache.get(cache.keyOf("Vivaldi i ja", Some(2023))).flatMap(_.tmdbId)
  }

  "a title match a fraction of the length the cinemas advertise" should "not be accepted" in {
    resolveVivaldi(vivaldiTmdb(featureRuntime = 110)) should not be Some(Concert)
  }

  it should "still resolve normally when the match's runtime is credible" in {
    // Same search, but the popular hit is now a real feature — nothing to veto.
    val tmdb = new TmdbClient(
      http = new StubFetch(Seq(
        "/search/movie" -> s"""{"results":[
          |{"id":$Concert,"title":"Vivaldi i ja","original_title":"Vivaldi i ja","release_date":"2023-04-01","popularity":9.0}
          |]}""".stripMargin,
        s"/movie/$Concert?" -> s"""{"id":$Concert,"title":"Vivaldi i ja","release_date":"2023-04-01","runtime":108}""",
        s"/movie/$Concert/external_ids" -> s"""{"id":$Concert,"imdb_id":""}"""
      )),
      apiKey = Some("stub"))
    resolveVivaldi(tmdb) shouldBe Some(Concert)
  }

  // S3: a conclusion now records the evidence it was reached on, so a guess from
  // a bare title stays distinguishable from an answer a director's filmography
  // confirmed. Without it every resolution looks equally settled, which is how
  // five wrong ones survived weeks in prod on rows that had since acquired the
  // very hints that would have corrected them.
  "a resolution" should "record that it was reached from a bare title alone" in {
    // A singleton title hit, no year and no director to narrow it — all a
    // deferred-detail cinema's first scrape can offer.
    val tmdb = new TmdbClient(
      http = new StubFetch(Seq(
        "/search/movie" -> s"""{"results":[{"id":$Concert,"title":"Vivaldi i ja","original_title":"Vivaldi i ja","release_date":"2023-04-01","popularity":9.0}]}""",
        s"/movie/$Concert?" -> s"""{"id":$Concert,"title":"Vivaldi i ja","release_date":"2023-04-01","runtime":108}""",
        s"/movie/$Concert/external_ids" -> s"""{"id":$Concert,"imdb_id":""}"""
      )),
      apiKey = Some("stub"))
    val cache = new CaffeineMovieCache(
      new InMemoryMovieRepository(Seq(("Vivaldi i ja", None,
        MovieRecord(data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some("Vivaldi i ja"))))))),
      normalizer = titleNormalizer)
    new MovieService(cache, new InProcessEventBus(), tmdb).reEnrichSync("Vivaldi i ja", None)

    // `settleResolved` has already re-keyed the yearless row onto the year of the
    // film it guessed — the very promotion of guess to identity this basis exists
    // to make visible — so read it back under that key.
    val settled = cache.get(cache.keyOf("Vivaldi i ja", Some(2023)))
    settled.flatMap(_.tmdbId)    shouldBe Some(Concert)
    settled.flatMap(_.tmdbBasis) shouldBe Some(services.resolution.TmdbBasis.TitleOnly.toString)
  }

  it should "record a director walk as the stronger basis it is" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(Title), director = Seq(Director))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq((Title, Year, seed))), normalizer = titleNormalizer)
    new MovieService(cache, new InProcessEventBus(), visitorTmdb()).reEnrichSync(Title, Year)

    val row = cache.get(cache.keyOf(Title, Year))
    row.flatMap(_.tmdbId)    shouldBe Some(Correct)
    row.flatMap(_.tmdbBasis) shouldBe Some(services.resolution.TmdbBasis.DirectorWalk.toString)
  }

  // The resolution-ID CACHE skips the search entirely on a hit, so the basis of the
  // id it hands back is not knowable at that point. Claiming the weakest one there
  // overwrites a recorded DirectorWalk with TitleOnly — which
  // `resolvedOnWeakerEvidenceThanAvailable` reads as "re-resolve me", and the
  // re-resolve hits the same cache and records TitleOnly again. That is a row
  // churning once per sweep for ever, so an unknown basis must stay unknown.
  /** Memoises like the production `WriteThroughResolutionCache`: a second call on the
   *  same hint key returns the stored id WITHOUT running the loader. The default
   *  `ResolutionCache.passthrough` resolves live every time, so a spec using it can
   *  never reach the branch that reads the basis of an id nothing just searched for. */
  private class MemoisingResolutionCache extends services.resolution.ResolutionCache {
    private val memo = scala.collection.mutable.Map.empty[String, Option[String]]
    var loaderRuns = 0
    def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] =
      memo.getOrElseUpdate(hintKey, { loaderRuns += 1; resolve })
  }

  it should "not downgrade a recorded basis when the id comes back off the cache" in {
    // Production wires a memoising resolution cache, so a hint key resolved once is
    // answered from memory ever after — the loader does not run, and the basis of that
    // id is not knowable at the point the row is written. Claiming the weakest one
    // there overwrites the DirectorWalk this row earned with TitleOnly, which
    // `resolvedOnWeakerEvidenceThanAvailable` reads as "re-resolve me"; the re-resolve
    // is answered from the same memory and records TitleOnly again, so the row churns
    // once per sweep for ever.
    val seed = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(Title), director = Seq(Director))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq((Title, Year, seed))), normalizer = titleNormalizer)
    val ids = new MemoisingResolutionCache
    val service = new MovieService(cache, new InProcessEventBus(), visitorTmdb(), tmdbIdCache = ids)

    service.reEnrichSync(Title, Year)
    cache.get(cache.keyOf(Title, Year)).flatMap(_.tmdbBasis) shouldBe
      Some(services.resolution.TmdbBasis.DirectorWalk.toString)

    // Strip the resolution, as a repair script does. That restores the original hint
    // key, so the re-resolve is answered from memory without searching.
    cache.putIfPresent(cache.keyOf(Title, Year), r =>
      r.copy(tmdbId = None, data = r.data.filterNot { case (src, _) => src == models.Tmdb }))
    service.reEnrichSync(Title, Year)

    val row = cache.get(cache.keyOf(Title, Year))
    withClue(s"loaderRuns=${ids.loaderRuns} (must be 1 — the second pass is a HIT): ")(
      ids.loaderRuns shouldBe 1)
    row.flatMap(_.tmdbId) shouldBe Some(Correct)
    withClue(s"basis=${row.flatMap(_.tmdbBasis)}: ")(
      row.flatMap(_.tmdbBasis) shouldBe Some(services.resolution.TmdbBasis.DirectorWalk.toString))
  }

  // S1': a key year stamped from a TitleOnly guess is not evidence — it IS the
  // guess, handed back to the next resolution as if it were a fact. That loop is
  // what made prod's mis-resolutions self-confirming: `homosapiens|1960` was keyed
  // 1960 because a title-only search picked a 1960 short, and every later attempt
  // then searched 1960 and found the same short, while twelve venues published
  // 2025. A row whose conclusion was a guess must fall through to what the CINEMAS
  // published instead.
  "a row whose key year came from a title-only guess" should
    "re-resolve on the cinemas' year, not on its own stamped one" in {
    val Short   = 891655    // the 9-minute 1960 short a title-only search found
    val Feature = 1200002   // the 2025 film twelve venues are actually screening
    val tmdb = new TmdbClient(
      http = new StubFetch(Seq(
        // Year-scoped searches: 1960 still finds the short, 2025 finds the feature.
        "&year=1960" ->
          s"""{"results":[{"id":$Short,"title":"Homo sapiens","release_date":"1960-01-01","popularity":5.0}]}""",
        "&year=2025" ->
          s"""{"results":[{"id":$Feature,"title":"Homo sapiens","release_date":"2025-01-01","popularity":1.0}]}""",
        s"/movie/$Short?"   -> s"""{"id":$Short,"title":"Homo sapiens","release_date":"1960-01-01","runtime":9}""",
        s"/movie/$Feature?" -> s"""{"id":$Feature,"title":"Homo sapiens","release_date":"2025-01-01","runtime":95}""",
        s"/movie/$Short/external_ids"   -> s"""{"id":$Short,"imdb_id":""}""",
        s"/movie/$Feature/external_ids" -> s"""{"id":$Feature,"imdb_id":""}"""
      )),
      apiKey = Some("stub"))

    // The row as prod held it: keyed 1960 off a TitleOnly conclusion, with every
    // venue publishing 2025 and a feature-length runtime.
    val seed = MovieRecord(
      tmdbId = Some(Short), tmdbBasis = Some(services.resolution.TmdbBasis.TitleOnly.toString),
      data = Map[Source, SourceData](
        CinemaCityPoznanPlaza -> SourceData(title = Some("Homo sapiens"), releaseYear = Some(2025), runtimeMinutes = Some(95))))
    val cache = new CaffeineMovieCache(
      new InMemoryMovieRepository(Seq(("Homo sapiens", Some(1960), seed))), normalizer = titleNormalizer)

    new MovieService(cache, new InProcessEventBus(), tmdb).reEnrichSync("Homo sapiens", Some(1960))

    withClue("the stamped 1960 must not be fed back in as if it were evidence: ") {
      cache.entries.flatMap(_._2.tmdbId).toSet should contain(Feature)
    }
  }
}
