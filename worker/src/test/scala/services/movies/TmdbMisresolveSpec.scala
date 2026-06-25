package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import tools.GetOnlyHttpFetch

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
    val cache = new CaffeineMovieCache(repository)
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
}
