package services.movies

import clients.TmdbClient
import models.{KinoMuza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

/**
 * Regression: "Mi Amor" (Kino Muza, reports director "Guillaume Nicloux",
 * release year 2025) went UNRESOLVED against TMDB.
 *
 * Captured from the live data:
 *   - A title search for "Mi amor" returns four same-title films; the closest-to-
 *     2025 hit is tmdb 1302640 — a DIFFERENT film (directors Christian Luna /
 *     Diego Alejandro Sierra), so `verifyByDirector` correctly rejects it.
 *   - The film the cinema actually shows is Nicloux's "Mi Amor" = tmdb 1432817,
 *     which TMDB dates 2026-05-06 (year 2026). The cinema reports 2025 (the
 *     production year). Filmography years routinely drift ±1 from a cinema's
 *     reported year (production vs first-release).
 *
 * `directorWalk` used to require an EXACT year match against the director's
 * filmography, so it found no 2025 Nicloux film and abstained. The fix lets the
 * walk pick the director's credit whose title matches the cinema's title even
 * when the year is off by one — landing on 1432817.
 */
class DirectorWalkYearDriftSpec extends AnyFlatSpec with Matchers {

  private val Title    = "Mi Amor"
  private val Year     = Some(2025)            // production year the cinema reports
  private val Decoy    = 1302640               // a different "Mi amor", 2025, not Nicloux
  private val Correct  = 1432817               // Nicloux's "Mi Amor", TMDB-dated 2026
  private val Director = "Guillaume Nicloux"
  private val PersonId = 17623

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  private def miAmorTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      // The title search lands on the wrong same-year "Mi amor" (1302640).
      "/search/movie" -> s"""{"results":[
        |{"id":$Decoy,"title":"Mi amor","original_title":"Mi amor","release_date":"2025-01-02","popularity":0.097}
        |]}""".stripMargin,
      // Decoy credits: not Nicloux → verifyByDirector rejects it.
      s"/movie/$Decoy/credits" -> """{"crew":[{"id":1,"name":"Christian Luna","job":"Director"}]}""",
      // Director-walk recovery for "Guillaume Nicloux": his "Mi Amor" is dated
      // 2026, plus two 2024 films — so an exact-2025 match finds nothing and a
      // naive ±1 window is ambiguous; only the title disambiguates.
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Guillaume Nicloux","known_for_department":"Directing"}]}""",
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":2,"title":"Sarah Bernhardt","release_date":"2024-12-18","department":"Directing"},
        |{"id":$Correct,"title":"Mi Amor","release_date":"2026-05-06","department":"Directing"}
        |]}""".stripMargin,
      s"/movie/$Correct/external_ids" -> s"""{"id":$Correct,"imdb_id":""}"""
    )),
    apiKey = Some("stub")
  )

  "a film whose TMDB release year drifts from the cinema's production year" should
    "still resolve via the director's filmography by title" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      KinoMuza -> SourceData(title = Some(Title), director = Seq(Director), releaseYear = Some(2025))))
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache = new CaffeineMovieCache(repository)
    val bus   = new services.events.InProcessEventBus()
    val service = new MovieService(cache, bus, miAmorTmdb())

    service.reEnrichSync(Title, Year)

    cache.get(cache.keyOf(Title, Year)).flatMap(_.tmdbId) shouldBe Some(Correct)
  }
}
