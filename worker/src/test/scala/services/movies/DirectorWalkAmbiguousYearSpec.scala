package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

/**
 * Regression: "Istoriya Igrashok 5 - UA" (Helios, Ukrainian-dubbed "Toy Story 5",
 * director "Andrew Stanton", year 2026) bound to the WRONG film's ratings.
 *
 * Captured from prod (kinowo.fly.dev/poznan/film?title=Istoriya%20Igrashok%205%20-%20UA):
 *   - The transliterated Ukrainian title has no TMDB/IMDb entry, so the title
 *     search returns nothing and `directorWalk` is the only path that can fire.
 *   - Andrew Stanton has TWO 2026 directing credits: "In the Blink of an Eye"
 *     (2026-01-26) and "Toy Story 5" (2026-06-17). Neither TMDB title matches the
 *     cinema's "Istoriya Igrashok 5", so the title match abstains and the walk
 *     fell back to a YEAR-ONLY match — `credits.find(_.releaseYear.contains(2026))`
 *     returns the FIRST 2026 credit, "In the Blink of an Eye". The row was bound to
 *     that film's tmdbId/imdbId, so the page showed its ratings (IMDb 6.1, MC 37,
 *     Filmweb 5.6) under a Toy Story 5 listing.
 *
 * A year-only fallback can only disambiguate when the year is UNIQUE within the
 * director's filmography. With two same-year credits it's a coin-flip — refuse to
 * guess rather than bind the wrong film's ratings.
 */
class DirectorWalkAmbiguousYearSpec extends AnyFlatSpec with Matchers {

  private val Title     = "Istoriya Igrashok 5 - UA"
  private val Year      = Some(2026)
  private val Director  = "Andrew Stanton"
  private val PersonId  = 7                       // Andrew Stanton
  private val Decoy     = 1032892                 // "In the Blink of an Eye", 2026-01-26
  private val ToyStory5 = 1022789                 // "Toy Story 5", 2026-06-17

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  private def toyStoryTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      // The transliterated Ukrainian title finds nothing on TMDB.
      "/search/movie" -> """{"results":[]}""",
      // Director-walk recovery for "Andrew Stanton": two 2026 credits, neither
      // titled to match the cinema's "Istoriya Igrashok 5".
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Andrew Stanton","known_for_department":"Directing"}]}""",
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":$Decoy,"title":"In the Blink of an Eye","release_date":"2026-01-26","department":"Directing"},
        |{"id":$ToyStory5,"title":"Toy Story 5","release_date":"2026-06-17","department":"Directing"}
        |]}""".stripMargin
    )),
    apiKey = Some("stub")
  )

  "director-walk over a director with two same-year credits and no title match" should
    "refuse to bind a film rather than guess the first one" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(Title), director = Seq(Director), releaseYear = Some(2026))))
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache = new CaffeineMovieCache(repository)
    val bus   = new services.events.InProcessEventBus()
    val service = new MovieService(cache, bus, toyStoryTmdb())

    service.reEnrichSync(Title, Year)

    cache.get(cache.keyOf(Title, Year)).flatMap(_.tmdbId) shouldBe None
  }
}
