package services.movies

import clients.TmdbClient
import models.{KinoMuza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

/**
 * Regression: "The Square" (Kino Muza, festival entry, director "Bo-sol Kim",
 * reported year 2025) kept a `ResolveTmdb` queue task retrying forever — 10+
 * "failed transiently; retrying" tries over an hour against an exception that
 * never clears.
 *
 * Captured from the live data: Bo-sol Kim's TMDB filmography has a bare stub
 * "The Square" (id 1651289) carrying NO `release_date`, alongside the fully
 * populated duplicate "Square"/"광장" (id 1361054, 2026). The director-walk picks
 * the stub because its title matches the cinema's exactly, then
 * `collapseDirectorDuplicate` ran `credits.filter(…).map(_.id).min` — and the
 * filter is EMPTY whenever the resolved film has no year (its own
 * `resolved.releaseYear.exists(…)` is false), so `.min` threw
 * `UnsupportedOperationException: empty.min`. The resolve caught that as a
 * "transient" failure and the queue re-dispatched it indefinitely.
 *
 * A resolved film with no TMDB release year must therefore conclude (keep its
 * own id, nothing to collapse), not crash the whole resolve.
 */
class DirectorWalkNoReleaseYearSpec extends AnyFlatSpec with Matchers {

  private val Title    = "The Square"
  private val Year     = Some(2025)     // production/festival year the cinema reports
  private val Stub     = 1651289        // TMDB stub: matching title, NO release_date
  private val Director = "Bo-sol Kim"
  private val PersonId = 3587863

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  private def squareTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      // Title search finds nothing useful — resolution comes from the director walk.
      "/search/movie" -> """{"results":[]}""",
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Kim Bo-sol","known_for_department":"Directing"}]}""",
      // The matching credit carries NO release_date → releaseYear = None, which is
      // what tripped `collapseDirectorDuplicate`'s `.min` on an empty filter.
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":$Stub,"title":"The Square","original_title":"The Square","department":"Directing"}
        |]}""".stripMargin,
      s"/movie/$Stub/external_ids" -> s"""{"id":$Stub,"imdb_id":""}""",
      s"/movie/$Stub/images" -> """{"posters":[]}""",
      s"/movie/$Stub?" -> s"""{"id":$Stub,"title":"The Square","original_title":"The Square",
        |"credits":{"crew":[{"id":$PersonId,"name":"Kim Bo-sol","job":"Director"}]}}""".stripMargin
    )),
    apiKey = Some("stub")
  )

  "a director-walk hit whose TMDB credit has no release year" should
    "resolve (keep its id) instead of crashing the whole resolve as a transient failure" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      KinoMuza -> SourceData(title = Some(Title), director = Seq(Director), releaseYear = Some(2025))))
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache = new CaffeineMovieCache(repository)
    val bus   = new services.events.InProcessEventBus()
    val service = new MovieService(cache, bus, squareTmdb())

    service.reEnrichSync(Title, Year)

    cache.get(cache.keyOf(Title, Year)).flatMap(_.tmdbId) shouldBe Some(Stub)
  }
}
