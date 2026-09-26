package services.movies

import clients.TmdbClient
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.RoutingHttpFetch

/**
 * A candidate film the venue's own facts deny is not the venue's film, however it was found.
 *
 * PL, 2026-09-25: Kinoteka lists Wong Kar Wai's "Happy Together" (1997) for a 2026 screening,
 * 96 minutes, director Wong Kar Wai. TMDB's title search named nothing, and the IMDb recovery
 * — a yearless rung — bound the title to tt9282472, Kim Jeong-hwan's 2018 "Happy Together";
 * TMDB found that id and the row resolved to it. Its year AND its director both deny what the
 * venue published (`MixedFilmDetector.deniesFilm`), which is the one combination a same-titled
 * different film always has and the venue's own film never does.
 */
class DeniedCandidateSpec extends AnyFlatSpec with Matchers {

  private val Kim = 551655

  "a resolve" should "refuse a film whose year and director both deny the venue's own listing" in {
    val tmdb = new TmdbClient(http = RoutingHttpFetch.getOnly(Map(
      "/find/tt9282472"          -> s"""{"movie_results":[{"id":$Kim,"title":"Happy Together","original_title":"Happy Together","release_date":"2018-11-21"}]}""",
      s"/movie/$Kim/external_ids" -> s"""{"id":$Kim,"imdb_id":"tt9282472"}""",
      s"/movie/$Kim/credits"      -> """{"crew":[{"job":"Director","name":"Kim Jeong-hwan"}],"cast":[]}""",
      s"/movie/$Kim?"             -> s"""{"id":$Kim,"title":"Happy Together","original_title":"Happy Together","release_date":"2018-11-21","runtime":110,
                                       |"credits":{"crew":[{"job":"Director","name":"Kim Jeong-hwan"}],"cast":[]}}""".stripMargin,
      "/search/movie"            -> """{"results":[]}""",
      "/search/person"           -> """{"results":[]}"""
    )), apiKey = Some(settings.TmdbApiKey("stub")))
    val service = new MovieService(new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer),
      normalizer = titleNormalizer), new InProcessEventBus(), tmdb)

    val existing = MovieRecord(imdbId = Some("tt9282472"), data = Map[Source, SourceData](
      Kinoteka -> SourceData(title = Some("Happy Together"), originalTitle = Some("Happy Together"),
        releaseYear = Some(2026), director = Seq("Wong Kar Wai"), runtimeMinutes = Some(96))))
    val resolved = service.resolveStagingRecord("Happy Together", None, existing)
    service.stop()

    resolved.flatMap(_.tmdbId) should not be Some(Kim)
  }
}
