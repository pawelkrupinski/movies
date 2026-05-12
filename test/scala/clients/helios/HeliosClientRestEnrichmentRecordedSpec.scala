package clients.helios

import clients.HeliosClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeliosClientRestEnrichmentRecordedSpec extends AnyFlatSpec with Matchers {

  private val client =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment"))

  private def fetch() =
    client.fetch()

  "HeliosClient.fetch" should "use REST movie metadata to enrich NUXT movies" in {
    val result = fetch()

    result should not be empty
    result.flatMap(_.showtimes) should not be empty

    val enrichedMovies =
      result.filter(movie =>
        movie.movie.runtimeMinutes.nonEmpty ||
          movie.synopsis.nonEmpty ||
          movie.cast.nonEmpty ||
          movie.director.nonEmpty ||
          movie.movie.releaseYear.nonEmpty
      )

    enrichedMovies should not be empty
  }

  it should "use REST screening metadata to enrich NUXT showtimes" in {
    val showtimes =
      fetch().flatMap(_.showtimes)

    showtimes should not be empty

    val restEnrichedShowtimes =
      showtimes.filter(st => st.room.nonEmpty || st.format.nonEmpty)

    restEnrichedShowtimes should not be empty
  }

  it should "use REST poster details when available" in {
    val result = fetch()

    val moviesWithRestLikePosters =
      result.filter(_.posterUrl.exists(_.startsWith("https://movies.helios.pl/")))

    moviesWithRestLikePosters should not be empty
  }
}