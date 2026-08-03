package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.HeliosClient
import services.movies.SingleCountryNormalizer.titleNormalizer

class HeliosClientRestEnrichmentRecordedSpec extends AnyFlatSpec with Matchers {

  private val client =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment"), titles = titleNormalizer)

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

  // Helios ships `genres: [{id, name, description}]` with lowercase Polish
  // labels — "animowany", "dramat", "science fiction". The client
  // title-cases them at the write boundary so display matches TMDB/Filmweb
  // spelling.

  it should "lift originalTitle from the REST movie response" in {
    val byTitle = fetch().map(m => m.movie.title -> m).toMap
    byTitle.get("Projekt Hail Mary").flatMap(_.movie.originalTitle) shouldBe Some("Project Hail Mary")
  }

  // Helios ships the Polish age certificate as `ratings: [{symbol, value,
  // description}]` on the movie DETAIL body — `symbol`/`value` the clean short
  // form ("15", "7", "0"), `description` the verbose "od lat 15" / "b.o.". The
  // client lifts the first rating's clean symbol onto `CinemaMovie.ageRating`.

  it should "lift the clean age-rating symbol from the REST movie details" in {
    val byTitle = fetch().map(m => m.movie.title -> m).toMap
    // "Projekt Hail Mary" carries ratings[].symbol == "15" in its fixture.
    byTitle.get("Projekt Hail Mary").flatMap(_.ageRating) shouldBe Some("15")
  }

  it should "map the no-restriction marker (symbol 0 / b.o.) to no age rating" in {
    val byTitle = fetch().map(m => m.movie.title -> m).toMap
    // "Pucio" carries ratings[].symbol == "0" / description == "b.o." — bez
    // ograniczeń — which must collapse to None rather than showing "0".
    val pucio = byTitle.get("Pucio")
    pucio should not be empty
    pucio.flatMap(_.ageRating) shouldBe None
    // No row anywhere should surface the raw no-restriction sentinel.
    fetch().flatMap(_.ageRating) should not contain "0"
  }

  it should "extract Polish genre labels from REST movie details, title-cased" in {
    val byTitle = fetch().map(m => m.movie.title -> m).toMap
    val kurozajac = byTitle.get("Kurozając i Świątynia Świstaka")
    kurozajac.map(_.movie.genres) shouldBe Some(Seq("Animowany"))
    // Every enriched row that had a `genres` array in its fixture should
    // surface at least one title-cased label.
    val withGenres = fetch().filter(_.movie.genres.nonEmpty)
    withGenres should not be empty
    withGenres.foreach { m =>
      m.movie.genres.foreach { g =>
        g.headOption.exists(_.isUpper) shouldBe true
      }
    }
  }
}