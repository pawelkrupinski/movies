package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models.{Movie, Multikino, CinemaMovie, Showtime}
import services.cinemas.common.CinemaMovieJson

import java.time.LocalDateTime

class CinemaMovieJsonSpec extends AnyFlatSpec with Matchers {

  "CinemaMovieJson" should "round-trip a chunk slice, re-attaching the cinema on decode" in {
    val movies = Seq(
      CinemaMovie(
        movie = Movie("Diuna", runtimeMinutes = Some(166), releaseYear = Some(2024),
          countries = Seq("USA"), genres = Seq("Sci-Fi"), originalTitle = Some("Dune"), rawTitle = Some("Diuna 2D")),
        cinema = Multikino, posterUrl = Some("https://p/dune.jpg"), filmUrl = Some("https://x/film/diuna"),
        synopsis = Some("Paul..."), cast = Seq("Timothée Chalamet"), director = Seq("Denis Villeneuve"),
        showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 25, 18, 0), Some("https://book/1"), Some("Sala 3"), List("2D", "NAP"))),
        externalIds = Map("tmdb" -> "693134"), trailerUrl = Some("https://yt/abc")),
      CinemaMovie(Movie("Wicked"), Multikino, None, None, None, Nil, Nil,
        Seq(Showtime(LocalDateTime.of(2026, 6, 26, 20, 30), None)), Map.empty, None))

    CinemaMovieJson.decode(CinemaMovieJson.encode(movies), Multikino) shouldBe movies
  }

  // `ageRating` was absent from the transport mirror, so every CHUNKED cinema
  // silently lost its certificate on the reduce round-trip while the identical
  // non-chunked cinema kept it — and the UK chains that carry BBFC certificates
  // (Cineworld, Odeon, Vue) are exactly the ones scraped in chunks.
  it should "preserve the age rating across the chunk round-trip" in {
    val movies = Seq(
      CinemaMovie(Movie("Diuna"), Multikino, None, None, None, Nil, Nil,
        Seq(Showtime(LocalDateTime.of(2026, 6, 25, 18, 0), None)), Map.empty, None, ageRating = Some("15")),
      CinemaMovie(Movie("Wicked"), Multikino, None, None, None, Nil, Nil,
        Seq(Showtime(LocalDateTime.of(2026, 6, 26, 20, 30), None)), Map.empty, None, ageRating = None))

    val decoded = CinemaMovieJson.decode(CinemaMovieJson.encode(movies), Multikino)
    decoded.map(_.ageRating) shouldBe Seq(Some("15"), None)
    decoded                  shouldBe movies
  }

  it should "handle an empty slice" in {
    CinemaMovieJson.decode(CinemaMovieJson.encode(Nil), Multikino) shouldBe empty
  }

  // A row written before `ageRating` joined the transport shape has no such key;
  // an in-flight chunk from the previous deploy must still decode.
  it should "decode a legacy slice that predates the age-rating field" in {
    val legacy = """[{"movie":{"title":"Diuna","countries":[],"genres":[]},"cast":[],"director":[],
                    |"showtimes":[{"dateTime":"2026-06-25T18:00:00","format":[]}],"externalIds":{}}]""".stripMargin

    CinemaMovieJson.decode(legacy, Multikino).map(_.ageRating) shouldBe Seq(None)
  }
}
