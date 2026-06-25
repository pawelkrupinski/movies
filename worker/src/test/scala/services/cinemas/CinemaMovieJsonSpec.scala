package services.cinemas

import models.{Movie, Multikino, CinemaMovie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

  it should "handle an empty slice" in {
    CinemaMovieJson.decode(CinemaMovieJson.encode(Nil), Multikino) shouldBe empty
  }
}
