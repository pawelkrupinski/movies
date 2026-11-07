package controllers

import models.{Helios, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.time.LocalDate

/** The repertoire API serves each genre's master-set id beside the display names, which stay as they were. */
class ApiFilmGenreIdsSpec extends AnyFlatSpec with Matchers {

  private def film(genres: Seq[String]): FilmSchedule =
    FilmSchedules.of(Movie("Bing", runtimeMinutes = None, releaseYear = Some(2025), genres = genres), LocalDate.of(2026, 11, 6),
      cinemaFilmUrls = Seq(Helios -> "https://helios.pl/film/bing"))

  "the repertoire film" should "serve the names unchanged and the ids of those the master set knows" in {
    val json = Json.toJson(ApiFilm.from(film(Seq("Przygodowy", "Familijny", "Spotkanie z twórcami")), java.util.Locale.forLanguageTag("pl")))
    (json \ "genres").as[Seq[String]] shouldBe Seq("Przygodowy", "Familijny", "Spotkanie z twórcami")
    (json \ "genreIds").as[Seq[String]] shouldBe Seq("adventure", "family")
  }
}
