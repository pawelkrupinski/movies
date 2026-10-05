package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.nio.file.{Files, Paths}

class TmdbFilmRecordSpec extends AnyFlatSpec with Matchers {

  private def recorded(name: String) =
    Json.parse(Files.readString(Paths.get(s"test/resources/fixtures/tmdb/$name")))

  "A TMDB film record" should "name its co-directors among its directors" in {
    // "The Last Whale Singer" (Vincent. Legenda oceanu): TMDB credits Reza Memari as Director and
    // Pavel Hrubos and Steven Majaury as Co-Director. Venues that name the co-directors scored
    // "director different" against the film, which vetoed 51 PL listings and split the film.
    val (film, _) = TmdbFilmRecord.parse(Seq(recorded("movie-677558-credits-pl.json"))).get
    film.directors.get should contain theSameElementsAs Seq("Reza Memari", "Pavel Hrubos", "Steven Majaury")
  }

  it should "name a filmed stage production's stage director among its directors" in {
    // "Fallen Angels by Noël Coward" (US ×365, UK ×287): the venues credit Scott Ellis, TMDB's
    // Stage Director; its Director is Annette Jolles, who directed the filming.
    val (film, _) = TmdbFilmRecord.parse(Seq(recorded("movie-1702350-credits-en.json"))).get
    film.directors.get should contain allOf ("Annette Jolles", "Scott Ellis")
  }

  it should "carry the day it was released, a broadcast's air date" in {
    val (film, _) = TmdbFilmRecord.parse(Seq(recorded("movie-1702350-credits-en.json"))).get
    film.released shouldBe Some(java.time.LocalDate.of(2026, 10, 22))
    film.year shouldBe Some(2026)
  }
}
