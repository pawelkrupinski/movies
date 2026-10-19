package clients.dom_kultury_lapy

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.DomKulturyLapy
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.DomKulturyLapyClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `dklapy.pl/kino/` listing (a WordPress page that embeds
 *  the full body of the venue's "Premiery kinowe" posts inline) through the
 *  client, captured 2026-09-23. */
class LapyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new DomKulturyLapyClient(new FakeHttpFetch("dom-kultury-lapy"), today = LocalDate.of(2026, 9, 23)).fetch()

  "DomKulturyLapyClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(DomKulturyLapy)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening with the date+time read off the h5 heading, year inferred from today" in {
    val film = movies.find(_.movie.title == "Lalka").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 1, 18, 45))
  }

  it should "parse the runtime, production country/year and genres off the meta line" in {
    val film = movies.find(_.movie.title == "100 Dni: Misja Zeus").value
    film.movie.runtimeMinutes.value shouldBe 112 // "1g. 52m."
    film.movie.countries shouldBe Seq("Polska")
    film.movie.releaseYear.value shouldBe 2026
    film.movie.genres shouldBe Seq("komedia", "akcja")
  }

  it should "exclude the unrelated 'Film Konesera' series, whose titles are withheld" in {
    movies.map(_.movie.title) should not contain "Tytuł filmu dostępny w domu kultury"
  }
}
