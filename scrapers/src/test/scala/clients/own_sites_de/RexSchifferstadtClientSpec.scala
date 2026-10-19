package clients.own_sites_de

import clients.tools.FakeHttpFetch
import models.{Cinema, GermanRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.de.RexSchifferstadtClient

import java.time.{LocalDate, LocalDateTime}

/** Replays rex-schifferstadt.de's home page as recorded 2026-10-12: the programme week
 *  Thursday 8 to Wednesday 14 October, one table per film, its days yearless. */
class RexSchifferstadtClientSpec extends AnyFlatSpec with Matchers with OptionValues {
  private val rex: Cinema = GermanRoster.theaterIdByCinema.collectFirst { case (c, "A1072") => c }.value

  private def fetch(today: LocalDate) = new RexSchifferstadtClient(
    new FakeHttpFetch("rex-schifferstadt", strict = true), "https://rex-schifferstadt.de/", rex, today
  ).fetch()

  private val movies = fetch(LocalDate.of(2026, 10, 8))

  "RexSchifferstadtClient" should "read each film's times off its own week table" in {
    movies.map(_.movie.title).toSet shouldBe Set(
      "VATERLAND", "SHAUN DAS SCHAF - Spuk im Kürbisfeld", "EMIL UND DIE DETEKTIVE", "DAS GEWISSE ETWAS",
      "COUSCOUS und Geheimnisse")
    // Its one cell with a time is the last column, MI 14.10.
    movies.find(_.movie.title == "VATERLAND").value.showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 10, 14, 19, 30))
    movies.map(_.cinema).toSet shouldBe Set(rex)
  }

  it should "place every yearless day in the programme week" in {
    movies.find(_.movie.title == "COUSCOUS und Geheimnisse").value.showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 10, 10, 20, 0))
    movies.flatMap(_.showtimes) should have size 5
  }

  it should "drop the week's days already past" in {
    fetch(LocalDate.of(2026, 10, 12)).map(_.movie.title) should not contain "COUSCOUS und Geheimnisse"
  }
}
