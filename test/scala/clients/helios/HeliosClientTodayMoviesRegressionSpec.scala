package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosClient

import java.time.LocalDate

class HeliosClientTodayMoviesRegressionSpec extends AnyFlatSpec with Matchers {

  private val client =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment"))

  // The fixture was captured on 2026-05-13 — pin "today" to that so the
  // assertions stay deterministic. Using `LocalDate.now()` against a recorded
  // fixture causes the expected set to drift every day.
  private val today = LocalDate.of(2026, 5, 13)

  private def fetchTodayTitles(): Seq[String] =
    client
      .fetch()
      .filter(_.showtimes.exists(_.dateTime.toLocalDate == today))
      .map(_.movie.title)

    "HeliosClient.fetch" should "return the exact expected movie titles for today from the recorded fixture" in {

      fetchTodayTitles().toSet shouldBe Set(
        "Billie Eilish - Hit Me Hard and Soft: The Tour Live",
        "Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D",
        "Diabeł ubiera się u Prady 2",
        "Drama",
        "Michael",
        "Mortal Kombat II",
        "Mumia: Film Lee Cronina",
        "Nawet myszy idą do nieba",
        "Odrodzony jako galareta. Film: Łzy Morza Lazurowego",
        "Projekt Hail Mary",
        "Pucio",
        "Sprawiedliwość owiec",
        "Super Mario Galaxy Film",
        "Top Gun 40. Rocznica",
        "Top Gun: Maverick"
      )
    }

  it should "include all expected movies for today from the recorded Helios fixture" in {
    val titles = fetchTodayTitles().toSet

    titles should contain ("Top Gun 40. Rocznica")
  }

  it should "not return an empty today repertoire" in {
    val titles = fetchTodayTitles()

    titles should not be empty
  }

  it should "assign the same poster to both Billie Eilish events and the base Tour entry" in {
    val results = client.fetch()

    // Mirror helios.pl/.../repertuar: variants of the same film share the parent poster.
    val billie3d  = results.find(_.movie.title == "Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D")
    val billieHns = results.find(_.movie.title == "Billie Eilish - Hit Me Hard and Soft: The Tour Live")
    val tour      = results.find(_.movie.title == "Billie Eilish - Hit Me Hard and Soft: The Tour")

    val expected = Some("https://img.helios.pl/pliki/film/billie-eilish-hit-me-hard-and-soft-the-tour/billie-eilish-hit-me-hard-and-soft-the-tour-plakat-28207.png")
    billie3d.get.posterUrl  shouldBe expected
    billieHns.get.posterUrl shouldBe expected
    tour.get.posterUrl      shouldBe expected
  }
}