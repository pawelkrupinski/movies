package clients.helios

import clients.HeliosClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, ZoneId}

class HeliosClientTodayMoviesRegressionSpec extends AnyFlatSpec with Matchers {

  private val client =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment"))

  private val today =
    LocalDate.now(ZoneId.of("Europe/Warsaw"))

  private def fetchTodayTitles(): Seq[String] =
    client
      .fetch()
      .filter(_.showtimes.exists(_.dateTime.toLocalDate == today))
      .map(_.movie.title)

    "HeliosClient.fetch" should "return the exact expected movie titles for today from the recorded fixture" in {

      fetchTodayTitles().toSet shouldBe Set(
        "Billie Eilish - Hit Me Hard and Soft: The Tour Live w HnS",
        "Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D w HnS",
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

  it should "not assign the same poster to both Billie Eilish events" in {
    val results = client.fetch()

    // Both events and the base "The Tour" film entry all share the same poster URL.
    // uniquePosterUrls awards it to "The Tour" (alphabetically first); both events end up with None.
    val billie3d = results.find(_.movie.title == "Billie Eilish - Hit Me Hard and Soft: The Tour Live in 3D w HnS")
    val billieHns = results.find(_.movie.title == "Billie Eilish - Hit Me Hard and Soft: The Tour Live w HnS")

    billie3d  shouldBe defined
    billieHns shouldBe defined
    billie3d.get.posterUrl  shouldBe None
    billieHns.get.posterUrl shouldBe None
  }
}