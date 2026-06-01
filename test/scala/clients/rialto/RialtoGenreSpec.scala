package clients.rialto

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.RialtoClient

import java.nio.file.{Files, Paths}

/**
 * Rialto's per-film event page heads the description with a genre+runtime line:
 *   <p class="movie-parameters">Dramat, Komedia | 120 min</p>
 *   <p class="movie-parameters">Animowany | 55 min</p>
 * The genre is absent from the repertoire listing, so it's parsed off the event
 * page the client already fetches for showtimes. The recorded `wydarzenie`
 * fixture carries the multi-genre "Dramat, Komedia" form.
 */
class RialtoGenreSpec extends AnyFlatSpec with Matchers {

  private val client = new RialtoClient(new FakeHttpFetch("rialto"))

  private val recordedEventPage: String =
    new String(Files.readAllBytes(
      Paths.get("test/resources/fixtures/rialto/www.kinorialto.poznan.pl/wydarzenie")))

  "RialtoClient.parseGenres" should "split the comma-separated list from the recorded event page" in {
    client.parseGenres(recordedEventPage) shouldBe Seq("Dramat", "Komedia")
  }

  it should "split a space-separated genre list (the form Rialto now serves)" in {
    client.parseGenres(
      """<p class="movie-parameters">Przygodowy Horror Sci-Fi | Od lat 12 | 99 min</p>"""
    ) shouldBe Seq("Przygodowy", "Horror", "Sci-Fi")
  }

  it should "read a single genre before the runtime pipe" in {
    client.parseGenres("""<p class="movie-parameters">Animowany | 55 min</p>""") shouldBe Seq("Animowany")
  }

  it should "not mistake the age-rating segment for a genre when the film has none" in {
    client.parseGenres("""<p class="movie-parameters">Od lat 12 | 99 min</p>""") shouldBe empty
  }

  it should "ignore a duration-only line with no genre" in {
    client.parseGenres("""<p class="movie-parameters">90 min</p>""") shouldBe empty
  }

  it should "return empty when the marker is absent" in {
    client.parseGenres("<p>no parameters here</p>") shouldBe empty
  }
}
