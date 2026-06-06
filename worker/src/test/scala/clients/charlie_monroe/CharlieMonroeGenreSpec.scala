package clients.charlie_monroe

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CharlieMonroeClient

/**
 * Charlie Monroe / Kino Malta lists each film's genre on the listing card's
 * `div.meta` line, ahead of the runtime: "Czarna komedia, Thriller / 139 min".
 * `parseMetaGenres` isolates the genre list (left of the ` / `) from the
 * runtime; the end-to-end flow is covered in CharlieMonroeClientSpec.
 */
class CharlieMonroeGenreSpec extends AnyFlatSpec with Matchers {

  private val client = new CharlieMonroeClient(new FakeHttpFetch("charlie-monroe"))

  "CharlieMonroeClient.parseMetaGenres" should "split a comma-separated list ahead of the runtime" in {
    client.parseMetaGenres("Czarna komedia, Thriller / 139 min") shouldBe Seq("Czarna komedia", "Thriller")
  }

  it should "read a single genre" in {
    client.parseMetaGenres("Dokumentalny / 66 min") shouldBe Seq("Dokumentalny")
  }

  it should "drop the runtime when a card carries no genre" in {
    client.parseMetaGenres("79 min") shouldBe empty
  }

  it should "return empty for a blank meta line" in {
    client.parseMetaGenres("") shouldBe empty
  }
}
