package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosClient

// Events listed under /wydarzenie/... carry their own posterPhoto in the NUXT
// repertoire payload — independent of any parent /filmy/ entry. The Radomiak –
// Lech Poznan football broadcast has no parent film, so the only place to pick
// up its poster is the event entry itself.
class HeliosClientEventPosterSpec extends AnyFlatSpec with Matchers {

  private val fakeHttp = new FakeHttpFetch("helios/event-radomiak")
  private val client   = new HeliosClient(fakeHttp)

  "HeliosClient.fetch" should "expose the Radomiak event with showtimes" in {
    val event = client.fetch().find(_.movie.title.contains("Radomiak"))
    event                       shouldBe defined
    event.get.showtimes         should not be empty
  }

  it should "extract the event's own posterPhoto for the Radomiak broadcast" in {
    val event = client.fetch().find(_.movie.title.contains("Radomiak"))
    event.flatMap(_.posterUrl) shouldBe Some(
      "https://img.helios.pl/pliki/wydarzenie/pko-bp-ekstraklasa-radomiak-radom-lech-poznan/pko-bp-ekstraklasa-radomiak-radom-lech-poznan-plakat-59779.jpg"
    )
  }
}
