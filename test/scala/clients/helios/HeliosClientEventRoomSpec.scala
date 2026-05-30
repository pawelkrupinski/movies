package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosClient

// Event screenings ("... w Helios Anime", concerts, sports broadcasts) are
// excluded from the REST `/screening` endpoint entirely, so room enrichment by
// the shared screening UUID found nothing and left `room = None`. The room is
// available from the `/api/cinema/{id}/event` endpoint, which lists each event
// screening with its screeningId + screenId — resolvable to a hall name via the
// existing `/screen/{id}` lookup. Regression for the "All You Need Is Kill"
// anime event, which rendered without a room.
class HeliosClientEventRoomSpec extends AnyFlatSpec with Matchers {

  private val client = new HeliosClient(new FakeHttpFetch("helios/event-room"))

  "HeliosClient.fetch" should "resolve the room for an event screening absent from /screening" in {
    val aynik = client.fetch().find(_.movie.title.contains("All You Need Is Kill"))

    aynik shouldBe defined
    val showtime = aynik.get.showtimes
      .find(_.dateTime.toLocalTime == java.time.LocalTime.of(18, 0))

    showtime shouldBe defined
    showtime.get.room   shouldBe Some("Sala 2")
    showtime.get.format should contain("NAP")
  }
}
