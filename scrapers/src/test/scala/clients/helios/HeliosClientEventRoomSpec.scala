package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.HeliosClient
import services.movies.SingleCountryNormalizer.titleNormalizer

// Event screenings ("... w Helios Anime", concerts, sports broadcasts) are
// excluded from the REST `/screening` endpoint entirely, so room enrichment by
// the shared screening UUID found nothing and left `room = None`. The room is
// available from the `/api/cinema/{id}/event` endpoint, which lists each event
// screening with its screeningId + screenId — resolvable to a hall name via the
// existing `/screen/{id}` lookup. Regression for the "All You Need Is Kill"
// anime event, which rendered without a room.
class HeliosClientEventRoomSpec extends AnyFlatSpec with Matchers {

  private val client = new HeliosClient(new FakeHttpFetch("helios/event-room"), titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay)

  "HeliosClient.fetch" should "resolve the room for an event screening absent from /screening" in {
    val aynik = client.fetch().find(_.movie.title.contains("All You Need Is Kill"))

    aynik shouldBe defined
    val showtime = aynik.get.showtimes
      .find(_.dateTime.toLocalTime == java.time.LocalTime.of(18, 0))

    showtime shouldBe defined
    showtime.get.room   shouldBe Some("Sala 2")
    showtime.get.format should contain("NAP")
  }

  // A screen body only names a showtime's room — no film depends on it — so one that fails leaves
  // the room empty and the listing complete; reported, it kept the venue from ever pruning.
  it should "leave the room empty, and the listing's completeness untouched, when a screen lookup fails" in {
    val recorded = new FakeHttpFetch("helios/event-room")
    val failingScreens = new tools.GetOnlyHttpFetch {
      override def get(url: String): String =
        if (url.contains("/screen/")) throw new java.io.IOException("screen lookup down") else recorded.get(url)
    }
    val (movies, reads) = services.cinemas.common.ListingReads.during(
      new HeliosClient(failingScreens, titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay).fetch())
    movies.flatMap(_.showtimes).flatMap(_.room) shouldBe empty
    movies.find(_.movie.title.contains("All You Need Is Kill")) shouldBe defined
    reads.failed.map(_.getMessage).filter(_.contains("screen lookup down")) shouldBe empty
  }
}
