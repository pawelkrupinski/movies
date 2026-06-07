package clients.kino_pod_baranami

import clients.tools.FakeHttpFetch
import models.KinoPodBaranami
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoPodBaranamiClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar.php` page (07-06-2026 capture, ISO-8859-2)
 *  through the client. The fixture is fetched via `getBytes` so the encoding
 *  decode path is exercised correctly, just as in production. `today` is pinned
 *  to the capture date so year-inference is stable. */
class KinoPodBaranamiClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-pod-baranami")
  private val client = new KinoPodBaranamiClient(http, KinoPodBaranami, LocalDate.of(2026, 6, 7))

  "KinoPodBaranamiClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoPodBaranami" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoPodBaranami)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Diabeł ubiera się u Prady 2 on 2026-06-07 at 10:30" in {
    val movies = client.fetch()
    val diabel = movies.find(_.movie.title.contains("Diabe")).value
    diabel.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 10, 30))
    diabel.showtimes.flatMap(_.bookingUrl).head should include("/rezerwacja_start.php?event_id=")
  }
}
