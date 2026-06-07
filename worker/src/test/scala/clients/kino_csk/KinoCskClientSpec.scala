package clients.kino_csk

import clients.tools.FakeHttpFetch
import models.KinoCskLublin
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoCskClient

import java.time.LocalDateTime

/** Replays the recorded iKSORIS `bilety.csklublin.pl/termin.html` page
 *  (07-06-2026 capture) through the client.  The fixture contains 4 upcoming
 *  "Seanse filmowe" events: 1 indoor Sala Kinowa screening (Akademia Filmowa
 *  Festiwalu NNW, 09-06-2026) and 3 outdoor rooftop "KINO NA DACHU" screenings
 *  (12, 19, 26 June).  Only the indoor event passes the outdoor filter. */
class KinoCskClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-csk")
  private val client = new KinoCskClient(http, KinoCskLublin)

  "KinoCskClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoCskLublin" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoCskLublin)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "exclude outdoor KINO NA DACHU events" in {
    val movies = client.fetch()
    movies.map(_.movie.title).exists(_.contains("KINO NA DACHU")) shouldBe false
  }

  it should "pin a concrete indoor screening: Akademia Filmowa Festiwalu NNW on 2026-06-09 at 18:00" in {
    // The fixture captures the Akademia Filmowa Festiwalu NNW double-feature on 09-06-2026
    // at 18:00 in the Sala Kinowa. After title cleaning (" | " suffix strip) the title
    // becomes "Akademia Filmowa Festiwalu NNW".
    val movies = client.fetch()
    val akademia = movies.find(_.movie.title == "Akademia Filmowa Festiwalu NNW").value
    akademia.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 9, 18, 0))
    akademia.showtimes.flatMap(_.bookingUrl).head should startWith("https://bilety.csklublin.pl/nienumerowane.html?id=6928")
  }
}
