package clients.flicks

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FlicksClient

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/** Replays a recorded Flicks sessions fragment (Odeon Cinema Norwich, date
 *  2026-07-11) through the pure `parseDay`. Pins that the AJAX
 *  `/cinema/sessions/<slug>/<date>/` fragment parses: one film per
 *  `article.cinema-times__article`, its `/movie/<slug>` id + title + runtime +
 *  director + Flicks `content_id`, the session times (24h `data-optlabel` and
 *  the 12h fallback) as local `LocalDateTime`s, the cinema-chain booking
 *  deep-links, and the premium/format labels (IMAX…). */
class FlicksClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val date = LocalDate.of(2026, 7, 11)

  private def fixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/odeon-cinema-norwich/2026-07-11.html")
    try src.mkString finally src.close()
  }

  private val slots = FlicksClient.parseDay(fixture, date)

  "parseDay" should "read every film on the day" in {
    slots.map(_.slug).distinct.size shouldBe 15
  }

  it should "carry a film's title, slug, runtime, director and Flicks id" in {
    val minions = slots.find(_.slug == "minions-3").value
    minions.title shouldBe "Minions & Monsters"       // jsoup decodes the &amp;
    minions.runtimeMinutes.value shouldBe 90           // "90 mins"
    minions.director.value shouldBe "Pierre Coffin"
    minions.contentId.value shouldBe "25079"
  }

  it should "parse session times to local date-times with the chain booking link" in {
    val tenTen = slots.find(s => s.slug == "minions-3" && s.dateTime == LocalDateTime.of(2026, 7, 11, 10, 10)).value
    tenTen.booking.value should include("odeon.co.uk")
  }

  it should "surface premium/format labels on variant screenings" in {
    slots.flatMap(_.format).toSet should contain("IMAX")
  }
}
