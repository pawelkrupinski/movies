package services.cinemas.common

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/** The markup [[FlicksClient.slimmed]] drops before parsing must change nothing the parse reads:
 *  every recorded day fragment parses to the same slots slimmed as raw. */
class FlicksSlimSpec extends AnyFlatSpec with Matchers {
  private val Uk   = FlicksMarket.UnitedKingdom
  private val Days = Seq(
    "odeon-cinema-norwich/2026-07-11", "arc-cinema-at-the-royalty-great-yarmouth/2026-07-11",
    "dartington-art-centre-totnes/2026-07-28")
  private def page(day: String) =
    clients.tools.FixtureFile.read(s"test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/$day.html")
  private def dateOf(day: String) = LocalDate.parse(day.split('/').last)

  "FlicksClient.parseDay" should "read the same slots off the slimmed fragment as off the raw one" in {
    Days.foreach { day =>
      val raw = FlicksClient.parseDocument(Jsoup.parse(page(day), Uk.baseUrl), dateOf(day), Uk)
      withClue(day)(FlicksClient.parseDay(page(day), dateOf(day), Uk) shouldBe raw)
    }
  }

  // The positive control for the test above: a slimming that dropped nothing would pass it too.
  "FlicksClient.slimmed" should "drop the repeated session blobs and the icons from a busy venue's day" in {
    val busy = page(Days.head)
    val slim = FlicksClient.slimmed(busy)
    slim.length.toDouble / busy.length should be < 0.5
    slim should not include "<svg"
    "data-eventjson".r.findAllMatchIn(slim).size shouldBe FlicksClient.parseDay(busy, dateOf(Days.head), Uk).map(_.slug).distinct.size
  }
}
