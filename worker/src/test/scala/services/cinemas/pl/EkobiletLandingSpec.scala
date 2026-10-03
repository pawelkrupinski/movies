package services.cinemas.pl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The card-grid landing's (title, detail URL) pairs, off hand-cut markup in the
 *  shape the recorded `ekobilet-jaworzyna` landing has: each `event-card` inside its
 *  own `desktop-card-wrapper`, the title a `p.overme` beside it. */
class EkobiletLandingSpec extends AnyFlatSpec with Matchers {

  private def card(slug: String, title: Option[String]) =
    s"""<div class="d-flex flex-column desktop-card-wrapper">
       |  <div class="event-card"><a href="https://ekobilet.pl/kino-jaworzyna/$slug?date=2026-06-16">Kup bilet</a></div>
       |  ${title.fold("")(t => s"""<div><p class="primary-color overme">$t</p></div>""")}
       |</div>""".stripMargin

  "EkobiletClient.parseLanding" should "drop a card with no title of its own rather than lend it another card's" in {
    val html = s"<div>${card("lalka-1", Some("Lalka | 2D"))}${card("bez-tytulu-2", None)}</div>"
    EkobiletClient.parseLanding(html) shouldBe Seq("Lalka | 2D" -> "https://ekobilet.pl/kino-jaworzyna/lalka-1")
  }
}
