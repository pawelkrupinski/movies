package clients.rialto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.RialtoClient
import tools.HttpFetch

import java.time.LocalDateTime

/** Rialto lists the same film under several repertoire blocks, each linking its
 *  own event page. The plain title and a "- pokaz przedpremierowy" preview
 *  screening both normalize to "Ojczyzna" and MERGE into one row. A
 *  "Filmowy Klub Seniora:" senior-club showing targets a distinct audience and
 *  stays its OWN row (its prefix is kept by `stripCyclePrefix`).
 *
 *  Each event page double-lists every slot (two ticket buttons per showing),
 *  which `parseEventPage`'s `distinctBy(_.dateTime)` folds back to one. */
class RialtoOjczyznaReproSpec extends AnyFlatSpec with Matchers {

  private val Sep = """<hr class="table-seperator"/>"""

  private def block(rawTitle: String, eventId: String): String =
    s"""<div class="list-item">
       |  <a title="Film: $rawTitle" href="https://www.kinorialto.poznan.pl/wydarzenie/?id=$eventId">Zobacz</a>
       |  <div class="title">$rawTitle</div>
       |</div>""".stripMargin

  // Each slot appears twice — mirrors the live page's two ticket buttons.
  private def eventPage(times: Seq[String]): String = {
    val buttons = times.flatMap { t =>
      Seq.fill(2)(
        s"""<a class="b24-button show" href="https://www.kinorialto.poznan.pl/b24/?id=x" title="Kup bilet - Film: OJCZYZNA - $t - Poznań">Kup</a>"""
      )
    }.mkString("\n")
    s"<html><body>$buttons</body></html>"
  }

  private val seniora = Seq("2026-06-23 13:00", "2026-06-23 15:30")
  private val regular = Seq("2026-06-14 17:30", "2026-06-19 19:15", "2026-06-20 14:15")
  private val preview = Seq("2026-06-04 17:00", "2026-06-05 19:00")

  private val repertoire = Seq(
    block("Filmowy Klub Seniora: OJCZYZNA", "157099"),
    block("OJCZYZNA", "157098"),
    block("OJCZYZNA - pokaz przedpremierowy", "157192")
  ).mkString(s"\n$Sep\n")

  private val http = new HttpFetch {
    def get(url: String): String =
      if (url.contains("/repertuar")) repertoire
      else """id=(\d+)""".r.findFirstMatchIn(url).map(_.group(1)) match {
        case Some("157099") => eventPage(seniora)
        case Some("157098") => eventPage(regular)
        case Some("157192") => eventPage(preview)
        case other          => throw new IllegalArgumentException(s"unexpected url $url ($other)")
      }
    def post(url: String, body: String, contentType: String): String = ???
  }

  private val results = new RialtoClient(http).fetch()
  private def times(cm: models.CinemaMovie) = cm.showtimes.map(_.dateTime).toSet

  "RialtoClient" should "merge the plain and preview blocks into one Ojczyzna row" in {
    val ojczyzna = results.filter(_.movie.title.equalsIgnoreCase("Ojczyzna"))
    ojczyzna.size shouldBe 1
    val t = times(ojczyzna.head)
    t.size shouldBe (regular ++ preview).size
    t should contain(LocalDateTime.of(2026, 6, 14, 17, 30)) // plain (157098)
    t should contain(LocalDateTime.of(2026, 6, 4, 17, 0))   // preview (157192)
    t should not contain LocalDateTime.of(2026, 6, 23, 13, 0) // NOT the Klub Seniora slot
  }

  it should "keep the Filmowy Klub Seniora showing as its own row" in {
    val seniorRow = results.filter(_.movie.title.toLowerCase.contains("klub seniora"))
    seniorRow.size shouldBe 1
    seniorRow.head.movie.title shouldBe "Filmowy klub seniora: ojczyzna"
    val t = times(seniorRow.head)
    t shouldBe Set(LocalDateTime.of(2026, 6, 23, 13, 0), LocalDateTime.of(2026, 6, 23, 15, 30))
  }
}
