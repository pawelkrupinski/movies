package clients.rialto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.RialtoClient
import tools.HttpFetch

import java.time.LocalDateTime

/** Rialto lists the same film under several repertoire blocks — a plain title,
 *  a "Filmowy Klub Seniora:" cycle prefix, and a "- pokaz przedpremierowy"
 *  preview suffix — each linking to its own event page. `normalizeTitle`
 *  collapses all three to "Ojczyzna", and `fetch` must merge the showtimes
 *  from every linked event page into the single row, not just the first one.
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

  "RialtoClient" should "merge every Ojczyzna event page into one row" in {
    val ojczyzna = new RialtoClient(http).fetch()
      .filter(_.movie.title.equalsIgnoreCase("Ojczyzna"))

    ojczyzna.size shouldBe 1
    val times = ojczyzna.head.showtimes.map(_.dateTime).toSet
    times.size shouldBe (seniora ++ regular ++ preview).size
    // A date that exists only on each respective event page — proves none of
    // the three blocks was dropped during the merge.
    times should contain(LocalDateTime.of(2026, 6, 23, 13, 0)) // cycle-prefix block (157099)
    times should contain(LocalDateTime.of(2026, 6, 14, 17, 30)) // plain block (157098)
    times should contain(LocalDateTime.of(2026, 6, 4, 17, 0))  // preview-suffix block (157192)
  }
}
