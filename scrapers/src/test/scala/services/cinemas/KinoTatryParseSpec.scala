package services.cinemas

import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoTatry
import services.cinemas.pl.KinoTatryClient

import java.time.{LocalDate, LocalDateTime}

/** Unit cover for `KinoTatryClient.parseHomepage`'s slot parsing — a single card
 *  whose `harmonogram-list` carries several comma-separated times across two
 *  dates (the real homepage fixture only ever has one time per date). */
class KinoTatryParseSpec extends AnyFlatSpec with Matchers with LoneElement {

  "KinoTatryClient.parseHomepage" should "expand comma-separated times and multiple dates in one card" in {
    val html =
      """<div class="item-content">
        |  <h3 class="title-article">TEST FILM</h3>
        |  <div class="et-seans"><ul class="harmonogram-list">
        |    <li><strong>21/06/2026:</strong> 15:00, 18:30</li>
        |    <li><strong>22/06/2026:</strong> 20:00</li>
        |  </ul></div>
        |  <div class="position-button"><a class="btn-theme" href="https://kinotatrylodz.pl/repertuar/test-film/">Więcej</a></div>
        |</div>""".stripMargin
    val movie = KinoTatryClient.parseHomepage(html, LocalDate.of(2026, 6, 21), KinoTatry).loneElement
    val expected = Seq(
      LocalDateTime.of(2026, 6, 21, 15, 0),
      LocalDateTime.of(2026, 6, 21, 18, 30),
      LocalDateTime.of(2026, 6, 22, 20, 0))
    movie.showtimes.map(_.dateTime) shouldBe expected
  }
}
