package clients.nckf

import models.KinoBajka
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.NckfClient
import tools.GetOnlyHttpFetch

import java.time.{LocalDate, LocalDateTime}

/** Drives the parser directly with a self-contained minimal HTML string.
 *  No fixture files are required because `NckfClient` exposes
 *  `parseHtml(html, today, cinema)` in the `services.cinemas` package; the
 *  inline HTML below is the canonical structure of one
 *  `div.events-archive__item.cinema.active` as fetched from
 *  `ec1lodz.pl/narodowe-centrum-kultury-filmowej/repertuar-kina/` on 07-06-2026.
 *
 *  The cinema parameter is passed explicitly (as `KinoBajka` standing in for the
 *  real `Nckf` object that will be added to Cinema.scala on integration). The
 *  `cinema` field tests propagation, not the specific object.
 *
 *  Recorder line (for a full fixture-based test later):
 *    curl -sSL -m 25 -A "Mozilla/5.0 (Macintosh; ...) Chrome/120 Safari/537.36"
 *      "https://ec1lodz.pl/narodowe-centrum-kultury-filmowej/repertuar-kina/"
 *      > test/resources/fixtures/nckf/ec1lodz.pl/narodowe-centrum-kultury-filmowej/repertuar-kina.html
 */
class NckfClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  // Minimal HTML matching the live page structure.
  // Film "Akademia Kina Polskiego: Ostatni etap (1948)…" has two slots on
  // 09-06-2026 and 10-06-2026; both are in the future relative to today=07-06-2026.
  // The past slot (02-06-2026) must be dropped by the today filter.
  private val sampleHtml = """<!DOCTYPE html><html><body>
    <div class="events-archive__item cinema active">
      <div class="events-archive__item-image">
        <img src="/fileadmin/user_upload/fe4.png" alt="Akademia Kina Polskiego: Ostatni etap (1948) + Przy torze kolejowym (1963)">
      </div>
      <div class="events-archive__item-content">
        <h3>Akademia Kina Polskiego: Ostatni etap (1948) + Przy torze kolejowym (1963)</h3>
        <div class="daty-godz-seansow-wrapper">
          <div class="daty-godz-seansow">
            <div class="daty-godz-seansow__date">02.06</div>
            <span class="daty-godz-seansow__date-full" style="display:none;">2026-06-02</span>
            <div class="daty-godz-seansow__hour">17:00</div>
            <a target="_blank" href="https://bilety.ec1lodz.pl/past" class="daty-godz-seansow__url"></a>Odeon
          </div>
          <div class="daty-godz-seansow">
            <div class="daty-godz-seansow__date">09.06</div>
            <span class="daty-godz-seansow__date-full" style="display:none;">2026-06-09</span>
            <div class="daty-godz-seansow__hour">17:00</div>
            <a target="_blank" href="https://bilety.ec1lodz.pl/future" class="daty-godz-seansow__url"></a>Odeon
          </div>
        </div>
        <a class="events-archive__item-url"
           href="/narodowe-centrum-kultury-filmowej/repertuar-kina/akademia-ostatni-etap"></a>
      </div>
    </div>
    <div class="events-archive__item cinema active">
      <div class="events-archive__item-image">
        <img src="/fileadmin/user_upload/batman.png" alt="Akademia Kina Światowego: Batman">
      </div>
      <div class="events-archive__item-content">
        <h3>Akademia Kina Światowego: Batman</h3>
        <div class="daty-godz-seansow-wrapper">
          <div class="daty-godz-seansow">
            <div class="daty-godz-seansow__date">10.06</div>
            <span class="daty-godz-seansow__date-full" style="display:none;">2026-06-10</span>
            <div class="daty-godz-seansow__hour">19:30</div>
            <a target="_blank" href="https://bilety.ec1lodz.pl/batman" class="daty-godz-seansow__url"></a>Urania
          </div>
        </div>
        <a class="events-archive__item-url"
           href="/narodowe-centrum-kultury-filmowej/repertuar-kina/akademia-batman"></a>
      </div>
    </div>
  </body></html>"""

  private val http = new GetOnlyHttpFetch {
    def get(url: String): String = sampleHtml
  }

  private val testCinema = KinoBajka  // stand-in; real integration uses Nckf
  private val today      = LocalDate.of(2026, 6, 7)
  private val client     = new NckfClient(http, testCinema, today)

  "NckfClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with the cinema passed in" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(testCinema)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Akademia Kina Polskiego: Ostatni etap on 2026-06-09 at 17:00" in {
    val movies = client.fetch()
    val film   = movies
      .find(_.movie.title == "Akademia Kina Polskiego: Ostatni etap (1948) + Przy torze kolejowym (1963)")
      .value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 9, 17, 0))
  }

  it should "drop the past slot (2026-06-02) and keep only future ones" in {
    val movies   = client.fetch()
    val akademia = movies
      .find(_.movie.title.startsWith("Akademia Kina Polskiego"))
      .value
    akademia.showtimes.map(_.dateTime) should not contain LocalDateTime.of(2026, 6, 2, 17, 0)
    akademia.showtimes should have size 1
  }

  it should "attach room and booking URL to each showtime" in {
    val movies = client.fetch()
    val akademia = movies.find(_.movie.title.startsWith("Akademia Kina Polskiego")).value
    val slot = akademia.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 9, 17, 0)).value
    slot.room.value shouldBe "Odeon"
    slot.bookingUrl.value should startWith("https://bilety.ec1lodz.pl/")
  }
}
