package clients.kino_swiatowid_elblag

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoSwiatowidElblag
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoSwiatowidElblagClient
import services.cinemas.common.ScrapeHorizon
import tools.GetOnlyHttpFetch

import java.time.LocalDate
import scala.collection.mutable.ArrayBuffer

/** Replays the recorded per-day `repertuar?dzien=YYYY-MM-DD` pages (captured
 *  2026-09-23 through the tail of the venue's published programme) through
 *  the client, pinning `today` to the capture date so the day-window URLs hit
 *  the recorded fixtures.
 *
 *  The per-day date is load-bearing in a way this venue is unusually sneaky
 *  about: the booking link's own slug (`…/kup-bilet/tedi-i-magiczna-lampa-
 *  2026-09-18-14-00-7-4`) embeds a date that is NOT the screening's real date
 *  — it's frozen at whenever the recurring booking-system "event" was
 *  created. Probing the booking site directly confirmed the REAL date is
 *  always the `dzien` value the `<li>` was read under, paired with its bare
 *  `HH:MM` text — never the slug. */
class KinoSwiatowidElblagClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 23)
  private val movies = new KinoSwiatowidElblagClient(new FakeHttpFetch("kino-swiatowid-elblag"), today = today).fetch()

  "KinoSwiatowidElblagClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSwiatowidElblag)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening to the queried day and carry the booking link" in {
    val film = movies.find(_.movie.title == "Obcy").value
    val first = film.showtimes.find(_.dateTime == java.time.LocalDateTime.of(2026, 9, 23, 19, 45)).value
    first.bookingUrl.value should include("bilet.swiatowid.elblag.pl")
  }

  it should "trust the queried `dzien` day over the date baked into the booking-link slug" in {
    // "Tedi i magiczna lampa" screened at 12:00 on 26 September — but its
    // booking slug for that very showing embeds "2026-09-18-14-00", a
    // completely different date and time. If the parser trusted the slug
    // instead of the day it was read under, this showtime either wouldn't
    // exist or would carry the wrong date.
    val film = movies.find(_.movie.title == "Tedi i magiczna lampa").value
    val slot = film.showtimes.find(_.dateTime == java.time.LocalDateTime.of(2026, 9, 26, 12, 0)).value
    slot.bookingUrl.value should include("tedi-i-magiczna-lampa-2026-09-18-14-00-7-4")
  }

  it should "carry the language-version tag off the second description line" in {
    val film = movies.find(_.movie.title == "Marsupilami").value
    all(film.showtimes.map(_.format)) shouldBe List("DUB")
  }

  it should "parse genres and runtime off the first description line" in {
    val film = movies.find(_.movie.title == "Obcy").value
    film.movie.runtimeMinutes.value shouldBe 122
    film.movie.genres should contain allOf ("dramat", "kryminał")
  }

  it should "keep legitimate event-cinema broadcasts rather than treating them as live events" in {
    movies.map(_.movie.title) should contain ("The Metropolitan Opera: Cosi Fan Tutte")
    movies.map(_.movie.title) should contain ("André Rieu. Niech żyje Maastricht!")
  }

  it should "drop a genuine non-film event from the merged listing" in {
    val day = "2026-09-23"
    val eventHtml =
      s"""<div class="movie-card">
         |  <div class="movie-card__thumbnail"><a href="http://kino.swiatowid.elblag.pl/repertuar/999,koncert"><img src="/poster.jpg"></a></div>
         |  <h3 class="movie-card__title">Koncert Joscho Stephan Trio</h3>
         |  <p class="movie-card__description">koncert - 90 min - b/o</p>
         |  <p class="movie-card__description"></p>
         |  <ul class="movie-card__showtimes"><li><a href="https://bilet.swiatowid.elblag.pl/index.php/kup-bilet/koncert-x">19:00</a></li></ul>
         |</div>""".stripMargin
    val stub = new GetOnlyHttpFetch {
      def get(url: String): String = if (url.contains(s"dzien=$day")) eventHtml else "<html></html>"
    }
    val filtered = new KinoSwiatowidElblagClient(stub, KinoSwiatowidElblag, LocalDate.parse(day)).fetch()
    filtered.map(_.movie.title) should not contain "Koncert Joscho Stephan Trio"
  }

  // ── The day-walk itself ──────────────────────────────────────────────
  //
  // A stub rather than the recorded corpus: what's under test is which DAYS
  // get asked for, not how a day's HTML parses (covered above).

  private def movieCardHtml(day: String): String =
    s"""<div class="movie-card">
       |  <h3 class="movie-card__title">Coś w repertuarze</h3>
       |  <p class="movie-card__description">dramat - 100 min - 12+</p>
       |  <p class="movie-card__description"></p>
       |  <ul class="movie-card__showtimes"><li><a href="https://bilet.swiatowid.elblag.pl/index.php/kup-bilet/x-$day-20-00">20:00</a></li></ul>
       |</div>""".stripMargin

  "planChunks" should "walk forward while the programme answers and stop once it runs dry" in {
    val start = LocalDate.of(2026, 9, 1)
    val stub = new GetOnlyHttpFetch {
      def get(url: String): String = {
        val day = """dzien=(\d{4}-\d{2}-\d{2})""".r.findFirstMatchIn(url).map(_.group(1)).getOrElse("")
        if (day <= "2026-09-10") movieCardHtml(day) else "<html></html>"
      }
    }
    val days = new KinoSwiatowidElblagClient(stub, KinoSwiatowidElblag, start).planChunks().flatMap(_.split(","))

    days should contain ("2026-09-10")
    days should not contain "2026-09-11"
  }

  it should "stop once the programme runs out, so a dormant venue stays cheap" in {
    val asked = ArrayBuffer.empty[String]
    val stub = new GetOnlyHttpFetch {
      def get(url: String): String = { asked += url; "<html></html>" }
    }
    val client = new KinoSwiatowidElblagClient(stub, KinoSwiatowidElblag, LocalDate.of(2026, 9, 1))

    client.planChunks() shouldBe empty
    asked.size shouldBe ScrapeHorizon.MaxEmptyDays
  }
}
