package clients.modern_events_calendar

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{KinoCKZambrow, KinoSokolNisko}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.{ModernEventsCalendarClient, ModernEventsCalendarPage}

import java.time.{LocalDate, LocalDateTime}

/** Replays two venues' Modern Events Calendar pages, captured 2026-09-27 with
 *  the `admin-ajax.php` month loads the calendar's own "next month" arrow
 *  sends, and each film's event page:
 *
 *  - MOK Zambrów (`kino.mokzambrow.pl`), the `daily_view` skin, one event per
 *    (film, time) titled "15:30 – Mistyczka". Its Filmweb page — the venue's
 *    only source before — is empty from 2 October.
 *  - NCK Sokół Nisko (`nck.nisko.pl/kino-sokol/repertuar/`), the
 *    `monthly_view` skin filtered to the cinema's category, titles tagged
 *    "[PREMIERA] [DUBBING]". */
class ModernEventsCalendarClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today = LocalDate.of(2026, 9, 27)

  private val zambrow = new ModernEventsCalendarClient(new FakeHttpFetch("kino-ck-zambrow"),
    ModernEventsCalendarPage("https://kino.mokzambrow.pl/"), KinoCKZambrow, today)
  private val nisko = new ModernEventsCalendarClient(new FakeHttpFetch("kino-sokol-nisko"),
    ModernEventsCalendarPage("https://nck.nisko.pl/kino-sokol/repertuar/"), KinoSokolNisko, today)

  private lazy val zambrowMovies = zambrow.fetch()
  private lazy val niskoMovies   = nisko.fetch()

  "ModernEventsCalendarClient on the daily skin (Zambrów)" should "read every film of this month and the months after it" in {
    zambrowMovies.map(_.cinema).toSet shouldBe Set(KinoCKZambrow)
    zambrowMovies.map(_.movie.title) should contain theSameElementsAs Seq(
      "Dzień dziecka księdza Jana Kaczkowskiego", "Folwark zwierzęcy", "Kręciołek", "Lalka", "Marsupilami",
      "Mistyczka", "Odzyskany", "Podręcznik dla superbohaterów")
    zambrowMovies.map(_.showtimes.size).sum shouldBe 55
  }

  it should "peel the leading time off the event title and keep today's and later showings only" in {
    zambrowMovies.find(_.movie.title == "Mistyczka").value.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 27, 19, 30), LocalDateTime.of(2026, 9, 28, 19, 30),
      LocalDateTime.of(2026, 9, 29, 19, 30), LocalDateTime.of(2026, 10, 1, 19, 30))
  }

  it should "read the 12-hour clock on the daily skin and follow a run into the AJAX-loaded month" in {
    val lalka = zambrowMovies.find(_.movie.title == "Lalka").value
    lalka.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 10, 2, 18, 30), LocalDateTime.of(2026, 10, 22, 18, 30))
    lalka.showtimes should have size 19
  }

  it should "point the film at its event page, whose detail carries runtime, year, countries and trailer" in {
    val odzyskany = zambrowMovies.find(_.movie.title == "Odzyskany").value
    odzyskany.filmUrl.value shouldBe "https://kino.mokzambrow.pl/odzyskany/"
    val detail = zambrow.fetchFilmDetail(odzyskany.filmUrl.value).value
    detail.runtimeMinutes.value shouldBe 85
    detail.releaseYear.value shouldBe 2026
    detail.countries shouldBe Seq("Polska")
    detail.posterUrl.value shouldBe "https://kino.mokzambrow.pl/wp-content/uploads/2026/09/Odzyskany.jpg"
    detail.synopsis.value should startWith ("Janek (Filip Gurłacz) to chłopak")
    detail.synopsis.value should not include "Zapraszamy do głosowania"
    detail.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=LluTt_12yDM"
  }

  it should "read the production year out of its parentheses, off every listed country" in {
    val marsupilami = zambrowMovies.find(_.movie.title == "Marsupilami").value
    val detail = zambrow.fetchFilmDetail(marsupilami.filmUrl.value).value
    detail.countries shouldBe Seq("Belgia", "Francja")
    detail.releaseYear.value shouldBe 2026
    detail.runtimeMinutes.value shouldBe 98
  }

  it should "keep a synopsis whose sentences read like 'label – value'" in {
    val kaczkowski = zambrowMovies.find(_.movie.title == "Dzień dziecka księdza Jana Kaczkowskiego").value
    val detail = zambrow.fetchFilmDetail(kaczkowski.filmUrl.value).value
    detail.synopsis.value should startWith ("Ksiądz Jan Kaczkowski – charyzmatyczny duchowny")
    detail.genres shouldBe Seq("Dokumentalny")
  }

  it should "walk past a dark month to the programme that resumes after it" in {
    // October served as dark (November's empty load) and October's real load served
    // as December's: a venue on a break that sells the months after it.
    val replay = new FakeHttpFetch("kino-ck-zambrow")
    val dark = new tools.HttpFetch {
      def get(url: String): String = replay.get(url)
      def post(url: String, body: String, contentType: String): String =
        if (body.contains("mec_month=10")) replay.post(url, body.replace("mec_month=10", "mec_month=11"), contentType)
        else if (body.contains("mec_month=12")) replay.post(url, body.replace("mec_month=12", "mec_month=10"), contentType)
        else replay.post(url, body, contentType)
    }
    val movies = new ModernEventsCalendarClient(dark, ModernEventsCalendarPage("https://kino.mokzambrow.pl/"),
      KinoCKZambrow, today).fetch()
    movies.flatMap(_.showtimes).map(_.dateTime.getMonthValue).toSet should contain (10)
  }

  it should "read a runtime with words before the number, in any case" in {
    def runtime(line: String) = {
      val page = new tools.HttpFetch {
        def get(url: String): String = s"""<div class="entry-content"><p>$line</p></div>"""
        def post(url: String, body: String, contentType: String): String = ""
      }
      new ModernEventsCalendarClient(page, ModernEventsCalendarPage("https://kino.mokzambrow.pl/"), KinoCKZambrow, today)
        .fetchFilmDetail("https://kino.mokzambrow.pl/odzyskany/").flatMap(_.runtimeMinutes)
    }
    runtime("Czas trwania: ok. 85 min").value shouldBe 85
    runtime("Czas trwania: 1 GODZ. 30 MIN").value shouldBe 90
  }

  "ModernEventsCalendarClient on the monthly skin (Nisko)" should "read the category-filtered calendar, tags peeled into badges" in {
    niskoMovies.map(_.cinema).toSet shouldBe Set(KinoSokolNisko)
    niskoMovies.map(_.movie.title) should contain theSameElementsAs Seq("Lalka", "Obcy", "Tedi i magiczna lampa")
    niskoMovies.find(_.movie.title == "Obcy").value.showtimes.map(s => (s.dateTime, s.format)) shouldBe Seq(
      (LocalDateTime.of(2026, 9, 27, 19, 0), List("NAP")))
    niskoMovies.find(_.movie.title == "Tedi i magiczna lampa").value.showtimes.map(s => (s.dateTime, s.format)) shouldBe Seq(
      (LocalDateTime.of(2026, 9, 27, 15, 0), List("DUB")), (LocalDateTime.of(2026, 9, 27, 17, 0), List("DUB")))
  }

  it should "follow a run from this month's page into the AJAX-loaded next month" in {
    val lalka = niskoMovies.find(_.movie.title == "Lalka").value
    lalka.showtimes should have size 26
    lalka.showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 9, 30, 16, 0)
    lalka.showtimes.last.dateTime shouldBe LocalDateTime.of(2026, 10, 28, 16, 0)
  }

  it should "read runtime, countries, genres, synopsis, poster and trailer off the MEC single-event page" in {
    val obcy   = niskoMovies.find(_.movie.title == "Obcy").value
    val detail = nisko.fetchFilmDetail(obcy.filmUrl.value).value
    detail.runtimeMinutes.value shouldBe 120
    detail.countries shouldBe Seq("Francja")
    detail.genres shouldBe Seq("dramat", "kryminał")
    detail.synopsis.value should startWith ("„Obcy” to nowa ekranizacja")
    detail.synopsis.value should not include "czas trwania"
    detail.posterUrl.value shouldBe "https://nck.nisko.pl/wp-content/uploads/2026/09/8265756.8.webp"
    detail.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=b2yaQbsNAk0"

    val lalka = niskoMovies.find(_.movie.title == "Lalka").value
    nisko.fetchFilmDetail(lalka.filmUrl.value).value.runtimeMinutes.value shouldBe 170
  }
}
