package clients.iksoris

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoWCK
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{IksorisBookingPage, IksorisCalendarClient, IksorisOrigin}
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays Wejherowskie Centrum Kultury's (Wejherowo) iKsoris "STARTER" booking
 *  site, captured 2026-09-27: the KINO group's (`idg=1`) month calendars for
 *  September, October and an empty November, and the `pobierzTerminy` JSON of
 *  each of the 25 days the calendars mark — 76 screenings of 14 films through
 *  25 October, where Filmweb (the venue's only source before) listed 7 films
 *  over its 7-day window.
 *
 *  Fixture directory: test/resources/fixtures/kino-wck/ */
class IksorisCalendarClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val page  = IksorisBookingPage(IksorisOrigin("https://bilety.wck.org.pl"))
  private val today = LocalDate.of(2026, 9, 27)

  private lazy val movies = new IksorisCalendarClient(new FakeHttpFetch("kino-wck"), page, KinoWCK, today).fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "IksorisCalendarClient" should "walk every scheduled day of every month the calendar fills" in {
    movies.map(_.cinema).toSet shouldBe Set(KinoWCK)
    movies.flatMap(_.showtimes) should have size 76
    movies should have size 14
    val dates = movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet
    dates should have size 25
    dates should contain allOf (LocalDate.of(2026, 9, 27), LocalDate.of(2026, 10, 10), LocalDate.of(2026, 10, 25))
  }

  it should "pin a screening to its time and seat-picker booking link" in {
    val mistyczka = film("Mistyczka")
    mistyczka.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 27, 17, 0), LocalDateTime.of(2026, 10, 3, 12, 0), LocalDateTime.of(2026, 10, 5, 19, 30))
    mistyczka.showtimes.find(_.dateTime == LocalDateTime.of(2026, 10, 3, 12, 0)).value.bookingUrl.value shouldBe
      "https://bilety.wck.org.pl/rezerwacja/miejsca.html?id=14736&idt=058d72d9e629de6bb4ee3e07c2309724&d=3&idg=1"
  }

  it should "move the version word off the title onto the showtime" in {
    film("TEDI I MAGICZNA LAMPA").showtimes.map(_.format).toSet shouldBe Set(List("DUB"))
    film("HOT SPOT").showtimes.map(_.format).toSet shouldBe Set(List("NAP"))
    film("500 MIL").showtimes.map(_.format).toSet shouldBe Set(List("LEK"))
    all(film("Lalka").showtimes.map(_.format)) shouldBe empty
    movies.map(_.movie.title).filter(_.matches("(?i).*\\b(dubbing|napisy|lektor)$")) shouldBe empty
  }

  it should "drop the film-cycle label glued on after a slash" in {
    film("Bez znieczulenia").showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 10, 18, 30))
    movies.map(_.movie.title).filter(_.contains("WAJDA")) shouldBe empty
  }

  it should "carry the event description as the synopsis, paragraphs kept" in {
    val synopsis = film("TEDI I MAGICZNA LAMPA").synopsis.value
    synopsis should startWith("Oli, nieustraszona dwuletnia córka Tediego")
    synopsis should include("\n\nTedi, Sara i cała ekipa")
    synopsis should not include "\r"
  }

  it should "walk past a dark month to the programme that resumes after it" in {
    // October served as dark (November's empty calendar) and October's real calendar
    // served as December's: a venue on a break that sells the months after it.
    val replay = new FakeHttpFetch("kino-wck")
    def month(n: Int) = IksorisCalendarClient.calendarUrl(page, java.time.YearMonth.of(2026, n))
    val dark = new tools.HttpFetch {
      def get(url: String): String =
        if (url == month(10)) replay.get(month(11))
        else if (url == month(12)) replay.get(month(10))
        else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val dates = new IksorisCalendarClient(dark, page, KinoWCK, today).fetch()
      .flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet
    dates should contain allOf (LocalDate.of(2026, 9, 27), LocalDate.of(2026, 10, 25))
  }

  it should "drop only the day whose answer is not JSON (a session page), keeping the rest" in {
    val replay = new FakeHttpFetch("kino-wck")
    val brokenDay = IksorisCalendarClient.dayUrl(page, LocalDate.of(2026, 9, 27))
    val sessionPage = new tools.HttpFetch {
      def get(url: String): String = if (url == brokenDay) "<html><body>Sesja wygasła</body></html>" else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val dates = new IksorisCalendarClient(sessionPage, page, KinoWCK, today).fetch()
      .flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet
    dates should have size 24
    dates should not contain LocalDate.of(2026, 9, 27)
  }

  it should "propagate a failed calendar instead of reporting an empty scrape" in {
    a[HttpStatusException] should be thrownBy
      new IksorisCalendarClient(new FailingHttpFetch(503), page, KinoWCK, today).fetch()
  }

  it should "fail the scrape when this month's calendar answers 200 without a calendar" in {
    val replay = new FakeHttpFetch("kino-wck")
    val thisMonth = IksorisCalendarClient.calendarUrl(page, java.time.YearMonth.from(today))
    val erroring = new tools.HttpFetch {
      def get(url: String): String = if (url == thisMonth) """{"status":"error"}""" else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    an[IllegalStateException] should be thrownBy new IksorisCalendarClient(erroring, page, KinoWCK, today).fetch()
  }
}
