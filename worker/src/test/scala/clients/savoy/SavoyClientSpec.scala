package clients.savoy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.PhoenixCinemaEastFinchley
import services.cinemas.uk.SavoyClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a 2026-10-12 capture of
 *  https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/WhatsOn (`WriteSavoy`).
 *
 *  Phoenix East Finchley had never had a single showtime from Flicks, its only source,
 *  though its own Savoy Systems site sells eight months ahead. */
class SavoyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Base    = "https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/"
  private val today   = LocalDate.of(2026, 10, 12)
  private def scrape(on: LocalDate) =
    new SavoyClient(new FakeHttpFetch("savoy"), Base, PhoenixCinemaEastFinchley, on).fetch()
  private val results = scrape(today)
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "SavoyClient.fetch" should "read the whole programme Flicks never listed off the page's Events literal" in {
    results.size shouldBe 38
    results.flatMap(_.showtimes).size shouldBe 52
    results.map(_.cinema).toSet shouldBe Set(PhoenixCinemaEastFinchley)
    results.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 6, 5)
  }

  it should "read time, screen, booking link and subtitles off each performance" in {
    val film = byTitle("The Yellow Tie")
    film.showtimes.map(s => (s.dateTime, s.room)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 12, 19, 30) -> Some("Screen 1"),
      LocalDateTime.of(2026, 10, 15, 17, 0)  -> Some("Screen 2"))
    film.showtimes.head.bookingUrl.value shouldBe
      s"${Base}Booking?Booking=TSelectItems.waSelectItemsPrompt.TcsWebMenuItem_0.TcsWebTab_0.TcsPerformance_759865.TcsSection_543"
    // "SU":"Y" — Comédie-Française's one screening is subtitled.
    byTitle("Comédie-Française").showtimes.map(_.format) shouldBe Seq(List("SUB"))
    film.showtimes.map(_.format).distinct shouldBe Seq(Nil)
  }

  it should "carry the film's own record: runtime, year, credits, certificate, poster and page" in {
    val film = byTitle("Pressure")
    film.movie.runtimeMinutes shouldBe Some(101)
    film.movie.releaseYear shouldBe Some(2026)
    film.director shouldBe Seq("Anthony Maras")
    film.cast should contain ("Brendan Fraser")
    film.ageRating shouldBe Some("15")
    film.posterUrl shouldBe Some("https://images.savoysystems.co.uk/PXL/635675.jpg")
    film.filmUrl shouldBe Some(s"${Base}WhatsOn?f=635683")
    film.externalIds shouldBe Map("savoy" -> "635683")
  }

  it should "unescape the HTML entities Savoy leaves in its JSON text" in {
    byTitle.keySet should contain ("Royal Ballet & Opera 2026/27: Carmen")
    byTitle.keySet.filter(_.contains("&amp;")) shouldBe empty
    // A doubled space in the source title is one space on the card.
    byTitle.keySet should contain ("The People's Emergency Briefing")
  }

  it should "keep the screened stage relays a cinema sells — they are what the venue shows" in {
    byTitle.keySet should contain allOf ("Met Opera 2026-27: Macbeth", "National Theatre Live: Golden Boy", "Hadestown: The Musical")
  }

  it should "drop performances before today" in {
    val later = scrape(LocalDate.of(2026, 10, 16))
    later.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 17)
    later.map(_.movie.title) should not contain "The Yellow Tie"
  }

  "SavoyClient.parse" should "refuse a page without the Events literal rather than call it an empty programme" in {
    an[IllegalArgumentException] should be thrownBy
      SavoyClient.parse("<html><body>Maintenance</body></html>", Base, PhoenixCinemaEastFinchley, today)
  }
}
