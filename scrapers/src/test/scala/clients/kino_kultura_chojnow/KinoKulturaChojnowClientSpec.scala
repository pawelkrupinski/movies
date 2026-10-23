package clients.kino_kultura_chojnow

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoKulturaChojnow
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKulturaChojnowClient

import java.time.LocalDateTime

/** Replays a 2026-10-23 capture of MOKSiR Chojnów's Ticket Manager feed,
 *  `bilety.chojnow.eu/Repertuar/Kalendarz1JsonDane?filter=https://bilety.chojnow.eu/`:
 *  9 screenings, 23–26 Oct.
 *
 *  That day the `kino.chojnow.eu` card grid the client used to read carried all
 *  three of the weekend's films (Róża, Marsupilami, Mistyczka) with a bare-date
 *  `Rozpoczęcie:` and their times only in prose, so it parsed to nothing and
 *  the venue sat white. The feed carries every screening with its time. */
class KinoKulturaChojnowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoKulturaChojnowClient(new FakeHttpFetch("kino-kultura-chojnow")).fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoKulturaChojnowClient" should "read every screening of the Ticket Manager feed" in {
    movies.flatMap(_.showtimes) should have size 9
    movies.map(_.movie.title) should contain theSameElementsAs Seq(
      "Kicia Kocia na pikniku", "Marsupilami", "Mira", "Mistyczka", "Róża")
    movies.map(_.cinema).toSet shouldBe Set(KinoKulturaChojnow)
  }

  it should "take each time from the event's 'godz.' line, not the feed's 23:59 date stamp" in {
    film("Róża").showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 10, 23, 12, 30), LocalDateTime.of(2026, 10, 24, 18, 0), LocalDateTime.of(2026, 10, 25, 18, 0))
  }

  it should "link each screening sold online to its seat map, and leave the box-office-only one unlinked" in {
    val roza = film("Róża").showtimes.map(s => s.dateTime.toLocalDate.getDayOfMonth -> s.bookingUrl).toMap
    roza(24).value shouldBe "https://bilety.chojnow.eu/Bilety/Sala/1639"
    roza(23) shouldBe None
  }

  it should "take the poster the box office shows for the film" in {
    film("Mistyczka").posterUrl.value should startWith("http://www.bilety.chojnow.eu/obrazki/")
  }
}
