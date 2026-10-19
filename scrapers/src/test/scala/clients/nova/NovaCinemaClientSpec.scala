package clients.nova

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.NovaCinemaWoking
import services.cinemas.uk.NovaCinemaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a 2026-10-12 capture of https://www.novacinema.com/ (`WriteNovaCinema`).
 *
 *  Nova Cinema Woking's seven screens never had a showtime from Flicks, their only
 *  source; the cinema's own home page carries the whole programme. */
class NovaCinemaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today   = LocalDate.of(2026, 10, 12)
  private def scrape(on: LocalDate) = new NovaCinemaClient(new FakeHttpFetch("nova"), NovaCinemaWoking, on).fetch()
  private val results = scrape(today)
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "NovaCinemaClient.fetch" should "read the whole programme Flicks never listed off the home page" in {
    results.size shouldBe 40
    results.flatMap(_.showtimes).size shouldBe 249
    results.map(_.cinema).toSet shouldBe Set(NovaCinemaWoking)
    results.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 5, 30)
  }

  it should "read each card's day, time, screen, subtitles and vivenu booking link" in {
    val film = byTitle("Verity")
    film.showtimes.take(4).map(s => (s.dateTime, s.room.value, s.format)) shouldBe Seq(
      (LocalDateTime.of(2026, 10, 12, 17, 15), "Screen 5", Nil),
      (LocalDateTime.of(2026, 10, 12, 20, 15), "Screen 5", Nil),
      (LocalDateTime.of(2026, 10, 13, 17, 30), "Screen 3", Nil),
      (LocalDateTime.of(2026, 10, 13, 20, 15), "Screen 2", List("SUB")))
    film.showtimes.head.bookingUrl.value shouldBe "https://vivenu.com/event/6ac4af72d8cc679b258d4375"
  }

  it should "carry the show's certificate, runtime, full-size poster and page" in {
    val film = byTitle("Verity")
    film.ageRating shouldBe Some("15")
    film.movie.runtimeMinutes shouldBe Some(117)
    film.posterUrl.value shouldBe "https://www.novacinema.com/cmsUploads/tmdbimage/images/yAmkYfM9Xwq1TfnIXPCW61cSMrL.jpg"
    film.filmUrl.value shouldBe "https://www.novacinema.com/shows/verity.html"
  }

  it should "take the strand Nova appends after a bar off the title" in {
    byTitle("Avengers: Doomsday").movie.rawTitle shouldBe Some("Avengers: Doomsday | Infinity Vision")
    byTitle.keySet.filter(_.contains(" | ")) shouldBe empty
  }

  it should "leave out the comedy night and the surprise-film club the page also sells" in {
    byTitle.keySet should not contain "Nova Laughs"
    byTitle.keySet should not contain "Mystery Movie Club"
    byTitle.keySet should contain ("National Theatre Live: Golden Boy")
  }

  it should "drop screenings before today" in {
    scrape(LocalDate.of(2026, 10, 14)).flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 14)
  }

  "NovaCinemaClient.parse" should "refuse a page without the films list rather than call it an empty programme" in {
    an[IllegalArgumentException] should be thrownBy
      NovaCinemaClient.parse("<html><body>Maintenance</body></html>", NovaCinemaWoking, today)
  }
}
