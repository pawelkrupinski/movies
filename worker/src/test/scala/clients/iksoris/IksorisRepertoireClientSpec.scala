package clients.iksoris

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.{KinoPlon, KinoSokolniaKepno}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{IksorisOrigin, IksorisRepertoireClient}
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays 2026-09-27 captures of two venues' iKsoris day-by-day repertoire
 *  (`repertuar.html?data=YYYY-MM-DD`, today's page plus every day its picker
 *  links) — Kino Sokolnia in Kępno (`termin-box` skin) and Kino Plon in
 *  Hrubieszów (`termin` skin).
 *
 *  Fixture directories: test/resources/fixtures/kino-sokolnia-kepno/ and
 *  test/resources/fixtures/kino-plon/ (recorded with RecordingHttpFetch over
 *  RealHttpFetch). */
class IksorisRepertoireClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today = LocalDate.of(2026, 9, 27)

  private val kepno = new IksorisRepertoireClient(
    new FakeHttpFetch("kino-sokolnia-kepno"), IksorisOrigin("https://kinosokolnia.org"), KinoSokolniaKepno, today).fetch()
  private val plon = new IksorisRepertoireClient(
    new FakeHttpFetch("kino-plon"), IksorisOrigin("https://kinoplon.pl"), KinoPlon, today).fetch()

  "IksorisRepertoireClient (termin-box skin, Kino Sokolnia)" should "read every day the picker links" in {
    kepno.map(_.cinema).toSet shouldBe Set(KinoSokolniaKepno)
    kepno.size shouldBe 10
    kepno.flatMap(_.showtimes).size shouldBe 55
    kepno.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2026, 10, 29)
  }

  it should "pin a showing's time, Kup booking link, runtime and poster" in {
    val luna = kepno.find(_.movie.title == "LUNA I ROZGADANA ŚWINKA").value
    luna.showtimes.head shouldBe models.Showtime(
      LocalDateTime.of(2026, 9, 27, 14, 30),
      Some("https://kinosokolnia.org/rezerwacja/numerowane.html?id=7530&identyfikator=18ff63c123c0d6fec7aaeee8c6295225&d=4"))
    luna.movie.runtimeMinutes.value shouldBe 90
    luna.posterUrl.value shouldBe "https://kinosokolnia.org/images/wydarzenia/full/luna_i_rozgadana_swinka_b1_1.jpg"
  }

  it should "fall back to the Rezerwuj link for a reservation-only showing" in {
    val niePatrz = kepno.find(_.movie.title == "NIE PATRZ W DÓŁ 2").value
    niePatrz.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 27, 19, 30)).value.bookingUrl.value shouldBe
      "https://kinosokolnia.org/rezerwacja/numerowane.html?id=7501&identyfikator=02fda21fc374753733e0ef4c5a013464&d=3"
  }

  it should "strip a version tag, keeping the raw title" in {
    val resident = kepno.find(_.movie.title == "RESIDENT EVIL").value
    resident.movie.rawTitle.value shouldBe "RESIDENT EVIL - napisy"
  }

  // The stripped version tag is the showing's language version — the sibling
  // iKsoris clients (booking, calendar) keep it on the showtime; this one threw
  // it away, so a subtitled showing read as an undifferentiated one.
  it should "carry the stripped version tag onto each showtime's format" in {
    kepno.find(_.movie.title == "RESIDENT EVIL").value.showtimes.map(_.format).toSet shouldBe Set(List("NAP"))
  }

  "IksorisRepertoireClient (termin skin, Kino Plon)" should "read every day the picker links, however far ahead" in {
    plon.map(_.cinema).toSet shouldBe Set(KinoPlon)
    plon.size shouldBe 10
    plon.flatMap(_.showtimes).size shouldBe 20
    plon.find(_.movie.title == "Dziadek do orzechów").value.showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 12, 19, 18, 0))
  }

  // Plon sells its concerts on the same repertoire ("CZERWONE GITARY. Diamentowy
  // koncert 60-lecia na bis"), which went to TMDB as a film: the client never
  // applied the live-event filter its doc said a scrape seam would.
  it should "drop a live concert sold on the same repertoire" in {
    plon.map(_.movie.title).filter(_.toLowerCase.contains("koncert")) shouldBe empty
  }

  it should "pin a showing's time, seat-picker link and runtime" in {
    val psiPatrol = plon.find(_.movie.title == "Psi Patrol i Dinozaury").value
    psiPatrol.showtimes shouldBe Seq(models.Showtime(
      LocalDateTime.of(2026, 9, 27, 15, 0),
      Some("https://kinoplon.pl/rezerwacja/numerowane.html?id=4412&identyfikator=aaa3fff3175538e2c3691b3bb4563135")))
    psiPatrol.movie.runtimeMinutes.value shouldBe 88
  }

  "IksorisRepertoireClient" should "propagate a failure of today's page instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy new IksorisRepertoireClient(
      new FailingHttpFetch(503), IksorisOrigin("https://kinoplon.pl"), KinoPlon, today).fetch()
  }
}
