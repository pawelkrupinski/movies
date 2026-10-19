package services.cinemas

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.NearTermProgramme

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * A listing that has screenings but none in the next three days is the shape
 * that hid Kino Polonez (2026-09-26): Filmweb 320 held four "Lalka" pre-sale
 * slots 4.7 days out and nothing before them, while the venue sold 50 screenings
 * that week on biletyna — a green bar over a feed that had stopped carrying the
 * programme.
 */
class NearTermProgrammeSpec extends AnyFlatSpec with Matchers {

  // 10:00 UTC = 12:00 in Warsaw, Multikino's zone.
  private val clock    = Clock.fixed(Instant.parse("2026-09-25T10:00:00Z"), ZoneOffset.UTC)
  private val localNow = LocalDateTime.parse("2026-09-25T12:00")

  private def listing(times: LocalDateTime*): Seq[CinemaMovie] = Seq(CinemaMovie(
    movie = Movie("Lalka"), cinema = Multikino, posterUrl = None, filmUrl = None, synopsis = None,
    cast = Seq.empty, director = Seq.empty, showtimes = times.map(t => Showtime(t, None))))

  "NearTermProgramme.isThin" should "flag screenings that only start after the next 72 hours (the Polonez shape)" in {
    NearTermProgramme.isThin(Multikino, listing(localNow.plusHours(112), localNow.plusHours(115)), clock) shouldBe true
  }

  it should "not flag a listing with a screening inside the next 72 hours" in {
    NearTermProgramme.isThin(Multikino, listing(localNow.plusHours(71), localNow.plusDays(9)), clock) shouldBe false
  }

  it should "flag a listing whose only showtimes have already passed" in {
    NearTermProgramme.isThin(Multikino, listing(localNow.minusHours(2)), clock) shouldBe true
  }

  it should "leave an empty listing alone — a white bar already says it" in {
    NearTermProgramme.isThin(Multikino, Seq.empty, clock) shouldBe false
  }

  // Showtimes are city-local wall-clock times: judged against a UTC "now", a
  // screening 71h away in Warsaw would read as 73h away and be wrongly flagged.
  it should "judge the window on the venue's own clock, not UTC" in {
    NearTermProgramme.isThin(Multikino, listing(localNow.plusHours(71).plusMinutes(30)), clock) shouldBe false
  }
}
