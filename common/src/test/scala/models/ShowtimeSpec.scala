package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * Locks the grace-window rule both the web's `toSchedules` list filter and the
 * worker's `kinowo_worker_movies_served` gauge count by, so the two can't drift on
 * the "just-started screening still counts" edge.
 */
class ShowtimeSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 6, 8, 18, 0)
  private def at(t: LocalDateTime) = Showtime(t, bookingUrl = None)

  "isUpcoming" should "include a showtime in the future" in {
    at(now.plusHours(1)).isUpcoming(now) shouldBe true
  }

  it should "include a showtime that started within the 30-minute grace" in {
    at(now.minusMinutes(29)).isUpcoming(now) shouldBe true
  }

  it should "exclude a showtime past the grace window" in {
    at(now.minusMinutes(31)).isUpcoming(now) shouldBe false
  }

  it should "exclude a showtime exactly at the grace boundary (strictly after)" in {
    // dateTime.isAfter(now - 30min): the boundary itself is not after, so it drops.
    at(now.minus(Showtime.Grace)).isUpcoming(now) shouldBe false
  }
}
