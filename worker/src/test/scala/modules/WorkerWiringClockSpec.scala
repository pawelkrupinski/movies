package modules

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.TestWiring

/** The composition root has ONE clock, and every component that asks "what is now?" is handed it.
 *
 *  US convergence, 2026-09-25: the next-day test moved the harness clock a day on, but the movie
 *  cache kept its own default clock, so the depth guard counted showtimes the day had already
 *  passed as still upcoming and DISCARDED the next day's listing as degraded — a venue's new film
 *  never landed and a withdrawn one kept its showtime. Production's root clock is the system
 *  clock either way; the US sample's next-day test is the behaviour this pins. */
class WorkerWiringClockSpec extends AnyFlatSpec with Matchers {

  "the worker's composition root" should "hand the movie cache its own clock, so the scrape guards judge by the same now" in {
    val moving = new tools.MutableClock(java.time.Instant.parse("2030-01-01T00:00:00Z"))
    val wiring = new WorkerWiring(Country.default) with TestWiring {
      override lazy val clock: java.time.Clock = moving
    }
    try wiring.movieCache.clock should be theSameInstanceAs moving
    finally wiring.stop()
  }
}
