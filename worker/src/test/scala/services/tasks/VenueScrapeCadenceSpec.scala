package services.tasks

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._

/**
 * Multikino is Poznań's, `Europe/Warsaw` — deliberately not UTC, so a test that
 * passed only because the venue's zone happens to coincide with the clock's own
 * couldn't hide the exact bug this exists to catch: `Showtime.dateTime` is
 * city-local wall-clock time, and comparing it against a bare UTC "now" would
 * misjudge every non-Polish venue by its own zone offset (durant/moab/zamora
 * are all several hours off UTC). `now` below is built the same way
 * `remainingHorizonOf` builds its own, so the two stay self-consistent across
 * any DST boundary without hard-coding the current UTC+1/+2 offset.
 */
class VenueScrapeCadenceSpec extends AnyFlatSpec with Matchers {

  private val fixedClock = Clock.fixed(Instant.parse("2026-09-08T10:00:00Z"), ZoneOffset.UTC)
  private val now        = java.time.LocalDateTime.now(fixedClock.withZone(models.City.forCinema(Multikino).get.zoneId))

  private def scrapeAt(hoursFromNow: Double*): Seq[CinemaMovie] =
    hoursFromNow.zipWithIndex.map { case (h, i) =>
      CinemaMovie(Movie(s"Film $i"), Multikino, posterUrl = None, filmUrl = None, synopsis = None,
        cast = Nil, director = Nil,
        showtimes = Seq(Showtime(now.plusMinutes((h * 60).toLong), bookingUrl = None)))
    }

  "remainingHorizonOf" should "measure to the LATEST showtime across every film, not the first" in {
    val movies = scrapeAt(1, 5, 3)
    VenueScrapeCadence.remainingHorizonOf(Multikino, movies, fixedClock) shouldBe 5.hours
  }

  it should "be zero for an empty listing" in {
    VenueScrapeCadence.remainingHorizonOf(Multikino, Seq.empty, fixedClock) shouldBe Duration.Zero
  }

  it should "be zero, not negative, when every showtime has already passed" in {
    VenueScrapeCadence.remainingHorizonOf(Multikino, scrapeAt(-2, -1), fixedClock) shouldBe Duration.Zero
  }

  it should "measure on the venue's OWN city clock, not the caller's" in {
    // A showtime 30 minutes past Warsaw midnight, with `now` at Warsaw 23:00 — under a
    // bare UTC comparison (Warsaw is UTC+2 in September) this would misread as ~1.5h
    // short of a full day, rather than the ~1.5h it actually is.
    val warsawMidnightPlus30 = now.toLocalDate.plusDays(1).atTime(0, 30)
    val lateEvening          = now.toLocalDate.atTime(23, 0)
    val movie = CinemaMovie(Movie("Late Show"), Multikino, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Seq(Showtime(warsawMidnightPlus30, bookingUrl = None)))
    val clockAtLateEvening = Clock.fixed(
      lateEvening.atZone(models.City.forCinema(Multikino).get.zoneId).toInstant, ZoneOffset.UTC)
    VenueScrapeCadence.remainingHorizonOf(Multikino, Seq(movie), clockAtLateEvening) shouldBe 90.minutes
  }

  "periodFor" should "leave the country default alone when runway meets or exceeds it" in {
    VenueScrapeCadence.periodFor(remainingHorizon = 14.hours, countryDefault = 14.hours) shouldBe 14.hours
    VenueScrapeCadence.periodFor(remainingHorizon = 20.hours, countryDefault = 14.hours) shouldBe 14.hours
  }

  it should "halve the runway for a thin venue, so there's still margin if the next attempt is late" in {
    // durant's shape: ~7h left against a 14h US cadence.
    VenueScrapeCadence.periodFor(remainingHorizon = 7.hours, countryDefault = 14.hours) shouldBe 3.5.hours
  }

  it should "never go below the floor, however little runway is left" in {
    VenueScrapeCadence.periodFor(remainingHorizon = 10.minutes, countryDefault = 14.hours) shouldBe VenueScrapeCadence.MinInterval
    VenueScrapeCadence.periodFor(remainingHorizon = Duration.Zero, countryDefault = 14.hours) shouldBe VenueScrapeCadence.MinInterval
  }

  it should "never exceed the country default even at the floor, for a country faster than the floor" in {
    // No real country is this fast today (PL's own 60min already clears the 30min
    // floor), but the clamp order must not let MinInterval win over a smaller default.
    VenueScrapeCadence.periodFor(remainingHorizon = Duration.Zero, countryDefault = 10.minutes) shouldBe 10.minutes
  }

  "VenueCadenceStore" should "answer the country default for a venue it has never recorded" in {
    val store = new VenueCadenceStore(countryDefault = 14.hours)
    store.periodFor("scrape|Never Seen") shouldBe 14.hours
  }

  it should "answer the shortened period once a thin scrape is recorded" in {
    val store = new VenueCadenceStore(countryDefault = 14.hours)
    store.record("scrape|Cinema One Antlers", remainingHorizon = 7.hours)
    store.periodFor("scrape|Cinema One Antlers") shouldBe 3.5.hours
    store.periodFor("scrape|Some Other Venue") shouldBe 14.hours
  }

  it should "widen back toward the default once the same venue reports more runway" in {
    val store = new VenueCadenceStore(countryDefault = 14.hours)
    store.record("scrape|Slickrock Cinema Moab", remainingHorizon = 10.minutes)
    store.periodFor("scrape|Slickrock Cinema Moab") shouldBe VenueScrapeCadence.MinInterval
    store.record("scrape|Slickrock Cinema Moab", remainingHorizon = 28.days)
    store.periodFor("scrape|Slickrock Cinema Moab") shouldBe 14.hours
  }
}
