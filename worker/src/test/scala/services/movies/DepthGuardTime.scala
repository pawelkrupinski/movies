package services.movies

import models.Showtime

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * The time a depth-guard spec runs at, and the showtimes it scrapes.
 *
 * The depth guard counts only showtimes still AHEAD of the cache's clock
 * (`ScrapeLanding`), so a spec whose showtimes are fixed dates but whose cache reads
 * the system clock passes only until those dates go by, and then every
 * rejection it asserts silently turns into an accept. Pin the cache to [[clock]]
 * and draw showtimes from [[showtimes]], which all start after it.
 */
object DepthGuardTime {

  /** A week before the first of [[showtimes]]. */
  val Now: Instant = Instant.parse("2027-06-01T00:00:00Z")

  val clock: Clock = Clock.fixed(Now, ZoneOffset.UTC)

  /** `count` distinct screenings, twelve a day from 2027-06-08, so a count scales
   *  past a single day's opening hours. */
  def showtimes(count: Int): Seq[Showtime] =
    (0 until count).map(n => Showtime(LocalDateTime.parse(f"2027-06-${8 + n / 12}%02dT${8 + n % 12}%02d:00"), None))
}
