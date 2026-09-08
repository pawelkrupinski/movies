package tools

import controllers.{CinemaShowtimes, FilmSchedule}
import models.Showtime

import java.time.LocalDateTime

/**
 * A corpus's first schedule, re-seated with `count` synthetic showtimes
 * packed onto its first cinema, today's date-group — enough on its own to
 * push a city over `MovieControllerService.LargeCityShowtimeThreshold` so
 * the day-carousel's instant-swap path (`usesInstantDayChange` in
 * shared.js) can be driven without a real city's worth of scraped data.
 * Both the Scala CDP spec and the Playwright fixture server build their
 * large-city fixture here, so the shape they drive can't drift apart — the
 * same rule `ManyCinemaFilm` follows for the cinema-fold fixture.
 */
object ManyShowtimesCity {

  /** `schedules` with its first film's showings replaced by one date-group
   *  (`now`'s date, so it lands in the default "today" bucket) at one cinema
   *  carrying `count` synthetic showtimes, a minute apart starting tomorrow —
   *  safely upcoming under any fixture's pinned clock, so none of them are
   *  client-side pruned as expired. */
  def apply(schedules: Seq[FilmSchedule], now: LocalDateTime, count: Int = 10500): Seq[FilmSchedule] =
    schedules match {
      case head +: tail =>
        val (_, cinemaShowtimes) = head.showings.head
        val cinema    = cinemaShowtimes.head.cinema
        val anchor    = now.plusDays(1)
        val showtimes = (0 until count).map(i => Showtime(anchor.plusMinutes(i.toLong), bookingUrl = None))
        val inflated  = head.copy(showings = Seq(now.toLocalDate -> Seq(CinemaShowtimes(cinema, showtimes))))
        inflated +: tail
      case empty => empty
    }
}
