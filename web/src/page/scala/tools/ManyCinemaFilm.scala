package tools

import controllers.{CinemaShowtimes, FilmSchedule}
import models.Cinema

/**
 * A corpus schedule re-seated at many cinemas on a single date.
 *
 * The /film page folds a date's cinemas past the tenth (a London film runs
 * 60+ venues a day), but the Poznań fixture corpus tops out at a handful — so
 * neither page-test harness can reach the fold off a real corpus film. Both
 * the Scala CDP spec and the Playwright fixture server build their fold
 * fixture here, so the shape they drive can't drift apart.
 */
object ManyCinemaFilm {

  /** `base` with its first date re-seated at `count` distinct cinemas, each
   *  carrying that date's first showtime. */
  def apply(base: FilmSchedule, count: Int = 12): FilmSchedule = {
    val (date, cinemaShowtimes) = base.showings.head
    val slot = cinemaShowtimes.head.showtimes.take(1)
    base.copy(showings = Seq(date -> Cinema.all.distinct.take(count).map(CinemaShowtimes(_, slot))))
  }
}
