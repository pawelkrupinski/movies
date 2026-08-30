package tools

import controllers.{CinemaShowtimes, FilmSchedule}
import models.Cinema

/**
 * A corpus schedule re-seated at many cinemas on a single date.
 *
 * The /movie page folds BOTH its cinema lists past the tenth — the link pills
 * under the title and each date's cinemas in the showings tree (a London film
 * runs 60+ venues a day) — but the Poznań fixture corpus tops out at a handful,
 * so neither page-test harness can reach either fold off a real corpus film.
 * Both the Scala CDP spec and the Playwright fixture server build their fold
 * fixture here, so the shape they drive can't drift apart.
 */
object ManyCinemaFilm {

  /** `base` with its first date re-seated at `count` distinct cinemas, each
   *  carrying that date's first showtime, and a link pill per cinema so the
   *  row under the title folds too. */
  def apply(base: FilmSchedule, count: Int = 12): FilmSchedule = {
    val (date, cinemaShowtimes) = base.showings.head
    val slot    = cinemaShowtimes.head.showtimes.take(1)
    val cinemas = Cinema.all.distinct.take(count)
    base.copy(
      showings       = Seq(date -> cinemas.map(CinemaShowtimes(_, slot))),
      cinemaFilmUrls = cinemas.map(c => c -> s"https://example.test/${c.pillName}"))
  }
}
