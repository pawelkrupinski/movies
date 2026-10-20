package services.movies

import models.{Cinema, CinemaMovie, CinemaNetworks, Source}

import java.time.LocalDateTime

/** What each venue of a recorded corpus bills the next day, for [[CountryConvergenceBehaviour]]'s next-day test. */
object NextDayListings {

  /** Each listing's showtimes from `cutoff` on; a listing whose showtimes all fall before it ends its run. One the
   *  venue billed with NO showtimes (a film page ahead of its dates) is billed the next day as it was: dropping it
   *  took its slot off a film whose listing had not changed (run 528: 'Kot prot' at Kino Parczew). */
  def of(today: Map[Cinema, Seq[CinemaMovie]], cutoff: LocalDateTime): Map[Cinema, Seq[CinemaMovie]] =
    today.map { case (c, fs) =>
      c -> fs.flatMap { cm =>
        val next = fromCutoff(cm, cutoff)
        Option.when(cm.showtimes.isEmpty || next.showtimes.nonEmpty)(next)
      }
    }

  def fromCutoff(cm: CinemaMovie, cutoff: LocalDateTime): CinemaMovie =
    cm.copy(showtimes = cm.showtimes.filterNot(_.dateTime.isBefore(cutoff)))

  /** A chain's network detail slot (`CinemaNetworks`), keyed by the bare network cinema or a showing of it. */
  def isNetworkSlot(source: Source): Boolean = source match {
    case models.CinemaShowing(c, _) => CinemaNetworks.all.contains(c)
    case c: Cinema                  => CinemaNetworks.all.contains(c)
    case _                          => false
  }
}
