package controllers

import models.{Cinema, Showtime}
import services.movies.ScreeningTokens

/**
 * The format tokens EVERY slot of one film at one cinema carries, split into the
 * part a showtime pill may drop and the part its cinema label has to say.
 *
 * A pill repeating what the whole cinema shares reads as noise and costs width
 * the two-per-row mobile layout does not have, so `_filmShowings` strips
 * [[common]] from each badge. That is right for a screen format — six pills all
 * saying `2D` tell a visitor nothing — but it was silently swallowing the
 * LANGUAGE VERSION too: a Multikino film screened only dubbed has every slot
 * tagged `2D DUB`, the intersection ate `DUB`, and the page could no longer tell
 * napisy from dubbing (334 of Poznań's 426 Multikino slots on 2026-09-04, and
 * the same shape at every other chain).
 *
 * So the version is not dropped, it is MOVED: [[version]] is the part of
 * [[common]] that names what you will hear and read, rendered once beside the
 * cinema's name instead of once per pill. Where the version DIFFERS between
 * slots it is not common, stays on each badge, and [[version]] is empty — which
 * is exactly where a per-slot badge is the only thing that can carry it.
 */
case class CinemaFormat(common: Set[String], version: List[String])

object CinemaFormat {

  val Empty: CinemaFormat = CinemaFormat(Set.empty, Nil)

  /** What each of `film`'s cinemas shares across ALL its slots, in `vocabulary`'s
   *  spelling of the version tokens.
   *
   *  The intersection runs over every slot INCLUDING format-less ones: a token
   *  is only common — and so only safe to state once in the label — when nothing
   *  at that cinema lacks it. */
  def byCinema(film: FilmSchedule, vocabulary: ScreeningTokens): Map[Cinema, CinemaFormat] =
    film.showings.flatMap(_._2).groupBy(_.cinema).map { case (cinema, showings) =>
      val slots  = showings.flatMap(_.showtimes)
      val common = slots.map(_.format.toSet).reduceOption(_ intersect _).getOrElse(Set.empty)
      cinema -> CinemaFormat(common, versionOf(slots, common, vocabulary))
    }

  /** The common version tokens in the SOURCE's own order — `common` is a Set, and
   *  a label reading `NAP ATMOS` where the source wrote `ATMOS NAP` is a needless
   *  re-spelling. Every slot carries all of them, so the first one speaks for all. */
  private def versionOf(
    slots: Seq[Showtime], common: Set[String], vocabulary: ScreeningTokens): List[String] =
    slots.headOption.toList.flatMap(_.format.filter(t => common(t) && vocabulary.isLanguageVersion(t)))
}
