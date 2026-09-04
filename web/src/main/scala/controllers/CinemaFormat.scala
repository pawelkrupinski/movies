package controllers

import models.Cinema
import services.movies.ScreeningTokens

/**
 * The format tokens a showtime pill may DROP: the ones every slot of one film at
 * one cinema shares, minus the language version, which every pill keeps.
 *
 * A pill repeating what the whole cinema shares reads as noise and costs width
 * the two-per-row mobile layout does not have — six pills all saying `2D` tell a
 * visitor nothing. But the LANGUAGE VERSION is never noise: it is the thing a
 * visitor is choosing between, and it belongs on the showtime they are about to
 * tap. Dropping it because a cinema happened to be uniform is what hid napisy vs
 * dubbing on 334 of Poznań's 426 Multikino slots, so it is excluded from the
 * strip whether it is common or not.
 */
object CinemaFormat {

  /** Per cinema of `film`, the tokens its pills may drop, in `vocabulary`'s
   *  spelling of the version tokens.
   *
   *  Format-less slots sit out the intersection: a slot with no tokens says
   *  nothing about what the rest have in common, and letting one veto the strip
   *  would put `2D` back on every pill around it. */
  def byCinema(film: FilmSchedule, vocabulary: ScreeningTokens): Map[Cinema, Set[String]] =
    film.showings.flatMap(_._2).groupBy(_.cinema).map { case (cinema, showings) =>
      val fmts = showings.flatMap(_.showtimes).map(_.format.toSet).filter(_.nonEmpty)
      cinema -> fmts.reduceOption(_ intersect _).getOrElse(Set.empty)
        .filterNot(vocabulary.isLanguageVersion)
    }
}
