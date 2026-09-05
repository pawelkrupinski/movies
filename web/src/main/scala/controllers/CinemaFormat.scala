package controllers

import models.Cinema

/**
 * The format tokens a showtime pill may DROP: the ones every slot of one film at
 * one cinema shares.
 *
 * A pill repeating what the whole cinema shares reads as noise and costs width
 * the two-per-row mobile layout does not have — six pills all saying `2D` tell a
 * visitor nothing, and neither do six all saying `NAP` when the cinema screens
 * the film no other way. What is left on a pill is what actually separates that
 * slot from the one beside it, which is the only thing a visitor is choosing
 * between. Same rule as iOS's and Android's `FormatTokenFilter`.
 */
object CinemaFormat {

  /** Per cinema of `film`, the tokens its pills may drop.
   *
   *  Format-less slots sit out the intersection: a slot with no tokens says
   *  nothing about what the rest have in common, and letting one veto the strip
   *  would put `2D` back on every pill around it. */
  def byCinema(film: FilmSchedule): Map[Cinema, Set[String]] =
    film.showings.flatMap(_._2).groupBy(_.cinema).map { case (cinema, showings) =>
      val fmts = showings.flatMap(_.showtimes).map(_.format.toSet).filter(_.nonEmpty)
      cinema -> fmts.reduceOption(_ intersect _).getOrElse(Set.empty)
    }
}
