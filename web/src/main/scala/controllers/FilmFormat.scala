package controllers

/**
 * The format tokens a showtime pill may DROP: the ones every slot of the film on
 * this card shares.
 *
 * A pill repeating what the whole card shares reads as noise and costs width the
 * two-per-row mobile layout does not have — six pills all saying `2D` tell a
 * visitor nothing, and neither do six all saying `NAP` when the film screens no
 * other way. What is left on a pill is what actually separates that slot from
 * the ones beside it, which is the only thing a visitor is choosing between.
 *
 * The comparison spans the WHOLE film — every cinema, every day — because that
 * is the span a reader compares across. A film Multikino screens dubbed and
 * Helios subtitled is mixed even though neither cinema is, and both keep their
 * tag. Same rule as iOS's and Android's `FormatTokenFilter`.
 */
object FilmFormat {

  /** The tokens every slot of `film` carries, and so the ones its pills may drop.
   *
   *  Format-less slots sit out the intersection: a slot with no tokens says
   *  nothing about what the rest have in common, and letting one veto the strip
   *  would put `2D` back on every pill around it. */
  def tokensToStrip(film: FilmSchedule): Set[String] = {
    val fmts = film.showings.flatMap(_._2).flatMap(_.showtimes).map(_.format.toSet).filter(_.nonEmpty)
    fmts.reduceOption(_ intersect _).getOrElse(Set.empty)
  }
}
