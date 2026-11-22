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

  /** The tokens that say a slot's dimension. IMAX, 4DX, ScreenX and the rest are projection systems the
   *  vocabulary pairs with either (`IMAX 3D`, `4DE 3D`), not dimensions of their own. */
  val DimensionTokens: Set[String] = Set("2D", "3D")

  /** The dimension of a slot that names none. */
  val ImpliedDimension = "2D"

  /** The tokens every slot of `film` carries, and so the ones its pills may drop.
   *
   *  A slot naming no dimension counts as [[ImpliedDimension]]: Kino Rialto writes `DUB` where Helios writes
   *  `2D DUB`, and two such slots kept `2D` on all 69 other pills of a film screened in 2D everywhere.
   *  Format-less slots sit out of the intersection: a slot with no tokens says nothing about what the rest have
   *  in common, and letting one veto the strip would put `2D` back on every pill around it. Same rule, held to
   *  the same vectors (`test/resources/format-token-strip.json`), as the apps' `FormatTokenFilter`. */
  def tokensToStrip(film: FilmSchedule): Set[String] = {
    val fmts = film.showings.flatMap(_._2).flatMap(_.showtimes).map(_.format.toSet).filter(_.nonEmpty).map(withDimension)
    fmts.reduceOption(_ intersect _).getOrElse(Set.empty)
  }

  private def withDimension(tokens: Set[String]): Set[String] =
    if (tokens.exists(DimensionTokens.contains)) tokens else tokens + ImpliedDimension
}
