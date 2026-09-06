package services.resolution

/**
 * Is a candidate's release year compatible with the year(s) we hold for a film?
 *
 * Every external-site matcher asks this — Metacritic and Rotten Tomatoes with a
 * 15-year window that absorbs a delayed regional release, Filmweb, OMDb, Cinemeta
 * and Wikidata with ±1 — and each used to compute it inline, so the SHAPE of the
 * answer ("both sides known and further apart than N") lived in six places.
 * The tolerance stays each site's own; only the arithmetic is shared.
 *
 * Silence is the load-bearing part. A missing year on either side is not evidence
 * of anything, so [[agrees]] returns None rather than a verdict, and each caller
 * decides what silence means for it: a slug PROBE abstains (Metacritic's
 * `yearsCompatible`), a SEARCH hit on a site whose pages routinely publish no year
 * must positively confirm (`yearConfirms`).
 */
object YearWindow {

  /** Some(true) when some year of `ours` lies within `tolerance` of `theirs`,
   *  Some(false) when both sides are known and none does, None when either side
   *  is absent. `ours` is a collection because a film can legitimately carry more
   *  than one year — its TMDB year and a cinema's own — and agreeing with any of
   *  them is agreement. */
  def agrees(ours: Iterable[Int], theirs: Option[Int], tolerance: Int): Option[Boolean] =
    theirs.filter(_ => ours.nonEmpty).map(t => ours.exists(y => math.abs(y - t) <= tolerance))

  def agrees(ours: Option[Int], theirs: Option[Int], tolerance: Int): Option[Boolean] =
    agrees(ours.toList, theirs, tolerance)

  /** Both sides known and every year of `ours` further than `tolerance` from
   *  `theirs` — positive evidence of a different film. Never true on silence. */
  def contradicts(ours: Iterable[Int], theirs: Option[Int], tolerance: Int): Boolean =
    agrees(ours, theirs, tolerance).contains(false)

  def contradicts(ours: Option[Int], theirs: Option[Int], tolerance: Int): Boolean =
    contradicts(ours.toList, theirs, tolerance)
}
