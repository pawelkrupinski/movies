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
 * The corpus's OWN folds ask it too, and their two tolerances are not any site's
 * but the pipeline's, so they live here by name: [[ProductionToRelease]] for how far
 * a film's rows may sit apart, [[PublishedAdjacency]] for how far two cinemas' years
 * for one film may. Each is read from more than one place that has to agree.
 *
 * Silence is the load-bearing part. A missing year on either side is not evidence
 * of anything, so [[agrees]] returns None rather than a verdict, and each caller
 * decides what silence means for it: a slug PROBE abstains (Metacritic's
 * `yearsCompatible`), a SEARCH hit on a site whose pages routinely publish no year
 * must positively confirm (`yearConfirms`).
 */
object YearWindow {

  /** How far a film's OWN rows may sit apart in year and still be one film: a cinema
   *  prints the PRODUCTION year where TMDB holds the release ("Zawieście czerwone
   *  latarnie", 1989 against 1991). Wide enough for that gap and no wider — a
   *  same-titled remake still awaiting its own tmdbId must not be swallowed. Read by
   *  the settle's cluster attach (`FilmCanonicalizer.clusterByFilm` rule 2) AND by the
   *  scrape landing (`MovieCache.concludedKeyFor`), which have to agree: a listing
   *  landed outside the settle's window is a row the settle folds a tick later, and
   *  one refused inside it is a row the settle never folds. */
  val ProductionToRelease: Int = 2

  /** How far two CINEMAS' published years for one film may sit apart — a festival
   *  year beside a release year. The corroboration `MixedFilmDetector` asks of a
   *  differing title when no runtime can speak, and the width of the settle's windows
   *  over rows nothing has resolved (`clusterByFilm` rule 3). */
  val PublishedAdjacency: Int = 1

  /** |ours − theirs|, for a caller that ranks candidates by NEARNESS within a window
   *  rather than asking a yes/no of one. */
  def distance(ours: Int, theirs: Int): Int = math.abs(ours - theirs)

  /** Some(true) when some year of `ours` lies within `tolerance` of `theirs`,
   *  Some(false) when both sides are known and none does, None when either side
   *  is absent. `ours` is a collection because a film can legitimately carry more
   *  than one year — its TMDB year and a cinema's own — and agreeing with any of
   *  them is agreement. */
  def agrees(ours: Iterable[Int], theirs: Option[Int], tolerance: Int): Option[Boolean] =
    theirs.filter(_ => ours.nonEmpty).map(t => ours.exists(distance(_, t) <= tolerance))

  def agrees(ours: Option[Int], theirs: Option[Int], tolerance: Int): Option[Boolean] =
    agrees(ours.toList, theirs, tolerance)

  /** [[agrees]] between two SETS of years — what two rows' cinemas published:
   *  Some(true) when any pair lies within `tolerance`, Some(false) when both sides
   *  are known and no pair does, None when either side published nothing. */
  def agrees(ours: Iterable[Int], theirs: Iterable[Int], tolerance: Int): Option[Boolean] =
    Option.when(ours.nonEmpty && theirs.nonEmpty)(theirs.exists(t => ours.exists(distance(_, t) <= tolerance)))

  /** Both sides known and every year of `ours` further than `tolerance` from
   *  `theirs` — positive evidence of a different film. Never true on silence. */
  def contradicts(ours: Iterable[Int], theirs: Option[Int], tolerance: Int): Boolean =
    agrees(ours, theirs, tolerance).contains(false)

  def contradicts(ours: Option[Int], theirs: Option[Int], tolerance: Int): Boolean =
    contradicts(ours.toList, theirs, tolerance)

  def contradicts(ours: Iterable[Int], theirs: Iterable[Int], tolerance: Int): Boolean =
    agrees(ours, theirs, tolerance).contains(false)
}
