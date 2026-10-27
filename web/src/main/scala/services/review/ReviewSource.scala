package services.review

import services.identity.ResolverDecision

/**
 * The storage seam of ONE country's review reads: the reads, and nothing about what they mean.
 * Every read is bounded by the keys it is given, or paged.
 */
trait ReviewSource {
  /** The model's decisions — only those of families holding an unmatched one when `unmatchedOnly`. */
  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision]
  /** For each corpus record naming one of `refs` (by its TMDB, IMDb, Filmweb, RT or Metacritic id), every ref it names. */
  def filmLinks(refs: Seq[FilmRef]): Seq[Set[FilmRef]]
  /** What prod serves each listing as, by [[services.movies.ListingKey.serialised]]: the TMDB film of its screening row's
   *  film (`screenings.filmId` → `movies.tmdbId`), `None` for a listing served with no TMDB film. A listing with no
   *  screening row is absent. The model's decision passes through the agreement stage and the projection on its way
   *  here, so a listing served otherwise than its cluster was decided is one the two stages disagree on. Read whole. */
  def served(): Map[String, Option[Int]]
}
