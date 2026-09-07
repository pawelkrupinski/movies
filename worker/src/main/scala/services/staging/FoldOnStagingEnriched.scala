package services.staging

import models.MovieRecord
import services.events.{DomainEvent, StagingFilmEnriched}
import services.movies.CacheKey

/**
 * What happens the moment a newcomer's staging chain concludes: the
 * `StagingFold` step publishes [[StagingFilmEnriched]] and this subscriber folds
 * the film's group into `movies` right then (group-scoped, settling as it goes),
 * rather than leaving it for a periodic sweep.
 *
 * Two decisions live here and nowhere else:
 *
 *   - WHICH rows the fold reads. The event names a title; the group is every
 *     staged row sharing its anchor, and the fold is handed exactly those ids so
 *     it reads them instead of the whole collection (`StagingFolder.foldGroup`'s
 *     `candidateIds` — a hint and a superset, never the selection itself).
 *   - WHAT is announced afterwards. Only the BRAND-NEW films the fold introduced
 *     (no pre-existing `movies` row merged in) are handed to `announceNewMovie`,
 *     which re-publishes their resolution outcome (so a TMDB-only hit kicks
 *     IMDb-id recovery) and enqueues their now-eligible rating tasks — a trickle,
 *     not the corpus-wide burst the reaper's cap smooths. A merge into an
 *     existing row keeps that row's ratings, so it is left untouched.
 *
 * Anchors are derived with the rules the repository keyed its rows under
 * (`staging.normalizer`), for the same reason [[StagingReaper]] does: an anchor
 * folded any other way names a group that isn't there.
 */
class FoldOnStagingEnriched(
  folder:           StagingFolder,
  staging:          StagingRepository,
  announceNewMovie: (CacheKey, MovieRecord) => Unit) {

  /** The bus subscription: fold on [[StagingFilmEnriched]], nothing else. */
  def onStagingFilmEnriched: PartialFunction[DomainEvent, Unit] = {
    case StagingFilmEnriched(title) => fold(title); ()
  }

  /** Fold `title`'s group and announce what it introduced. Returns the brand-new
   *  films, as the folder reported them. */
  def fold(title: String): Seq[(CacheKey, MovieRecord)] = {
    val group    = staging.findByAnchor(staging.normalizer.sanitize(title)).map(_.id).toSet
    val promoted = folder.foldGroup(title, Some(group))
    promoted.foreach { case (key, record) => announceNewMovie(key, record) }
    promoted
  }
}
