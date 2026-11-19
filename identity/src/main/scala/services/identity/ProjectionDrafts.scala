package services.identity

import models.ListingKey
import services.movies.LatestTitleYear
import services.titles.TitleNormalizer

/** [[IdentityProjectionPlan.draftOf]] and [[FilmKeys.finish]] over one country's title normalizer and its venue slots'
 *  [[VenueSlotSource]] (which the write completes a draft by): how a projection drafts its films, tick after tick (the worker's
 *  `ProjectionDrafter`, which keeps the venue slot memo and the films' shapes between projections). */
final class ProjectionDrafts(normalizer: TitleNormalizer, val slotSource: VenueSlotSource)(using LatestTitleYear) {
  def draftOf(index: ProjectionIndex, scope: ProjectionScope, memo: VenueSlotMemo, shapes: FilmShapes,
              changed: Set[ListingKey]): ProjectionDraft =
    IdentityProjectionPlan.draftOf(index, scope, normalizer, slotSource, memo, shapes, changed)
  /** `draft` titled and keyed, a fresh id never one a film stored in `index` holds. */
  def finish(draft: ProjectionDraft, index: ProjectionIndex): ProjectionPlan = {
    val storedIds = index.storedById.valuesIterator.map(_.id).toSet
    FilmKeys.finish(draft, normalizer, storedIds)
  }
}
