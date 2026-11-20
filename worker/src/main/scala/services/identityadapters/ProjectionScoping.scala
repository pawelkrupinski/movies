package services.identityadapters

import models.{Cinema, ListingKey, StoredMovieRecord}
import services.identity.*
import services.identity.measures.ResolverDecision
import services.evidence.LatestTitleYear
import services.titles.TitleNormalizer

/** The SCOPE stage of THE IDENTITY PROJECTION ([[IdentityProjection]]): the index of listings, stored films and clusters,
 *  kept from one projection to the next and moved where its inputs moved ([[LiveProjectionIndex]]), and what those moves
 *  reach — the scope a projection drafts ([[ProjectionScope.close]]). What moved under a projection refused and never
 *  written is carried to the next. `pagesMoved`: the venue pages whose read moved since the last call
 *  (`IndexedVenuePageFacts.takeMoved`), None when the feed no longer holds every filing since. */
final class ProjectionScoping(normalizer: TitleNormalizer, pagesMoved: () => Option[Seq[(Cinema, String)]])(using LatestTitleYear) {
  private var index = new LiveProjectionIndex(normalizer)
  /** What moved under projections that were refused and wrote nothing: still to project. */
  private var carried = ProjectionScope.Changes.none

  /** The index as it stands: what a written projection's change list is read off ([[LiveProjectionIndex.written]]), and
   *  the take reports read the listings and their prior takes off. */
  def live: LiveProjectionIndex = index

  /** The kept index over `counters`: what a spec holds to the index built afresh from the same corpus. */
  def keptIndex(counters: FilmIdCounters): ProjectionIndex = index.index(counters)

  /** What a venue re-published with a poster, days or names the model's decisions do not move with. */
  def republished(byVenue: Seq[(String, Seq[ProjectedListing])]): Seq[ListingKey] = index.republished(byVenue)

  /** Nothing written to build on: the next scope is read afresh. */
  def reset(): Unit = { index = new LiveProjectionIndex(normalizer); carried = ProjectionScope.Changes.none }

  /** The index moved under `venues` as the resolution decided them (`decisions`, of the listings `held`), and — when
   *  `building` on what the last projection wrote — the scope what moved reaches, closed over every film a moved one's
   *  draft can move. Exactly the listings the resolution decided: one that reached the intake after the model's snapshot
   *  is projected by the next tick, never left out of its film by this one. */
  def apply(venues: Seq[(Cinema, Seq[ProjectedListing])], byVenue: Seq[(String, Seq[ProjectedListing])], held: Iterable[ListingKey],
            decisions: Seq[ResolverDecision], stored: Seq[StoredMovieRecord], counters: FilmIdCounters, building: Boolean,
            phases: ProjectionPhases): ProjectionScoping.Scoped = {
    val updated = phases("index")(index.update(byVenue, held, decisions, stored) ++ pageChanges(venues))
    val now     = index.index(counters)
    // None when the last projection did not write all it planned, the FilmId map is not the one it left, or a stored
    // film is not numbered yet — each re-decided only by projecting the whole corpus.
    val changes = Option.when(building && now.additions.isEmpty)(carried ++ updated ++ ProjectionScope.standing(now))
    ProjectionScoping.Scoped(updated, now, changes, changes.map(c => phases("scope")(ProjectionScope.close(now, c))))
  }

  /** The guard held the projection back: what moved under it is still to project. */
  def carry(updated: ProjectionScope.Changes): Unit = carried = carried ++ updated

  /** Every write of the projection was taken: nothing is carried to the next. */
  def committed(): Unit = carried = ProjectionScope.Changes.none

  /** The listings whose venue page moved since the last projection took them ([[pagesMoved]]): each one's slot is built
   *  from the page as it is read now. Taken before the draft reads a page, so a page moving after is the next one's. */
  private def pageChanges(venues: Seq[(Cinema, Seq[ProjectedListing])]): ProjectionScope.Changes = {
    val moved = pagesMoved().map(_.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap)
    if (moved.exists(_.isEmpty)) ProjectionScope.Changes.none
    else ProjectionScope.Changes(venues.iterator.flatMap { case (cinema, listings) =>
      // the feed no longer holding every filing since: any page may have moved
      val pages = moved.fold(Option[String => Boolean](_ => true))(_.get(cinema))
      pages.fold(Iterator.empty[ListingKey])(onPage => listings.iterator.map(_.listing.key).filter {
        case ListingKey.Native(_, page, _) => onPage(page)
        case _                             => false
      })
    }.toSet, Set.empty)
  }
}

object ProjectionScoping {
  /** One projection's scope: what the index moved (`updated`), the index, the changes it builds on (none: the whole corpus
   *  is projected) and the scope they close over. */
  final case class Scoped(updated: ProjectionScope.Changes, index: ProjectionIndex, changes: Option[ProjectionScope.Changes],
                          moved: Option[ProjectionScope]) {
    /** The film keys the changes freed. */
    def freed: Set[String] = changes.fold(Set.empty[String])(_.keys)
    /** The listings the changes name. */
    def listings: Set[ListingKey] = changes.fold(Set.empty[ListingKey])(_.listings)
    /** The stored films another writer moved. */
    def movedElsewhere: Set[String] = changes.fold(Set.empty[String])(_.films)
  }
}
