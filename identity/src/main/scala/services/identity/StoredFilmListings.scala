package services.identity

import models.ListingKey
import models.StoredMovieRecord
import services.evidence.LatestTitleYear
import services.identity.vocabulary.StoredSlots
import services.titles.TitleNormalizer

/** The stored films as the published listings' sets ([[ProjectionIndexing.index]]): each listing's previous film, and
 *  each stored film's listings. */
private[identity] object StoredFilmListings {
  def of(byKey: Map[ListingKey, ProjectedListing], stored: Seq[StoredMovieRecord], normalizer: TitleNormalizer)
      (using LatestTitleYear): (Map[ListingKey, StoredFilmRef], Map[String, Set[ListingKey]]) = {
    // A listing is on the stored film whose slot it IS (the slot's own listing key); a listing no slot
    // names — one a venue's same-title fold hid behind another's slot — on the film holding its slot.
    val slotOwner: Map[ListingKey, StoredFilmRef] = stored.flatMap { r =>
      val ref = StoredFilmRef(r.id.value, r.record.tmdbId)
      StoredSlots.of(r).flatMap(_.key.map(_ -> ref))
    }.groupMap(_._1)(_._2).flatMap { case (k, refs) => StoredFilmRef.ownerOf(refs.toSet).map(k -> _) }
    val previousOf: Map[ListingKey, StoredFilmRef] =
      StoredFilmSlots.of(byKey.values.map(_.listing).toSeq, stored, normalizer) ++ slotOwner.filter { case (k, _) => byKey.contains(k) }
    val listingsOf = previousOf.toSeq.groupMap(_._2.id)(_._1).view.mapValues(_.toSet).toMap
    (previousOf, listingsOf)
  }
}
