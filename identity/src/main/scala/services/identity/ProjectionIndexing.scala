package services.identity

import services.movies.LatestTitleYear

import models.ListingKey
import services.movies.StoredMovieRecord
import services.titles.TitleNormalizer

/** How a projection's [[ProjectionIndex]] is built afresh from the whole listing set, the resolution and the stored films
 *  — each a pass over the corpus — by the readings of slots ([[StoredSlots]]) and clusters ([[Cluster.of]]) the kept
 *  index ([[LiveProjectionIndex]]) applies entry by entry. */
object ProjectionIndexing {

  /** What a projection reads off the whole listing set, the resolution and the stored films before it drafts any film:
   *  each a pass over the corpus, made every tick, and what [[ProjectionScope]] closes a tick's changes over. */
  def index(listings: Seq[ProjectedListing], resolution: Resolution, stored: Seq[StoredMovieRecord], counters: FilmIdCounters,
            normalizer: TitleNormalizer)(using LatestTitleYear): ProjectionIndex = {
    val byKey      = oneByKey(listings)
    val storedById = stored.map(r => r.id.value -> r).toMap

    // The stored films as listing sets, numbered.
    // A listing is on the stored film whose slot it IS (the slot's own listing key); a listing no slot
    // names — one a venue's same-title fold hid behind another's slot — on the film holding its slot.
    val slotOwner: Map[ListingKey, StoredFilmRef] = stored.flatMap { r =>
      val ref = StoredFilmRef(r.id.value, r.record.tmdbId)
      slotsOf(r).flatMap(_.key.map(_ -> ref))
    }.groupMap(_._1)(_._2).flatMap { case (k, refs) => StoredFilmRef.ownerOf(refs.toSet).map(k -> _) }
    val previousOf: Map[ListingKey, StoredFilmRef] =
      StoredFilmSlots.of(byKey.values.map(_.listing).toSeq, stored, normalizer) ++ slotOwner.filter { case (k, _) => byKey.contains(k) }
    val listingsOf = previousOf.toSeq.groupMap(_._2.id)(_._1).view.mapValues(_.toSet).toMap
    val (covered, additions) = counters.cover(listingsOf.toSeq.map { case (id, ls) => IdSeeding.Film(id, ls) })

    // Clusters, one per film: the resolver's, with the clusters of one TMDB film joined.
    val clusters: Map[ClusterId, Cluster] = resolution.decisions.groupBy(ClusterId.of).flatMap { case (id, ds) =>
      Cluster.of(id, ds, byKey.contains).map(id -> _)
    }
    val clusterOf = clusters.iterator.flatMap { case (id, c) => c.members.iterator.map(_ -> id) }.toMap
    ProjectionIndex(byKey, storedById, previousOf, listingsOf, covered, additions, clusters, clusterOf)(normalizer)
  }

  /** One listing per key — the smallest by the total order, as the resolver takes it, then by the rest of its row: the
   *  row a slot is built over (`VenueSlots.first`), whatever order the venue printed them in — standing for every row
   *  published under that key (its slot is built from all of them: `merged`), so a venue printing one listing twice
   *  loses no showtime. */
  def oneByKey(listings: Seq[ProjectedListing]): Map[ListingKey, ProjectedListing] = {
    val first = scala.collection.mutable.HashMap.empty[ListingKey, ProjectedListing]
    val twice = scala.collection.mutable.HashMap.empty[ListingKey, List[ProjectedListing]]
    listings.foreach { l =>
      val key = l.listing.key
      first.get(key) match {
        case None          => first(key) = l
        case Some(earlier) => twice(key) = l :: twice.getOrElse(key, List(earlier))
      }
    }
    twice.foreach { case (key, rows) => first(key) = rows.minBy(p => (p.listing, p.row)).copy(showtimes = rows.map(_.showtimes).sorted.##) }
    first.toMap
  }

  /** Every slot of a stored film ([[StoredSlots.slotOf]]). */
  def slotsOf(r: StoredMovieRecord): Seq[StoredSlots.Slot] = r.record.data.toSeq.map { case (source, sd) => StoredSlots.slotOf(source, sd) }
}
