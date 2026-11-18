package services.identity

import services.movies.LatestTitleYear

import models.{CinemaShowing, ListingKey, Source, SourceData}
import services.movies.StoredMovieRecord
import services.titles.TitleNormalizer

/** How a projection's [[ProjectionIndex]] is built afresh from the whole listing set, the resolution and the stored films
 *  — each a pass over the corpus — and the readings of slots and clusters it is built by, which the kept index
 *  ([[LiveProjectionIndex]]) applies entry by entry. */
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
    }.groupMap(_._1)(_._2).flatMap { case (k, refs) => ownerOf(refs.toSet).map(k -> _) }
    val previousOf: Map[ListingKey, StoredFilmRef] =
      StoredFilmSlots.of(byKey.values.map(_.listing).toSeq, stored, normalizer) ++ slotOwner.filter { case (k, _) => byKey.contains(k) }
    val listingsOf = previousOf.toSeq.groupMap(_._2.id)(_._1).view.mapValues(_.toSet).toMap
    val (covered, additions) = counters.cover(listingsOf.toSeq.map { case (id, ls) => IdSeeding.Film(id, ls) })

    // Clusters, one per film: the resolver's, with the clusters of one TMDB film joined.
    val clusters: Map[ClusterId, Cluster] = resolution.decisions.groupBy(clusterIdOf).flatMap { case (id, ds) =>
      ProjectionIndexing.clusterOf(id, ds, byKey.contains).map(id -> _)
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

  /** One slot of a stored film: where a venue's same-title fold puts it (`at`, a venue slot's venue and slot key), its
   *  data, and the listing key it is the slot of, if any. */
  final case class Slot(at: Option[(String, String)], data: SourceData, key: Option[ListingKey])

  def slotsOf(r: StoredMovieRecord): Seq[Slot] = r.record.data.toSeq.map { case (source, sd) => slotOf(source, sd) }

  def slotOf(source: Source, sd: SourceData): Slot =
    Slot(source match { case cs: CinemaShowing => Some(cs.cinema.displayName -> cs.titleKey); case _ => None }, sd, ListingKey.ofSource(source, sd))

  /** The film a listing is the slot of, of the films whose slot it is: the smallest id. */
  def ownerOf(refs: Set[StoredFilmRef]): Option[StoredFilmRef] = refs.minByOption(_.id)

  /** The cluster a decision is part of: its TMDB film's, or for no film its own. */
  def clusterIdOf(d: ResolverDecision): ClusterId = d.film.fold[ClusterId](ClusterId.Unmatched(d.members.min))(ClusterId.Matched(_))

  /** Cluster `id` of `decisions`, over the listings `published`: none when it publishes none. */
  def clusterOf(id: ClusterId, decisions: Seq[ResolverDecision], published: ListingKey => Boolean): Option[Cluster] = {
    val members     = decisions.iterator.flatMap(_.members).filter(published).toSet
    // a decision of its published listings Pending: the take served provisionally (step 6)
    val provisional = decisions.exists(d => d.awaits.nonEmpty && d.members.exists(published))
    Option.when(members.nonEmpty)(id match {
      case ClusterId.Matched(film) => Cluster(members, Some(film), provisional = provisional)
      case _                       => Cluster(members, None, decisions.iterator.flatMap(_.fallback).nextOption(), provisional)
    })
  }
}
