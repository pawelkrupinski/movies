package services.identity

import services.movies.ListingKey

/**
 * The migration's first id assignment (docs/design/identity-resolver.md §8, phase 4 "ID seeding"):
 * [[IdAssigner]] run with TODAY's films as the previous assignment, so every resolver cluster
 * inherits the id of the film it overlaps most, and the films it cannot do that for — the
 * migration's review list — are named.
 *
 * Today's film is the set of its slots' listing keys (`movie_slots.listingKey`). Its FilmId is an
 * opaque string, so it goes through the persisted FilmId map ([[FilmIdCounters]]) to the counter
 * [[IdAssigner]] works over: `IdAssigner` hands a contested cluster to the SMALLER number. A film
 * the map already holds keeps its stored counter; one it does not is numbered after it by the
 * map's own rule — largest film first (ties by id), since a film being migrated has no creation
 * counter to be "older" by, so the film with more listings, the one more of the site's showtimes
 * hang off, keeps its id. Fresh ids follow the largest counter.
 *
 * Pure and order-independent, like [[IdAssigner]]: the review is a function of the two sets.
 */
object IdSeeding {

  final case class Film(id: String, listings: Set[ListingKey])

  /** What seeding would do.
   *
   *  @param keeps       film id → the cluster that inherits it
   *  @param unmatched   films no cluster overlaps (their listings are not in the corpus)
   *  @param mergedAway  films that overlap a cluster only where a larger film's id won it: `(film, winner)`
   *  @param split       films whose listings the resolver spreads over two or more clusters
   *  @param fresh       clusters that get a new id, each with the films it overlaps (none: unseen listings)
   */
  final case class Review(keeps: Map[String, Set[ListingKey]], unmatched: Seq[Film], mergedAway: Seq[(Film, String)],
                          split: Seq[(Film, Seq[Set[ListingKey]])], fresh: Seq[(Set[ListingKey], Seq[String])])

  def review(films: Seq[Film], clusters: Seq[Set[ListingKey]], ids: FilmIdCounters = FilmIdCounters.empty): Review = {
    val present  = films.filter(_.listings.nonEmpty).distinctBy(_.id)
    val counters = ids.covering(present)
    val number   = present.map(f => f.id -> counters.counterOf(f.id).get).toMap
    val ranked   = present.sortBy(f => number(f.id))
    val byNumber = ranked.map(f => number(f.id) -> f).toMap
    val assigned = IdAssigner.assign(ranked.map(f => number(f.id) -> f.listings), clusters, counters.nextCounter)
    val owner    = clusters.iterator.flatMap(c => c.iterator.map(_ -> c)).toMap
    /** The clusters `f`'s listings fall in, the one holding most of them first. */
    def overlapped(f: Film): Seq[Set[ListingKey]] =
      f.listings.toSeq.flatMap(owner.get).groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sortBy { case (c, n) => (-n, c.min) }.map(_._1)
    val kept     = assigned.ids.collect { case (n, c) if byNumber.contains(n) => byNumber(n).id -> c }.toMap
    val filmsOf  = ranked.flatMap(f => overlapped(f).map(_ -> f.id)).groupMap(_._1)(_._2)
    val retired  = ranked.filterNot(f => kept.contains(f.id))
    Review(
      keeps      = kept,
      unmatched  = retired.filter(f => overlapped(f).isEmpty),
      mergedAway = retired.flatMap { f =>
        overlapped(f).headOption.flatMap(c => kept.collectFirst { case (id, k) if k == c => f -> id })
      },
      split      = ranked.map(f => f -> overlapped(f)).filter(_._2.sizeIs > 1),
      fresh      = assigned.ids.collect { case (n, c) if !byNumber.contains(n) => c -> filmsOf.getOrElse(c, Nil).sorted })
  }
}
