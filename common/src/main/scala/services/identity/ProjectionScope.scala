package services.identity

import services.movies.{ListingKey, StoredMovieRecord, TitleNormalizer}

/** A cluster across projections: the TMDB film a matched one is (the clusters of one film are one), or an unmatched
 *  decision's first listing — so a cluster that did not move is the same one next tick, whatever moved around it. */
enum ClusterId {
  case Matched(film: Int)
  case Unmatched(first: ListingKey)
}

/** A cluster: its listings published now, the TMDB film the resolver matched them to, and — for none — the fallback source's
 *  film it took instead ([[ResolverDecision.fallback]]), whether a question it was reached on is not answered yet, and the
 *  TMDB film its evidence leans to ([[ResolverDecision.leaning]]). */
final case class Cluster(members: Set[ListingKey], film: Option[Int], fallback: Option[ResolverDecision.Fallback] = None,
                         unanswered: Boolean = false, leaning: Option[ResolverDecision.Leaning] = None)

/** What a projection reads off the whole listing set, the resolution and the stored films before it drafts any film
 *  ([[IdentityProjectionPlan.index]]): every listing by key, each listing's previous film (`previousOf` — the film whose
 *  slot it is, else the one whose slot it folds into), each stored film's listings, the FilmId map extended over them,
 *  and the clusters, one per film, with each listing's. What [[ProjectionScope]] closes a tick's changes over. */
final case class ProjectionIndex(byKey: Map[ListingKey, ProjectedListing], storedById: Map[String, StoredMovieRecord],
                                 previousOf: Map[ListingKey, PipelineFilmRef], listingsOf: Map[String, Set[ListingKey]],
                                 covered: FilmIdCounters, additions: Seq[FilmIdCounter],
                                 clusters: Map[ClusterId, Cluster], clusterOf: Map[ListingKey, ClusterId]) {
  /** Whether some published listing is on stored film `id`. */
  def placed(id: String): Boolean = listingsOf.contains(id)

  /** Each stored film's PLAIN key — the `title|year` it is stored under, or would be were an older film of that title and
   *  year not holding it (`title~counter|year`, [[IdentityProjectionPlan.finish]]) — and the stored films under each. */
  def storedByPlainKey(normalizer: TitleNormalizer): Map[String, Seq[String]] =
    storedById.valuesIterator.map(r => ProjectionScope.plainKey(r.key(normalizer)) -> r.id.value).toSeq.groupMap(_._1)(_._2)

  /** Every film of the corpus. */
  def everything: ProjectionScope = ProjectionScope.Whole
}

/**
 * The films ONE projection must draft, compare and write: the ones its changes reach, and every film whose draft
 * any of theirs can move. Drafting only these gives each of them exactly the draft a projection of the whole corpus
 * gives it, and leaves every other film as stored — which is what the whole projection would leave it as, since a
 * film nothing moved projects to itself (P2).
 *
 * A film's draft reads, besides its own listings' rows: its listings' previous films (`previousOf`), the clusters those
 * films' listings fall in (ids are handed out by OVERLAP — [[IdAssigner]]; a cluster is every listing of its TMDB film,
 * the index joins them), and every film whose plain title key is its own (the older one keeps it —
 * [[IdentityProjectionPlan.finish]]). [[close]] follows the first two from the changes until nothing new is reached; the
 * key collisions, known only once the drafts are titled, are followed by the caller ([[keyHolders]]) and closed again.
 *
 * The changes ([[LiveProjectionIndex.update]]) are everything a projection reads that moved since the last one: a
 * listing added, gone, re-scraped or newly decided by the model; a listing on another previous film; a cluster that is
 * not the one it was (a merge, a split, another TMDB film); a stored film another writer changed or deleted, and the key
 * it held. And, every tick, the stored films a whole projection would not leave as they are whatever moved
 * ([[standing]]).
 *
 * That this is enough is what `ScopedProjectionEquivalenceSpec` holds it to: thousands of random runs, each tick's store
 * identical to a whole projection's, and every coupling above dropped in turn caught by it.
 */
final case class ProjectionScope(whole: Boolean, films: Set[String], clusters: Set[ClusterId], listings: Set[ListingKey]) {
  def isEmpty: Boolean = !whole && films.isEmpty && clusters.isEmpty && listings.isEmpty
}

object ProjectionScope {
  val Whole: ProjectionScope = ProjectionScope(whole = true, Set.empty, Set.empty, Set.empty)

  /** What moved: listings, stored films, and the plain keys those films were stored under — a key a gone film held is
   *  one another film may now take ([[keyHolders]]). */
  final case class Changes(listings: Set[ListingKey], films: Set[String], keys: Set[String] = Set.empty) {
    def ++(other: Changes): Changes = Changes(listings ++ other.listings, films ++ other.films, keys ++ other.keys)
    def isEmpty: Boolean = listings.isEmpty && films.isEmpty && keys.isEmpty
  }
  object Changes { val none: Changes = Changes(Set.empty, Set.empty) }

  /** The plain key a stored key is the variant of: `title~counter|year` is `title|year`'s. A sanitized title holds no
   *  `~` (its punctuation is stripped), so the variant is told apart exactly. */
  def plainKey(key: String): String = key.replaceFirst("~[0-9]+\\|", "|")

  /** The stored films a whole projection would change whatever moved, so every projection drafts them: one no published
   *  listing is on (retired), and one matched to a TMDB film whose details it still lacks (they are fetched again). */
  def standing(now: ProjectionIndex): Changes =
    Changes(Set.empty, now.storedById.valuesIterator.collect {
      case r if !now.placed(r.id.value) || r.record.tmdbId.exists(_ => !r.record.data.contains(models.Tmdb)) => r.id.value
    }.toSet)

  /** `changes`, closed over everything a draft reads of another film's (see the class doc). */
  def close(now: ProjectionIndex, changes: Changes): ProjectionScope = {
    val listings = scala.collection.mutable.HashSet.empty[ListingKey]
    val films    = scala.collection.mutable.HashSet.empty[String]
    val clusters = scala.collection.mutable.HashSet.empty[ClusterId]
    val pendingListings = scala.collection.mutable.Stack.empty[ListingKey]
    val pendingFilms    = scala.collection.mutable.Stack.empty[String]
    val pendingClusters = scala.collection.mutable.Stack.empty[ClusterId]
    def listing(k: ListingKey): Unit = if (listings.add(k)) pendingListings.push(k)
    def film(id: String): Unit       = if (films.add(id)) pendingFilms.push(id)
    def cluster(i: ClusterId): Unit  = if (clusters.add(i)) pendingClusters.push(i)
    changes.listings.foreach(listing)
    changes.films.foreach(film)
    while (pendingListings.nonEmpty || pendingFilms.nonEmpty || pendingClusters.nonEmpty) {
      while (pendingListings.nonEmpty) {
        val k = pendingListings.pop()
        now.previousOf.get(k).foreach(r => film(r.id))
        now.clusterOf.get(k).foreach(cluster)
      }
      while (pendingFilms.nonEmpty) {
        val id = pendingFilms.pop()
        now.listingsOf.getOrElse(id, Set.empty).foreach(listing)
      }
      // A cluster is every listing of its TMDB film (the index joins them), so a film's TMDB id couples it to nothing
      // its listings do not already lead to.
      while (pendingClusters.nonEmpty) now.clusters(pendingClusters.pop()).members.foreach(listing)
    }
    // Only stored films are films of the scope: a listing's previous film is always one.
    ProjectionScope(whole = false, films.iterator.filter(now.storedById.contains).toSet, clusters.toSet, listings.toSet)
  }

  /** The stored films outside `scope` under one of `plainKeys`: their keys are decided with the scope's films'. */
  def keyHolders(now: ProjectionIndex, scope: ProjectionScope, plainKeys: Set[String], normalizer: TitleNormalizer): Set[String] = {
    val byKey = now.storedByPlainKey(normalizer)
    plainKeys.flatMap(byKey.getOrElse(_, Nil)).filterNot(scope.films)
  }
}
