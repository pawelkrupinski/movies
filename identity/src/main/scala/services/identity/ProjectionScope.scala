package services.identity

import models.StoredMovieRecord


import models.ListingKey
import services.titles.TitleNormalizer
import services.identity.measures.ResolverDecision

/** A cluster across projections: the TMDB film a matched one is (the clusters of one film are one), or an unmatched
 *  decision's first listing — so a cluster that did not move is the same one next tick, whatever moved around it. */
enum ClusterId {
  case Matched(film: Int)
  case Unmatched(first: ListingKey)
}

object ClusterId {
  /** The cluster a decision is part of: its TMDB film's, or for no film its own. */
  def of(d: ResolverDecision): ClusterId = d.film.fold[ClusterId](ClusterId.Unmatched(d.members.min))(ClusterId.Matched(_))
}

/** A cluster: its listings published now, the TMDB film the resolver matched them to, and — for none — the fallback source's
 *  film it took instead ([[ResolverDecision.fallback]]). `provisional`: one of its decisions awaits an answer
 *  ([[ResolverDecision.awaits]]: a search, a venue page, a pool record): its take is the model's decision on the inputs as
 *  they stand, served marked so (step 6). The model's awaits alone -- a pure function of the inputs, read off decisions the
 *  projection holds anyway -- not the agreement stage's own waits, which depend on what an apply read and cost a pass
 *  over every decision each tick. */
final case class Cluster(members: Set[ListingKey], film: Option[Int], fallback: Option[ResolverDecision.Fallback] = None,
                         provisional: Boolean = false)

object Cluster {
  /** Cluster `id` of `decisions`, over the listings `published`: none when it publishes none. */
  def of(id: ClusterId, decisions: Seq[ResolverDecision], published: ListingKey => Boolean): Option[Cluster] = {
    val members     = decisions.iterator.flatMap(_.members).filter(published).toSet
    // a decision of its published listings Pending: the take served provisionally (step 6)
    val provisional = decisions.exists(d => d.awaits.nonEmpty && d.members.exists(published))
    Option.when(members.nonEmpty)(id match {
      case ClusterId.Matched(film) => Cluster(members, Some(film), provisional = provisional)
      case _                       => Cluster(members, None, decisions.iterator.flatMap(_.fallback).nextOption(), provisional)
    })
  }
}

/** What a projection reads off the whole listing set, the resolution and the stored films before it drafts any film
 *  ([[ProjectionIndexing.index]]): every listing by key, each listing's previous film (`previousOf` — the film whose
 *  slot it is, else the one whose slot it folds into), each stored film's listings, the FilmId map extended over them,
 *  and the clusters, one per film, with each listing's. What [[ProjectionScope]] closes a tick's changes over. */
final case class ProjectionIndex(byKey: Map[ListingKey, ProjectedListing], storedById: Map[String, StoredMovieRecord],
                                 previousOf: Map[ListingKey, StoredFilmRef], listingsOf: Map[String, Set[ListingKey]],
                                 covered: FilmIdCounters, additions: Seq[FilmIdCounter],
                                 clusters: Map[ClusterId, Cluster], clusterOf: Map[ListingKey, ClusterId])(normalizer: TitleNormalizer) {
  /** Whether some published listing is on stored film `id`. */
  def placed(id: String): Boolean = listingsOf.contains(id)

  /** Each stored film's PLAIN key — the `title|year` it is stored under, or would be were an older film of that title and
   *  year not holding it (`title~counter|year`, [[FilmKeys.finish]]) — and the stored films under each:
   *  worked out once per index, however often a scope is widened over it. */
  lazy val storedByPlainKey: Map[String, Seq[String]] =
    storedById.valuesIterator.map(r => ProjectionScope.plainKey(r.key(normalizer)) -> r.id.value).toSeq.groupMap(_._1)(_._2)

  /** Every film of the corpus. */
  def everything: ProjectionScope = ProjectionScope.Whole

  /** What of this index `scope` drafts: its listings' previous films, its stored films' listings, its clusters and its
   *  stored films — all of them for the whole corpus. */
  def within(scope: ProjectionScope): ProjectionIndex.Within =
    if (scope.whole) ProjectionIndex.Within(previousOf, listingsOf.toSeq, clusters.values.toSeq, storedById.values.toSeq)
    else ProjectionIndex.Within(previousOf.filter { case (k, _) => scope.listings(k) },
      scope.films.toSeq.flatMap(id => listingsOf.get(id).map(id -> _)), scope.clusters.toSeq.map(clusters),
      scope.films.toSeq.flatMap(storedById.get))
}

object ProjectionIndex {
  /** The part of an index one scope drafts ([[ProjectionIndex.within]]). */
  final case class Within(previousOf: Map[ListingKey, StoredFilmRef], listingsOf: Seq[(String, Set[ListingKey])],
                          clusters: Seq[Cluster], stored: Seq[StoredMovieRecord])
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
 * [[FilmKeys.finish]]). [[close]] follows the first two from the changes until nothing new is reached; the
 * key collisions, known only once the drafts are titled, are followed by the caller ([[widened]]) and closed again.
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
final case class ProjectionScope(whole: Boolean, films: Set[String], clusters: Set[ClusterId], listings: Set[ListingKey])

object ProjectionScope {
  val Whole: ProjectionScope = ProjectionScope(whole = true, Set.empty, Set.empty, Set.empty)

  /** What moved: listings, stored films, and the plain keys those films were stored under — a key a gone film held is
   *  one another film may now take ([[widened]]). */
  final case class Changes(listings: Set[ListingKey], films: Set[String], keys: Set[String] = Set.empty) {
    def ++(other: Changes): Changes = Changes(listings ++ other.listings, films ++ other.films, keys ++ other.keys)
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
    val listings = new Worklist[ListingKey]
    val films    = new Worklist[String]
    val clusters = new Worklist[ClusterId]
    changes.listings.foreach(listings.add)
    changes.films.foreach(films.add)
    while (listings.pending || films.pending || clusters.pending) {
      while (listings.pending) {
        val k = listings.next()
        now.previousOf.get(k).foreach(r => films.add(r.id))
        now.clusterOf.get(k).foreach(clusters.add)
      }
      while (films.pending) now.listingsOf.getOrElse(films.next(), Set.empty).foreach(listings.add)
      // A cluster is every listing of its TMDB film (the index joins them), so a film's TMDB id couples it to nothing
      // its listings do not already lead to.
      while (clusters.pending) now.clusters(clusters.next()).members.foreach(listings.add)
    }
    // Only stored films are films of the scope: a listing's previous film is always one.
    ProjectionScope(whole = false, films.seen.iterator.filter(now.storedById.contains).toSet, clusters.seen.toSet, listings.seen.toSet)
  }

  /** What [[close]] has reached of one kind, and what of it is still to follow, newest first. */
  private final class Worklist[A] {
    val seen: scala.collection.mutable.HashSet[A] = scala.collection.mutable.HashSet.empty[A]
    private val stack = scala.collection.mutable.Stack.empty[A]
    def add(a: A): Unit = if (seen.add(a)) stack.push(a)
    def pending: Boolean = stack.nonEmpty
    def next(): A = stack.pop()
  }

  /** `scope` closed again over the stored films outside it under a plain key one of its films is drafted under
   *  (`filmKeys`) or stored under, or one its changes `freed` — their keys are decided with the scope's films' — or none
   *  when no stored film outside it holds one. */
  def widened(now: ProjectionIndex, scope: ProjectionScope, filmKeys: Iterable[String], freed: Set[String],
              normalizer: TitleNormalizer): Option[ProjectionScope] = {
    val keys    = filmKeys.iterator.map(plainKey).toSet ++
      scope.films.iterator.flatMap(now.storedById.get).map(r => plainKey(r.key(normalizer))) ++ freed
    val holders = keys.flatMap(now.storedByPlainKey.getOrElse(_, Nil)).filterNot(scope.films)
    Option.when(holders.nonEmpty)(close(now, Changes(scope.listings, scope.films ++ holders)))
  }
}
