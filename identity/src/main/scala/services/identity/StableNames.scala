package services.identity

import services.movies.ListingKey

import scala.collection.mutable

/**
 * Stable names for clusters, by OVERLAP: the one rule behind the projection's film ids ([[IdAssigner]]) and the agreement
 * stage's cluster keys (docs/design/identity-cluster-identity.md). Names are opaque `Long`s from a counter, so a SMALLER
 * name is an OLDER one; nothing about a name is derived from a title or a year. A name only NAMES a cluster: no grouping,
 * take or wait reads one (docs/design/identity-no-drift.md, `NamesReadByNothingSpec`).
 *
 * Over every (previous cluster, next cluster) pair that share a listing, sorted by (previous name ascending, overlap
 * descending, next cluster's smallest listing ascending), each previous name goes to the first next cluster neither side
 * was matched in yet. In order of precedence:
 *
 *   - a MERGE keeps the OLDER name (the older previous cluster is offered first);
 *   - a SPLIT keeps the name on the LARGER half; on an equal split, on the half holding the smallest listing — a property
 *     of the listings, never of input order;
 *   - every next cluster left over gets a fresh name, in order of its smallest listing;
 *   - a previous name no next cluster took is RETIRED, never handed out again. One a next cluster overlaps is a merge's
 *     younger half: it is ABSORBED by the next cluster holding most of its listings (equal: the one holding the smallest
 *     listing) — named so to the staging screen, never a memory carried.
 *
 * Every step sorts by a key of the clusters themselves, so the result is invariant under the order either side is given
 * in. [[TieBreak.InputOrder]] is the teeth tests' mutation: an equal split goes to whichever half was listed first.
 */
object StableNames {

  private[identity] enum TieBreak { case Canonical, InputOrder }

  /** `names(i)`: the name of the `i`th next cluster; `absorbed`: each retired name a next cluster overlaps, with the index
   *  of the one that absorbs it; `nextFresh`: the counter past every name handed out. */
  final case class Named(names: IndexedSeq[Long], absorbed: Seq[(Long, Int)], nextFresh: Long)

  /** Names `next`, whose clusters are non-empty and disjoint, from the `previous` clusters' names. */
  def assign(previous: Iterable[(Long, Iterable[ListingKey])], next: IndexedSeq[Iterable[ListingKey]], nextFresh: Long): Named =
    assignWith(previous, next, nextFresh, TieBreak.Canonical)

  private[identity] def assignWith(previous: Iterable[(Long, Iterable[ListingKey])], next: IndexedSeq[Iterable[ListingKey]],
                                   nextFresh: Long, tieBreak: TieBreak): Named = {
    // Each cluster by its position, never by the set itself: a set's hash walks every member, and keying maps by the
    // clusters hashed each one again per lookup — most of a US projection's draft.
    val smallest = next.map(_.min(using ListingKey.ordering))
    val owner    = new java.util.HashMap[ListingKey, Integer]()
    next.indices.foreach(i => next(i).foreach(owner.put(_, i)))
    val pairs = previous.iterator.flatMap { case (name, members) =>
      val overlaps = mutable.HashMap.empty[Int, Int]
      members.foreach { listing => val n = owner.get(listing); if (n != null) overlaps(n) = overlaps.getOrElse(n, 0) + 1 }
      overlaps.iterator.map { case (n, overlap) => (name, n, overlap) }
    }.toSeq
    val ordered = tieBreak match {
      case TieBreak.Canonical  => pairs.sortBy { case (name, n, overlap) => (name, -overlap, smallest(n)) }
      case TieBreak.InputOrder => pairs.sortBy { case (name, n, overlap) => (name, -overlap, n) }
    }
    val taken = mutable.Set.empty[Long]
    val names = Array.fill(next.size)(-1L)
    ordered.foreach { case (name, n, _) => if (!taken(name) && names(n) < 0) { taken += name; names(n) = name } }
    // a retired name's first pair is the next cluster holding most of its listings: every one it overlaps took an older name
    val absorbed = ordered.iterator.filterNot { case (name, _, _) => taken(name) }.distinctBy(_._1).map { case (name, n, _) => name -> n }.toSeq
    var counter  = nextFresh
    next.indices.filter(names(_) < 0).sortBy(smallest)(using ListingKey.ordering).foreach { n => names(n) = counter; counter += 1 }
    Named(names.toIndexedSeq, absorbed, counter)
  }
}
