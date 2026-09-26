package services.identity

import services.movies.ListingKey

/**
 * Stable film ids for clusters, by OVERLAP (docs/design/identity-resolver.md §6, assumption A4).
 * Ids are opaque `Long`s from a counter, so a SMALLER id is an OLDER one; nothing about an id is
 * derived from a title or a year (P4).
 *
 * The rule, over every (previous cluster, next cluster) pair that share a listing: sort the pairs
 * by (previous id ascending, overlap descending, next cluster's smallest listing key ascending) and
 * hand each previous id to the first next cluster in that order that neither side has already
 * been matched in. In order of precedence that gives:
 *
 *   - a MERGE keeps the OLDER id (the older previous cluster is offered first);
 *   - a SPLIT keeps the id on the LARGER half; on an equal split, on the half holding the smallest
 *     listing key — a property of the listings, never of input order;
 *   - every next cluster left over gets a fresh id, in order of its smallest listing key;
 *   - a previous id no next cluster overlaps is RETIRED (it appears in no assignment).
 *
 * Every step sorts by a key of the clusters themselves, so the result is invariant under the
 * order either list is given in. [[TieBreak.InputOrder]] is the teeth tests' mutation: an equal
 * split goes to whichever half was listed first.
 */
object IdAssigner {

  private[identity] enum TieBreak { case Canonical, InputOrder }

  final case class Assignment(ids: Seq[(Long, Set[ListingKey])], nextFresh: Long) {
    lazy val idOf: Map[Set[ListingKey], Long]   = ids.map(_.swap).toMap
    lazy val idOfListing: Map[ListingKey, Long] = ids.flatMap { case (id, ls) => ls.map(_ -> id) }.toMap
  }

  def fresh(next: Seq[Set[ListingKey]], start: Long = 1L): Assignment = assign(Seq.empty, next, start)

  def assign(previous: Seq[(Long, Set[ListingKey])], next: Seq[Set[ListingKey]], nextFresh: Long): Assignment =
    assignWith(previous, next, nextFresh, TieBreak.Canonical)

  private[identity] def assignWith(previous: Seq[(Long, Set[ListingKey])], next: Seq[Set[ListingKey]], nextFresh: Long,
                                   tieBreak: TieBreak): Assignment = {
    val nexts    = next.filter(_.nonEmpty).distinct
    val smallest = nexts.map(n => n -> n.min).toMap
    val position = nexts.zipWithIndex.toMap
    val owner    = nexts.iterator.flatMap(n => n.iterator.map(_ -> n)).toMap
    val pairs = previous.flatMap { case (id, members) =>
      members.toSeq.flatMap(owner.get).groupMapReduce(identity)(_ => 1)(_ + _).map { case (n, overlap) => (id, n, overlap) }
    }
    val ordered = tieBreak match {
      case TieBreak.Canonical  => pairs.sortBy { case (id, n, overlap) => (id, -overlap, smallest(n)) }
      case TieBreak.InputOrder => pairs.sortBy { case (id, n, overlap) => (id, -overlap, position(n)) }
    }
    val takenIds   = scala.collection.mutable.Set.empty[Long]
    val takenNexts = scala.collection.mutable.Map.empty[Set[ListingKey], Long]
    ordered.foreach { case (id, n, _) =>
      if (!takenIds(id) && !takenNexts.contains(n)) { takenIds += id; takenNexts(n) = id }
    }
    var counter = nextFresh
    nexts.filterNot(takenNexts.contains).sortBy(smallest).foreach { n => takenNexts(n) = counter; counter += 1 }
    Assignment(nexts.map(n => takenNexts(n) -> n).sortBy(_._1), counter)
  }

  /** How many listings present in both assignments changed film id — the churn P2 bounds at 0. */
  def listingIdChanges(before: Assignment, after: Assignment): Int =
    after.idOfListing.count { case (l, id) => before.idOfListing.get(l).exists(_ != id) }
}
