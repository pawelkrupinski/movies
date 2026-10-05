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

  /** A read-only map over a hash table of listings' film ids: a change makes an ordinary map of the same entries. */
  private[identity] final class ListingIds(table: java.util.HashMap[ListingKey, java.lang.Long])
      extends scala.collection.immutable.AbstractMap[ListingKey, Long] {
    def get(key: ListingKey): Option[Long] = { val id = table.get(key); if (id == null) None else Some(id.longValue) }
    override def contains(key: ListingKey): Boolean = table.containsKey(key)
    def iterator: Iterator[(ListingKey, Long)] = {
      val entries = table.entrySet().iterator()
      Iterator.continually(entries).takeWhile(_.hasNext).map(_.next()).map(e => e.getKey -> e.getValue.longValue)
    }
    override def size: Int      = table.size
    override def knownSize: Int = table.size
    def removed(key: ListingKey): Map[ListingKey, Long] = Map.from(iterator).removed(key)
    def updated[V1 >: Long](key: ListingKey, value: V1): Map[ListingKey, V1] = Map.from(iterator).updated(key, value)
  }

  final case class Assignment(ids: Seq[(Long, Set[ListingKey])], nextFresh: Long) {
    lazy val idOf: Map[Set[ListingKey], Long]   = ids.map(_.swap).toMap
    /** Each listing's film id. Built every projection over ~100k listings on worker-us: as an immutable map, its build
     *  resized and copied node arrays all the way up — dead `int[]`s and `Object[]`s in the old generation (heap dump
     *  2026-10-05) — so it is a hash table sized for them up front, each film's id boxed once, read as a `Map`. */
    lazy val idOfListing: Map[ListingKey, Long] = {
      val table = new java.util.HashMap[ListingKey, java.lang.Long]((ids.iterator.map(_._2.size).sum * 4 / 3) + 1)
      ids.foreach { case (id, ls) => val boxed = java.lang.Long.valueOf(id); ls.foreach(table.put(_, boxed)) }
      new IdAssigner.ListingIds(table)
    }
  }

  def fresh(next: Seq[Set[ListingKey]], start: Long = 1L): Assignment = assign(Seq.empty, next, start)

  def assign(previous: Seq[(Long, Set[ListingKey])], next: Seq[Set[ListingKey]], nextFresh: Long): Assignment =
    assignWith(previous, next, nextFresh, TieBreak.Canonical)

  private[identity] def assignWith(previous: Seq[(Long, Set[ListingKey])], next: Seq[Set[ListingKey]], nextFresh: Long,
                                   tieBreak: TieBreak): Assignment = {
    // Each cluster by its position, never by the set itself: a set's hash walks every member, and
    // keying maps by the clusters hashed each one again per lookup — most of a US projection's draft.
    val nexts    = next.filter(_.nonEmpty).distinct.toVector
    val smallest = nexts.map(_.min)
    val owner    = nexts.iterator.zipWithIndex.flatMap { case (n, i) => n.iterator.map(_ -> i) }.toMap
    val pairs = previous.flatMap { case (id, members) =>
      members.toSeq.flatMap(owner.get).groupMapReduce(identity)(_ => 1)(_ + _).map { case (n, overlap) => (id, n, overlap) }
    }
    val ordered = tieBreak match {
      case TieBreak.Canonical  => pairs.sortBy { case (id, n, overlap) => (id, -overlap, smallest(n)) }
      case TieBreak.InputOrder => pairs.sortBy { case (id, n, overlap) => (id, -overlap, n) }
    }
    val takenIds = scala.collection.mutable.Set.empty[Long]
    val idOf     = Array.fill(nexts.size)(-1L)
    ordered.foreach { case (id, n, _) =>
      if (!takenIds(id) && idOf(n) < 0) { takenIds += id; idOf(n) = id }
    }
    var counter = nextFresh
    nexts.indices.filter(idOf(_) < 0).sortBy(smallest).foreach { n => idOf(n) = counter; counter += 1 }
    Assignment(nexts.indices.map(n => idOf(n) -> nexts(n)).sortBy(_._1), counter)
  }

  /** How many listings present in both assignments changed film id — the churn P2 bounds at 0. */
  def listingIdChanges(before: Assignment, after: Assignment): Int =
    after.idOfListing.count { case (l, id) => before.idOfListing.get(l).exists(_ != id) }
}
