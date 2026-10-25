package services.identity.agreement

import services.movies.ListingKey

import scala.collection.mutable

/**
 * The agreement stage's cluster keys ([[AgreementStage.keyOf]], docs/design/identity-cluster-identity.md), by OVERLAP:
 * the rule [[services.identity.IdAssigner]] gives the projection's film ids. Keys are opaque `Long`s from the stage's own
 * counter, so a SMALLER key is an OLDER one. Over every (previous cluster, next cluster) pair that share a listing, sorted
 * by (previous key ascending, overlap descending, next cluster's smallest listing ascending), each previous key goes to
 * the first next cluster neither side was matched in yet:
 *
 *   - a MERGE keeps the OLDER key;
 *   - a SPLIT keeps the key on the LARGER half; on an equal split, on the half holding the smallest listing;
 *   - every next cluster left over gets a fresh key, in order of its smallest listing;
 *   - a previous key no next cluster took is RETIRED, never handed out again. One a next cluster overlaps is a merge's
 *     younger half: it is ABSORBED by the next cluster holding most of its listings (equal: the one holding the smallest
 *     listing) — named so to the staging screen, never a memory carried (docs/design/identity-no-drift.md).
 */
object ClusterKeys {

  /** `keys(i)`: the key of the `i`th next cluster; `absorbed`: each retired key a next cluster overlaps, with the index of
   *  the one that absorbs it; `nextFresh`: the counter past every key handed out. */
  final case class Assigned(keys: IndexedSeq[Long], absorbed: Seq[(Long, Int)], nextFresh: Long)

  def assign(previous: Iterable[(Long, Iterable[ListingKey])], next: IndexedSeq[Iterable[ListingKey]], nextFresh: Long): Assigned = {
    val smallest = next.map(_.min(using ListingKey.ordering))
    val owner    = new java.util.HashMap[ListingKey, Integer]()
    next.indices.foreach(i => next(i).foreach(owner.put(_, i)))
    val pairs = previous.iterator.flatMap { case (key, members) =>
      val overlaps = mutable.HashMap.empty[Int, Int]
      members.foreach { listing => val n = owner.get(listing); if (n != null) overlaps(n) = overlaps.getOrElse(n, 0) + 1 }
      overlaps.iterator.map { case (n, overlap) => (key, n, overlap) }
    }.toSeq.sortBy { case (key, n, overlap) => (key, -overlap, smallest(n)) }
    val taken = mutable.Set.empty[Long]
    val keys  = Array.fill(next.size)(-1L)
    pairs.foreach { case (key, n, _) => if (!taken(key) && keys(n) < 0) { taken += key; keys(n) = key } }
    // a retired key's first pair is the next cluster holding most of its listings: every one it overlaps took an older key
    val absorbed = pairs.iterator.filterNot { case (key, _, _) => taken(key) }.distinctBy(_._1).map { case (key, n, _) => key -> n }.toSeq
    var counter = nextFresh
    next.indices.filter(keys(_) < 0).sortBy(smallest)(using ListingKey.ordering).foreach { n => keys(n) = counter; counter += 1 }
    Assigned(keys.toIndexedSeq, absorbed, counter)
  }
}
