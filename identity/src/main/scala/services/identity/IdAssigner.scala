package services.identity

import services.movies.ListingKey

/**
 * Stable film ids for clusters, by OVERLAP (docs/design/identity-resolver.md §6, assumption A4): [[StableNames]]' rule
 * over the projection's clusters — a merge keeps the OLDER id, a split keeps it on the LARGER half (equal: the half
 * holding the smallest listing key), every cluster left over gets a fresh id in order of its smallest listing key, and a
 * previous id no cluster overlaps is RETIRED (it appears in no assignment). Ids are opaque `Long`s from a counter, so a
 * SMALLER id is an OLDER one; nothing about an id is derived from a title or a year (P4).
 */
object IdAssigner {

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

  def assign(previous: Seq[(Long, Set[ListingKey])], next: Seq[Set[ListingKey]], nextFresh: Long): Assignment = {
    val nexts = next.filter(_.nonEmpty).distinct.toVector
    val named = StableNames.assign(previous, nexts, nextFresh)
    Assignment(nexts.indices.map(n => named.names(n) -> nexts(n)).sortBy(_._1), named.nextFresh)
  }
}
