package services.identity

import scala.collection.immutable.LongMap

/**
 * An immutable map from Long keys, as two arrays sorted by key: 12 bytes an entry and no hash node, table or boxed key —
 * for a map read many times and replaced in one step ([[merged]]). An open-addressing map kept at half load was ~30 bytes
 * an entry (the venue slot memo's ~200k on worker-us).
 *
 * The arrays are shared from one merge to the next, the changes since they were built held beside them (`overlay`, a
 * removed key as null) until they reach [[SortedLongMap.compactAt]] of them: the venue slot memo merges, every light
 * projection, the tens of thousands of entries it looked up — almost all the very ones it holds — and a few it built.
 * Copied whole each time, the arrays lived until the next projection: minutes, so promoted to the old generation every
 * projection, to die there.
 */
private[identity] final class SortedLongMap[V <: AnyRef] private (keys: Array[Long], values: Array[AnyRef],
                                                                   overlay: LongMap[AnyRef], val size: Int) {
  private def inBase(key: Long): Int = java.util.Arrays.binarySearch(keys, key)

  def get(key: Long): Option[V] = Option(lookup(key).asInstanceOf[V])

  /** The value held under `key`, null for none. */
  private def lookup(key: Long): AnyRef = overlay.get(key) match {
    case Some(v) => v
    case None    => val i = inBase(key); if (i >= 0) values(i) else null
  }

  /** Whether the overlay names `key` — LongMap's own `get`, which neither boxes the key nor allocates for one it lacks. */
  private def overlaid(key: Long): Boolean = overlay.nonEmpty && overlay.get(key).isDefined

  def valuesIterator: Iterator[V] =
    (keys.indices.iterator.filterNot(i => overlaid(keys(i))).map(values(_)) ++ overlay.valuesIterator.filter(_ != null))
      .map(_.asInstanceOf[V])

  /** This map without the keys `removed` holds, and with `added` over it (an added key is kept whether or not removed). A
   *  key added with the value held — the same object, or an equal one — is no change; a merge of none is this map. */
  def merged(added: scala.collection.Map[Long, V], removed: Long => Boolean = _ => false): SortedLongMap[V] = {
    var next  = overlay
    var count = size
    added.foreachEntry { (k, v) =>
      val held = lookup(k)
      if (!((held eq v) || (held != null && held == v))) { next = next.updated(k, v); if (held == null) count += 1 }
    }
    def remove(k: Long): Unit = if (removed(k) && !added.contains(k)) {
      next = if (inBase(k) >= 0) next.updated(k, null) else next - k
      count -= 1
    }
    var i = 0
    while (i < keys.length) { val k = keys(i); if (!overlaid(k)) remove(k); i += 1 }   // no boxing of 200k keys
    overlay.foreach { case (k, v) => if (v != null) remove(k) }
    if (next eq overlay) this
    else if (next.size >= SortedLongMap.compactAt(keys.length)) SortedLongMap.compact(keys, values, next, count)
    else new SortedLongMap[V](keys, values, next, count)
  }
}

private[identity] object SortedLongMap {
  def empty[V <: AnyRef]: SortedLongMap[V] = new SortedLongMap[V](Array.emptyLongArray, Array.emptyObjectArray, LongMap.empty, 0)

  /** How many changes a map of `base` entries holds beside its arrays before it builds them again: an 8th of them, so its
   *  arrays are built again once every ~30 light projections on worker-us, not every one. */
  private[identity] def compactAt(base: Int): Int = math.max(1024, base / 8)

  private def compact[V <: AnyRef](keys: Array[Long], values: Array[AnyRef], overlay: LongMap[AnyRef], size: Int): SortedLongMap[V] = {
    val outK = new Array[Long](size)
    val outV = new Array[AnyRef](size)
    val adds = overlay.iterator.filter(_._2 != null).toArray.sortInPlaceBy(_._1)
    var i, j, n = 0
    def emit(k: Long, v: AnyRef): Unit = { outK(n) = k; outV(n) = v; n += 1 }
    while (i < keys.length || j < adds.length) {
      if (j >= adds.length || (i < keys.length && keys(i) < adds(j)._1)) { if (overlay.get(keys(i)).isEmpty) emit(keys(i), values(i)); i += 1 }
      else { if (i < keys.length && keys(i) == adds(j)._1) i += 1; emit(adds(j)._1, adds(j)._2); j += 1 }
    }
    new SortedLongMap[V](outK, outV, LongMap.empty, size)
  }
}
