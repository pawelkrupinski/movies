package services.identity

/**
 * An immutable map from Long keys, as two arrays sorted by key: 12 bytes an entry and no hash node, table or boxed key —
 * for a map read many times and replaced in one step ([[merged]]). An open-addressing map kept at half load was ~30 bytes
 * an entry (the venue slot memo's ~200k on worker-us).
 */
private[identity] final class SortedLongMap[V <: AnyRef] private (keys: Array[Long], values: Array[AnyRef]) {
  def size: Int = keys.length

  def get(key: Long): Option[V] = {
    val i = java.util.Arrays.binarySearch(keys, key)
    if (i >= 0) Some(values(i).asInstanceOf[V]) else None
  }

  def valuesIterator: Iterator[V] = values.iterator.map(_.asInstanceOf[V])

  /** This map without the keys `removed` holds, and with `added` over it (an added key is kept whether or not removed). */
  def merged(added: scala.collection.Map[Long, V], removed: Long => Boolean = _ => false): SortedLongMap[V] = {
    val adds  = added.toArray.sortInPlaceBy(_._1)
    val outK  = new Array[Long](keys.length + adds.length)
    val outV  = new Array[AnyRef](keys.length + adds.length)
    var i, j, n = 0
    def emit(k: Long, v: AnyRef): Unit = { outK(n) = k; outV(n) = v; n += 1 }
    while (i < keys.length || j < adds.length) {
      if (j >= adds.length || (i < keys.length && keys(i) < adds(j)._1)) { if (!removed(keys(i))) emit(keys(i), values(i)); i += 1 }
      else {
        if (i < keys.length && keys(i) == adds(j)._1) i += 1
        emit(adds(j)._1, adds(j)._2); j += 1
      }
    }
    new SortedLongMap[V](java.util.Arrays.copyOf(outK, n), java.util.Arrays.copyOf(outV, n))
  }
}

private[identity] object SortedLongMap {
  def empty[V <: AnyRef]: SortedLongMap[V] = new SortedLongMap[V](Array.emptyLongArray, Array.emptyObjectArray)
}
