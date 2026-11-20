package services.identity

/**
 * A listing's measures against a film ([[IdentityMeasures.listingFilmTitled]]) as a map of its fixed keys, each value in a
 * slot of one array: built for every listing/film pair the resolver weighs, the 14-entry `HashMap` and its entries' tuples
 * were among worker-pl's identity model's largest allocations (JFR 2026-10-05). A map like any other to read, compare and
 * hash; a change to one of its keys is another such map, any other change an ordinary one.
 */
final class ListingFilmMeasures private[identity] (private val slots: Array[Measure])
    extends scala.collection.immutable.AbstractMap[String, Measure] {
  import ListingFilmMeasures.{Keys, slotOf}

  def get(key: String): Option[Measure] = { val i = slotOf(key); if (i < 0) None else Some(slots(i)) }
  override def getOrElse[V1 >: Measure](key: String, default: => V1): V1 = { val i = slotOf(key); if (i < 0) default else slots(i) }
  override def contains(key: String): Boolean = slotOf(key) >= 0
  override def apply(key: String): Measure = { val i = slotOf(key); if (i < 0) default(key) else slots(i) }
  def iterator: Iterator[(String, Measure)] = Keys.indices.iterator.map(i => Keys(i) -> slots(i))
  override def keysIterator: Iterator[String]                    = Keys.iterator
  override def valuesIterator: Iterator[Measure] = slots.iterator
  override def size: Int      = Keys.length
  override def knownSize: Int = Keys.length
  override def isEmpty: Boolean = false

  def updated[V1 >: Measure](key: String, value: V1): Map[String, V1] = (slotOf(key), value) match {
    case (i, m: Measure) if i >= 0 => val next = slots.clone(); next(i) = m; new ListingFilmMeasures(next)
    case _                                         => scala.collection.immutable.HashMap.from[String, V1](this).updated(key, value)
  }
  def removed(key: String): Map[String, Measure] =
    if (slotOf(key) < 0) this else scala.collection.immutable.HashMap.from(this).removed(key)
}

object ListingFilmMeasures {
  /** The keys, by slot. */
  val Keys: IndexedSeq[String] = IndexedSeq("title", "numeral", "originalTitle", "year.delta", "year.distance", "titleYear.delta",
    "season.delta", "director", "runtime.delta", "country", "search.rank", "popularity.log2", "rivals", "venues.corroborating",
    "certificate")
  private val Slots = { val m = new java.util.HashMap[String, Integer](Keys.length * 2); Keys.zipWithIndex.foreach { case (k, i) => m.put(k, i) }; m }
  private def slotOf(key: String): Int = { val i = Slots.get(key); if (i == null) -1 else i.intValue }
}
