package services.identity

import scala.collection.mutable

import models.ListingKey
import services.identity.measures.ResolverDecision

/**
 * What a [[LiveProjectionIndex]] knows per listing key — the listing published under it, its previous film, its cluster,
 * the decision naming it, the stored films whose slot it is — as ONE [[ListingEntries.Entry]] in one map: a map per field
 * kept a hash node (and, for the immutable ones, a share of a trie) per key in each, ~11 MB of worker-us's ~90k listings.
 * An entry goes once it holds nothing, and is filed under the published listing's own key object when there is one. The
 * index's per-key maps are views of it ([[byKey]], [[previousOf]], [[clusterOf]]), as the entries stand.
 *
 * Single-threaded: the projection's tick holds it.
 */
private[identity] final class ListingEntries {
  import ListingEntries.Entry

  private val entries = mutable.HashMap.empty[ListingKey, Entry]
  private var published, placed, clustered = 0   // how many entries hold a listing, a previous film, a cluster

  def get(k: ListingKey): Option[Entry] = entries.get(k)
  def apply(k: ListingKey): Entry = entries(k)
  def valuesIterator: Iterator[Entry] = entries.valuesIterator
  /** `k`'s entry, a new one when it has none. */
  def entry(k: ListingKey): Entry = entries.getOrElseUpdate(k, new Entry(k))
  /** `e` gone, when it holds nothing. */
  def tidy(e: Entry): Unit = if (e.holdsNothing) entries.remove(e.key)

  def listingAt(k: ListingKey): Option[ProjectedListing] = entries.get(k).flatMap(e => Option(e.listing))
  def isPublished(k: ListingKey): Boolean = entries.get(k).exists(_.listing != null)
  def previousAt(k: ListingKey): Option[StoredFilmRef] = entries.get(k).flatMap(e => Option(e.previous))

  /** Publish `l` under its key, the entry re-filed under the listing's own key object. */
  def publish(l: ProjectedListing): Unit = {
    val k = l.listing.key
    val e = entries.get(k) match {
      case Some(e) if e.key ne k => entries.remove(k); e.key = k; entries(k) = e; e
      case Some(e)               => e
      case None                  => val e = new Entry(k); entries(k) = e; e
    }
    if (e.listing == null) published += 1
    e.listing = l
  }
  def unpublish(k: ListingKey): Unit = entries.get(k).filter(_.listing != null).foreach { e =>
    e.listing = null; published -= 1; tidy(e)
  }
  def place(k: ListingKey, ref: StoredFilmRef): Unit = { val e = entry(k); if (e.previous == null) placed += 1; e.previous = ref }
  def unplace(k: ListingKey): Unit = entries.get(k).filter(_.previous != null).foreach { e =>
    e.previous = null; placed -= 1; tidy(e)
  }
  def cluster(k: ListingKey, id: ClusterId): Unit = { val e = entry(k); if (e.cluster == null) clustered += 1; e.cluster = id }
  def uncluster(k: ListingKey): Unit = entries.get(k).filter(_.cluster != null).foreach { e =>
    e.cluster = null; clustered -= 1; tidy(e)
  }

  /** One field of every entry as a map, as the entries stand. */
  private final class FieldView[V <: AnyRef](field: Entry => V, count: () => Int) extends scala.collection.immutable.AbstractMap[ListingKey, V] {
    def get(key: ListingKey): Option[V] = entries.get(key).flatMap(e => Option(field(e)))
    override def contains(key: ListingKey): Boolean = entries.get(key).exists(field(_) != null)
    def iterator: Iterator[(ListingKey, V)] = entries.valuesIterator.filter(field(_) != null).map(e => e.key -> field(e))
    override def size: Int = count()
    override def knownSize: Int = count()
    override def isEmpty: Boolean = count() == 0
    def removed(key: ListingKey): Map[ListingKey, V] = Map.from(iterator).removed(key)
    def updated[V1 >: V](key: ListingKey, value: V1): Map[ListingKey, V1] = Map.from(iterator).updated(key, value)
  }
  val byKey: Map[ListingKey, ProjectedListing]     = new FieldView(_.listing, () => published)
  val previousOf: Map[ListingKey, StoredFilmRef]   = new FieldView(_.previous, () => placed)
  val clusterOf: Map[ListingKey, ClusterId]        = new FieldView(_.cluster, () => clustered)
}

private[identity] object ListingEntries {
  /** What the index knows of one listing key: the listing published under it, its previous film, its cluster and the
   *  decision naming it (each null for none), and the stored films whose slot it is. */
  final class Entry(var key: ListingKey) {
    var listing: ProjectedListing   = null
    var previous: StoredFilmRef   = null
    var cluster: ClusterId          = null
    var decision: ResolverDecision  = null
    var owners: Set[StoredFilmRef] = Set.empty
    var heldAt: Int                  = -1   // the last update whose model held the key: a mark, not something it holds
    def holdsNothing: Boolean = listing == null && previous == null && cluster == null && decision == null && owners.isEmpty
  }
}
