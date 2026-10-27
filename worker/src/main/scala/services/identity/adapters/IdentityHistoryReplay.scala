package services.identity
package adapters

import org.bson.BsonDocument
import services.movies.ListingKey

import java.util.concurrent.ConcurrentHashMap

/**
 * A past moment of the identity system, rebuilt from what it reads now and the history written since
 * ([[IdentityHistory]]): each answer, venue page and listing as it stood at `at` — the value the first entry after `at`
 * replaced, or, none moved it since, what it is now ([[IdentityHistory.valueAt]]). A resolve over them takes what the
 * worker's model took at `at` (P1: a resolve is a function of its listings and answers), so a take that flipped can be
 * resolved again at each side of the flip (`scripts/identity-replay`).
 *
 * `entries`: the history written after `at` — every entry of the kinds read, whatever its id.
 */
final class IdentityHistoryReplay(entries: Seq[HistoryEntry], at: Long) {
  private val byKindAndId: Map[(String, String), Seq[HistoryEntry]] = entries.filter(_.at > at).groupBy(e => (e.kind, e.id))

  /** The value `kind`'s document `id` held at `at`, given what it holds now. */
  def valueAt(kind: String, id: String, current: => Option[BsonDocument]): Option[BsonDocument] =
    byKindAndId.get((kind, id)).fold(current)(IdentityHistory.valueAt(_, at, current))

  /** The listings the model held at `at`, given those it holds now: each one an entry moved since as that entry found it
   *  — one taken in since left out, one let go since back in. */
  def listings(current: Seq[Listing]): Seq[Listing] = {
    val now     = current.map(l => ListingKey.serialised(l.key) -> l).toMap
    val touched = byKindAndId.keySet.collect { case (HistoryEntry.Listing, id) => id }
    val kept    = now.view.filterKeys(id => !touched(id)).values.toSeq
    val past    = touched.toSeq.flatMap(id => valueAt(HistoryEntry.Listing, id, now.get(id).map(ListingBson.doc)).map(ListingBson.of))
    Listing.distinct(kept ++ past)
  }

  /** The venue pages as `venue_pages` held them at `at`, given its documents now (each by its `_id`). */
  def pages(current: Seq[BsonDocument]): Seq[BsonDocument] = {
    val kind    = services.venuepages.HistoryVenuePageStore.Kind
    val now     = current.map(d => d.getString("_id").getValue -> d).toMap
    val touched = byKindAndId.keySet.collect { case (`kind`, id) => id }
    now.view.filterKeys(id => !touched(id)).values.toSeq ++ touched.toSeq.flatMap(id => valueAt(kind, id, now.get(id)))
  }

  /** `current`'s documents as they stood at `at`, each as an answer reads it. What a replay writes — the question
   *  memory marking the gaps it met — stays here, never reaching `current`. */
  def documents(current: TmdbDocuments): TmdbDocuments = new TmdbDocuments {
    private val written = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, BsonDocument]()).toMap
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
      val (own, theirs) = ids.distinct.partition(written(kind).containsKey)
      lazy val now      = current.answers(kind, theirs)
      own.map(id => id -> written(kind).get(id).clone()).toMap ++
        theirs.flatMap(id => valueAt(HistoryEntry.answer(kind), id, now.get(id)).map(id -> _.clone()))
    }
    override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] =
      get(kind, ids).view.mapValues(TmdbDocuments.project(kind, _)).toMap
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = docs.foreach { case (id, d) => written(kind).put(id, d.clone()) }
  }
}

object IdentityHistoryReplay {
  /** The kinds a replay reads. */
  val Kinds: Seq[String] = HistoryEntry.Answers ++ Seq(services.venuepages.HistoryVenuePageStore.Kind, HistoryEntry.Listing)
  def from(reader: IdentityHistoryReader, at: Long): IdentityHistoryReplay = new IdentityHistoryReplay(reader.since(Kinds, at), at)
}
