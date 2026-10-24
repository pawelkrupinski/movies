package services.identity
package adapters

import org.mongodb.scala.MongoDatabase
import services.metrics.CacheOccupancy

import java.time.Clock

/**
 * The identity model's answers from TMDB, IMDb and the other film database families, as one layer over one
 * database: the documents ([[TmdbDocuments]]), the store that files TMDB's answers into them and announces what moved
 * ([[TmdbStore]]), and the store of the families' answers kept beside them ([[FamilyAnswerStore]]).
 *
 * One layer, because its parts announce their filings on one feed ([[feed]]): a reader handed documents another
 * layer's store wrote would never learn they moved. The TMDB store publishes a document whose value moved, the family
 * store every answer it files, and the worker's venue page index (`VenuePageIndex`) a page whose answer moved; the
 * model subscribes, and the agreement stage counts the keys its questions name through the family store's view
 * (`FamilyAnswerStore.changes`). Whoever holds the layer gets them together; a worker builds its own over its country's
 * database.
 *
 * Over Mongo, the documents are coalesced in front of a cache in front of the collections ([[CoalescedTmdbDocuments]],
 * [[CachedTmdbDocuments]]); without one, in memory.
 */
final class IdentityTmdbLayer(database: Option[MongoDatabase], clock: Clock) {
  /** Where the documents are kept, under the coalescing: what the retention sweep scans and deletes through. */
  lazy val backend: TmdbDocuments & TmdbDocumentRetention = cached.getOrElse(new InMemoryTmdbDocuments)
  private lazy val cached: Option[CachedTmdbDocuments] = database.map(db => new CachedTmdbDocuments(new MongoTmdbDocuments(db)))
  /** The document cache's occupancy by cache name, for `kinowo_worker_cache_*`; none in memory, where there is no cache. */
  lazy val cacheOccupancies: Seq[(String, () => CacheOccupancy)] = cached.toSeq.flatMap(_.occupancies)
  lazy val documents: TmdbDocuments = database.fold[TmdbDocuments](backend)(_ => new CoalescedTmdbDocuments(backend))
  /** Where every filing of the layer's stores, and of the venue page index, is announced. */
  lazy val feed: services.identity.agreement.AnswerFeed = new FiledAnswerFeed
  lazy val store: TmdbStore = new TmdbStore(documents, clock, feed)
  lazy val familyAnswers: FamilyAnswerStore = new FamilyAnswerStore(documents, clock, feed)
}
