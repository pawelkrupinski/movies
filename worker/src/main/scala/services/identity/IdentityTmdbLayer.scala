package services.identity

import org.mongodb.scala.MongoDatabase

import java.time.Clock

/**
 * The identity model's answers from TMDB, IMDb and the other film database families, as one layer over one
 * database: the documents ([[TmdbDocuments]]), the store that files TMDB's answers into them and announces what moved
 * ([[TmdbStore]]), and the store of the families' answers kept beside them ([[FamilyAnswerStore]]).
 *
 * One layer, because its parts keep state about each other: the store's listeners hear only its own filings, and a
 * family-answer store counts only its own (`version`), so a reader handed documents another store wrote would never
 * learn they moved. Whoever holds the layer gets all three together.
 *
 * A worker builds its own over its country's database. The order-independence replay hands one to all its passes, so
 * a TMDB answer one pass fetched is filed once and read by the others as a warm store's answer — production's case
 * after the first tick, not a cold store three times over.
 *
 * Over Mongo, the documents are coalesced in front of a cache in front of the collections ([[CoalescedTmdbDocuments]],
 * [[CachedTmdbDocuments]]); without one, in memory.
 */
final class IdentityTmdbLayer(database: Option[MongoDatabase], clock: Clock) {
  /** Where the documents are kept, under the coalescing: what the retention sweep scans and deletes through. */
  lazy val backend: TmdbDocuments & TmdbDocumentRetention =
    database.fold[TmdbDocuments & TmdbDocumentRetention](new InMemoryTmdbDocuments)(db => new CachedTmdbDocuments(new MongoTmdbDocuments(db)))
  lazy val documents: TmdbDocuments = database.fold[TmdbDocuments](backend)(_ => new CoalescedTmdbDocuments(backend))
  lazy val store: TmdbStore = new TmdbStore(documents, clock)
  lazy val familyAnswers: FamilyAnswerStore = new FamilyAnswerStore(documents, clock)
}
