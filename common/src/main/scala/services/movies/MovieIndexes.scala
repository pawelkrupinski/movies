package services.movies

import org.mongodb.scala.model.{Filters, IndexOptions, Indexes}
import org.mongodb.scala.{MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** The `movies` collection's indexes, built on [[MongoMovieRepository]]'s first use. */
private[movies] object MovieIndexes extends Logging {
  /** The `key` index and its one-time backfill. Idempotent + best-effort: a re-create is
   *  a no-op, a failure only logs. (The `(title, year)` index that used to serve
   *  `delete`'s column-matching fallback is not created any more — nothing queries
   *  those columns; the index left on prod is inert.) */
  def ensure(db: MongoDatabase, coll: MongoCollection[StoredMovieDto]): Unit = {
    // The lookup key lives in its own field now that `_id` is the permanent `FilmId`
    // (see [[FilmId]]). A document written before then has no `key` — its `_id` IS its
    // key — so backfill it once, here, before anything queries by key: the cache's cold
    // lookup filters on `key`. Idempotent
    // (only documents lacking the field) and a pipeline update, so one round trip.
    Try {
      val backfilled = Await.result(coll.updateMany(Filters.exists("key", false),
        Seq(org.mongodb.scala.bson.collection.immutable.Document("$set" -> org.mongodb.scala.bson.collection.immutable.Document("key" -> "$_id")))).toFuture(), 60.seconds)
      if (backfilled.getModifiedCount > 0)
        logger.info(s"movies: backfilled `key` from `_id` on ${backfilled.getModifiedCount} document(s) written before film ids.")
    }.recover {
      case exception: Throwable => logger.warn(s"movies `key` index/backfill failed: ${exception.getMessage}")
    }
    // One document per film: `tmdbId` unique over the documents that HAVE one. The
    // write-time fold (`MovieCache.put`) already merges a duplicate before it is written;
    // this is the store refusing the one that slips past a race, so the settle never has
    // a same-tmdbId pair to merge. A refused write is logged by `upsert` and the film
    // keeps its previous document until the next scrape writes it again. Measured
    // 2026-09-06: zero duplicate tmdbIds in any country, so the index builds clean.
    //
    // PARTIAL on `tmdbId` being a number, not SPARSE: the codec writes an absent option
    // as `tmdbId: null`, and a sparse index indexes null as a value — every unresolved
    // row would collide on it (found by the integration suite, not production).
    // An earlier definition that differs in more than uniqueness is reported, not rebuilt:
    // see `MongoIndex` for why nothing here drops an index.
    ensureIndex(db, "tmdbId", new IndexOptions().unique(true).background(true)
      .partialFilterExpression(org.mongodb.scala.bson.collection.immutable.Document(
        "tmdbId" -> org.mongodb.scala.bson.collection.immutable.Document("$type" -> "number"))))
    // One document per stored key: the key is the lookup identity now, and the cache
    // refuses a second row under a key another film holds (`MovieCache.persist`); this
    // is the store refusing the one a race lets through. Measured 2026-09-07: zero
    // duplicate keys in any country, so the index builds clean. Every document has
    // the field after the backfill above, so plain unique — no partial filter.
    ensureIndex(db, "key", new IndexOptions().unique(true).background(true))
    // `updatedAt` is bumped on every write and read by the change-stream catch-up
    // (`foreachRecordUpdatedSince`): "the rows written since the cursor last delivered" has
    // to be a range read on an index, or the 30-minute prune sweep that asks it would scan
    // the collection to find, in steady state, nothing.
    ensureIndex(db, "updatedAt", new IndexOptions().background(true))
  }

  /** A single-field index on `field`, through [[services.MongoIndex]] — which converts
   *  an earlier plain index to unique in place and never drops one. A failure is logged
   *  there, never fatal: the film store works without the index — a unique one merely
   *  stops refusing duplicates, a plain one merely stops serving its range read. */
  private def ensureIndex(db: MongoDatabase, field: String, options: IndexOptions): Unit = {
    services.MongoIndex.ensure(db, MovieRepository.Collection, Indexes.ascending(field), options, "movies")
    ()
  }
}
