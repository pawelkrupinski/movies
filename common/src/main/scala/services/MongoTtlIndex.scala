package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Indexes
import play.api.Logging

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** Bring a single-field TTL index in line with a wanted `expireAfterSeconds`, by
 *  READING IT BACK FIRST.
 *
 *  `createIndex` can create a TTL index but never ALTER one, so the only way to
 *  change an existing expiry is `collMod`. Both callers used to fire that
 *  `collMod` unconditionally on every construction and swallow the result at
 *  `logger.debug`, which hid two things at once:
 *
 *  1. `kinowo_app` holds `readWrite` on the country databases, and `readWrite`
 *     DOES NOT INCLUDE `collMod`. Every one of those commands came back
 *     `Unauthorized` (13), so the reconciliation the comments described has
 *     never once run in production, and nothing said so because `debug` is not
 *     enabled there. Measured 2026-09-05: ~30 rejected `collMod`s per pod boot,
 *     ~300 across a ten-pod rollout, each counted by mongod as a USER ASSERT —
 *     enough to help trip `MongodUserAssertionsRising` on a five-worker
 *     `Recreate` rollout.
 *  2. Every one of those commands was also a NO-OP. All thirty live indexes
 *     already carried the wanted expiry, so an authorised `collMod` would have
 *     changed nothing either.
 *
 *  `listIndexes` IS covered by `readWrite`, so reading the current expiry costs
 *  no privilege we don't have. That turns the order around: read, compare, and
 *  send the `collMod` only when it would actually change something — at which
 *  point the failure is worth a `warn` naming both values, because an operator
 *  then has to run it by hand (or grant the action). In the steady state this
 *  sends no `collMod` at all.
 *
 *  Every step is a `Try`: a collection whose TTL cannot be reconciled must not
 *  stop the monitor or the resolution store from running. */
object MongoTtlIndex extends Logging {

  /** Ensure `collection` carries a TTL index on `field` expiring after
   *  `wantedSeconds` — creating it when absent, reconciling it when it
   *  disagrees, and doing nothing at all when it already agrees.
   *
   *  `db` MUST be the database `collection` lives in: `collMod` is a database
   *  command naming the collection, so a mismatched pair addresses a collection
   *  in the wrong database. The collection itself is passed rather than looked up
   *  from `db` so the caller's own handle — and the write concern it configured —
   *  is the one that builds the index.
   *
   *  `label` names the CALLER, not the collection: this logs the collection
   *  itself, so passing a collection name here reads `resolve_tmdb: resolve_tmdb
   *  TTL index on ...`. */
  def reconcile(
    db:            MongoDatabase,
    collection:    MongoCollection[Document],
    field:         String,
    wantedSeconds: Long,
    label:         String
  ): Unit = {
    val name = collection.namespace.getCollectionName
    currentExpiry(collection, field) match {
      case Some(actual) if actual == wantedSeconds => ()

      case Some(actual) =>
        logger.warn(s"$label: $name TTL index on `$field` expires after ${actual}s, want ${wantedSeconds}s — reconciling with collMod.")
        applyCollMod(db, name, field, wantedSeconds, label)

      case None =>
        Try {
          Await.result(collection.createIndex(
            Indexes.ascending(field),
            new JIndexOptions().expireAfter(wantedSeconds, TimeUnit.SECONDS)
          ).toFuture(), 10.seconds)
        }.recover { case exception =>
          // `IndexOptionsConflict` HERE MEANS THE READ ABOVE FAILED, not that the index is
          // absent — `currentExpiry` returns None for an unreadable collection too, and
          // `createIndex` is then rejected by the index it could not see. Saying "could not
          // be created" would send a reader looking for a missing index that is right there
          // with the wrong expiry, so name both possibilities.
          logger.warn(s"$label: $name has no readable TTL index on `$field` and one could not be created — " +
            s"if it exists, it KEEPS ITS OLD EXPIRY rather than ${wantedSeconds}s: ${exception.getMessage}")
        }
    }
  }

  /** The `expireAfterSeconds` of the existing single-field TTL index on `field`,
   *  or None when there is no such index — or when the read itself failed, in
   *  which case `createIndex` is the right next move and reports properly if the
   *  index is in fact already there. */
  private def currentExpiry(collection: MongoCollection[Document], field: String): Option[Long] =
    Try {
      Await.result(collection.listIndexes().toFuture(), 10.seconds).flatMap { index =>
        val onFieldAlone = index.get("key")
          .collect { case keys: org.bson.BsonDocument => keys }
          .exists(keys => keys.size == 1 && keys.containsKey(field))
        if (onFieldAlone) index.get("expireAfterSeconds").collect { case seconds: org.bson.BsonNumber => seconds.longValue() }
        else None
      }.headOption
    }.recover { case exception =>
      logger.debug(s"${collection.namespace.getCollectionName} index read failed, treating `$field` as un-indexed: ${exception.getMessage}")
      None
    }.toOption.flatten

  /** The one command `readWrite` cannot issue. WARN on failure, not debug: by the
   *  time we get here the expiry genuinely disagrees, so a silent failure leaves
   *  documents living for the wrong length of time with nothing saying so. */
  private def applyCollMod(db: MongoDatabase, collection: String, field: String, wantedSeconds: Long, label: String): Unit =
    Try {
      val command = new org.bson.Document("collMod", collection)
        .append("index", new org.bson.Document("keyPattern", new org.bson.Document(field, 1))
          .append("expireAfterSeconds", wantedSeconds))
      Await.result(db.runCommand[Document](command).toFuture(), 10.seconds)
      logger.info(s"$label: $collection TTL index on `$field` now expires after ${wantedSeconds}s.")
    }.recover { case exception =>
      logger.warn(s"$label: collMod failed for $collection.`$field` → ${wantedSeconds}s, so the index KEEPS ITS OLD EXPIRY: ${exception.getMessage}")
    }
}
