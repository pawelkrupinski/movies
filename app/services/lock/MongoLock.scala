package services.lock

import com.mongodb.MongoWriteException
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonDateTime, BsonString}
import org.mongodb.scala.model.{Filters, UpdateOptions, Updates}
import play.api.Logging

import java.net.InetAddress
import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Mongo-backed named lock. Stores one document per lock name in the
 * collection passed in (typically `kinowo.locks`).
 *
 * Document shape:
 * {{{
 *   { _id: "scrape-lock-helios", holder: "host-1234", expiresAt: ISODate(...) }
 * }}}
 *
 * `acquire` is an `updateOne(filter, set, upsert=true)` where the filter
 * matches docs that either don't exist (the upsert path) or have an expired
 * `expiresAt` (the takeover path). If a currently-held lock exists and the
 * upsert tries to insert a duplicate `_id`, the resulting `DuplicateKeyError`
 * is caught and returned as `false` — the standard Mongo distributed-lock
 * idiom.
 *
 * `holderId` is per-instance, derived from `${hostname}-${pid}`, so two pods
 * on the same host with different PIDs are distinct holders. Each `MongoLock`
 * instance assumes a single thread of execution — the holderId doesn't
 * encode the thread, so two threads sharing the instance would each appear
 * to hold the lock from the perspective of `release`/`heartbeat`. Use one
 * instance per logical worker.
 */
class MongoLock(coll: MongoCollection[Document]) extends Lock with Logging {

  private val holderId: String = {
    val host = Try(InetAddress.getLocalHost.getHostName).getOrElse("unknown")
    val pid  = ProcessHandle.current().pid()
    s"$host-$pid"
  }

  override def acquire(name: String, ttl: FiniteDuration): Boolean = {
    val now       = Instant.now()
    val expiresAt = now.plusMillis(ttl.toMillis)
    // Filter: this _id either doesn't exist (upsert creates it) or its
    // `expiresAt` is in the past (we take it over). When a current holder
    // is still alive, the filter matches nothing AND the upsert's attempt
    // to insert a duplicate `_id` throws a 11000 — caught and returned as
    // "didn't acquire".
    val filter = Filters.and(
      Filters.eq("_id", name),
      Filters.or(
        Filters.exists("holder", exists = false),
        Filters.lt("expiresAt", BsonDateTime(now.toEpochMilli))
      )
    )
    val update = Updates.combine(
      Updates.set("holder",    BsonString(holderId)),
      Updates.set("expiresAt", BsonDateTime(expiresAt.toEpochMilli))
    )
    Try {
      Await.result(coll.updateOne(filter, update, new UpdateOptions().upsert(true)).toFuture(), 10.seconds)
      true
    }.recover {
      case ex: MongoWriteException if Option(ex.getError).exists(_.getCode == 11000) =>
        // Duplicate-key on upsert — another holder owns this lock.
        false
      case ex: Throwable =>
        logger.warn(s"MongoLock.acquire($name) failed: ${ex.getMessage}")
        false
    }.getOrElse(false)
  }

  override def heartbeat(name: String, ttl: FiniteDuration): Boolean = {
    val expiresAt = Instant.now().plusMillis(ttl.toMillis)
    Try {
      val result = Await.result(coll.updateOne(
        Filters.and(Filters.eq("_id", name), Filters.eq("holder", BsonString(holderId))),
        Updates.set("expiresAt", BsonDateTime(expiresAt.toEpochMilli))
      ).toFuture(), 10.seconds)
      result.getMatchedCount > 0
    }.recover {
      case ex: Throwable =>
        logger.warn(s"MongoLock.heartbeat($name) failed: ${ex.getMessage}")
        false
    }.getOrElse(false)
  }

  override def release(name: String): Unit = {
    Try {
      Await.result(coll.deleteOne(Filters.and(
        Filters.eq("_id", name),
        Filters.eq("holder", BsonString(holderId))
      )).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable =>
        logger.warn(s"MongoLock.release($name) failed: ${ex.getMessage}")
    }
  }
}
