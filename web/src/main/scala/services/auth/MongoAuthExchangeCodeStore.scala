package services.auth

import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.model.{Filters, Indexes}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * [[AuthExchangeCodeStore]] on Mongo, in the SHARED users database.
 *
 * Which database is the whole point. The cross-domain handoff mints a code on
 * `kinowo.net`'s pod and redeems it on `showtimes.cc`'s — two processes — so an
 * in-process cache cannot carry it, and a per-country database would leave the
 * code as unreachable as the session cookie it replaces. It lives beside `users`
 * and `userStates` in `Country.usersDbName` for exactly that reason.
 *
 * `db` is `None` when there is no Mongo (local dev): the store then holds nothing
 * and every redemption misses, which is why Wiring runs
 * [[InMemoryAuthExchangeCodeStore]] there instead.
 */
class MongoAuthExchangeCodeStore(db: Option[MongoDatabase]) extends AuthExchangeCodeStore with Logging {

  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection(MongoAuthExchangeCodeStore.CollectionName))

  // Off-thread so boot never waits on Mongo to build an index — the store works
  // without it (the code is the `_id`), the index only sweeps up the codes nobody
  // ever came back to redeem.
  coll.foreach { c =>
    val thread = new Thread(() => ensureTtlIndex(c), s"${MongoAuthExchangeCodeStore.CollectionName}-init")
    thread.setDaemon(true)
    thread.start()
  }

  override def put(pending: PendingExchangeCode): Unit = coll.foreach { c =>
    Try(Await.result(c.insertOne(Document(
      "_id"      -> pending.code,
      "userId"   -> pending.userId,
      "issuedAt" -> new java.util.Date(pending.issuedAt.toEpochMilli)
    )).toFuture(), MongoAuthExchangeCodeStore.Timeout))
      // WARN, not debug: the visitor lands signed out on the far side and has no
      // way to tell why, so this line is the only trace the handoff was even
      // attempted.
      .recover { case exception =>
        logger.warn(s"Auth exchange code not stored — the handoff will land signed out: ${exception.getMessage}") }
  }

  /** `findOneAndDelete` rather than find-then-delete: single-use has to survive
   *  two browsers arriving with the same code at once, and only the server can
   *  make that one step. */
  override def remove(code: String): Option[PendingExchangeCode] = coll.flatMap { c =>
    Try(Await.result(c.findOneAndDelete(Filters.eq("_id", code)).headOption(), MongoAuthExchangeCodeStore.Timeout))
      .recover { case exception =>
        logger.warn(s"Auth exchange code lookup failed: ${exception.getMessage}"); None }
      .toOption.flatten
      .flatMap(toPending)
  }

  private def toPending(document: Document): Option[PendingExchangeCode] = for {
    code     <- Option(document.getString("_id"))
    userId   <- Option(document.getString("userId"))
    issuedAt <- Option(document.getDate("issuedAt"))
  } yield PendingExchangeCode(code, userId, Instant.ofEpochMilli(issuedAt.getTime))

  /** Housekeeping only — [[AuthExchangeCodes.redeem]] is what actually refuses an
   *  expired code, and it does so the instant the code is presented. Mongo's TTL
   *  monitor runs about once a minute, so this index is the sweep that stops
   *  unredeemed codes accumulating, not the expiry rule. Given a minute of slack
   *  over the TTL for that reason: a row deleted slightly late costs nothing, and
   *  one deleted early would be indistinguishable from a code that never existed. */
  private def ensureTtlIndex(c: MongoCollection[Document]): Unit =
    Try {
      Await.result(c.createIndex(
        Indexes.ascending("issuedAt"),
        new JIndexOptions().expireAfter(AuthExchangeCodes.Ttl.getSeconds + 60, TimeUnit.SECONDS)
      ).toFuture(), 10.seconds)
    }.recover { case exception =>
      logger.debug(s"Auth exchange code TTL index not created: ${exception.getMessage}") }
}

object MongoAuthExchangeCodeStore {
  val CollectionName = "authExchangeCodes"

  /** Short: this sits on the redirect a visitor is watching, and a slow Mongo
   *  should drop them on a sign-in button rather than hang the hop. */
  val Timeout: FiniteDuration = 5.seconds
}
