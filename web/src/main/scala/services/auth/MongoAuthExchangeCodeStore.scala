package services.auth

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

import org.mongodb.scala.bson.{BsonDateTime, BsonDocument, BsonString}
import org.mongodb.scala.{Document, MongoDatabase, documentToUntypedDocument}
import play.api.Logging

/**
 * [[AuthExchangeCodeStore]] over a keyed store ([[tools.KeyedStore]]) -- in Mongo, in the SHARED users database.
 *
 * Which database is the whole point. The cross-domain handoff mints a code on
 * `kinowo.net`'s pod and redeems it on `showtimes.cc`'s — two processes — so an
 * in-process cache cannot carry it, and a per-country database would leave the
 * code as unreachable as the session cookie it replaces. It lives beside `users`
 * and `userStates` in `UsersDatabase.name` for exactly that reason.
 *
 * `documents` is `None` when there is no Mongo (local dev): the store then holds nothing
 * and every redemption misses, which is why Wiring runs
 * [[InMemoryAuthExchangeCodeStore]] there instead.
 */
abstract class KeyedAuthExchangeCodeStore(documents: Option[tools.StoreDocuments]) extends AuthExchangeCodeStore with Logging {
  protected val codes = new tools.KeyedStore(documents.getOrElse(tools.StoreDocuments.Absent), tools.KeyCodec.Text, KeyedAuthExchangeCodeStore.Codec)

  override def put(pending: PendingExchangeCode): Unit = if (documents.isDefined) {
    // WARN, not debug: the visitor lands signed out on the far side and has no
    // way to tell why, so this line is the only trace the handoff was even
    // attempted.
    def lost(why: String): Unit = logger.warn(s"Auth exchange code not stored — the handoff will land signed out: $why")
    Try(codes.insert(pending.code, pending)) match {
      case Success(true)      => ()
      case Success(false)     => lost("the code is already held")
      case Failure(exception) => lost(exception.getMessage)
    }
  }

  /** A take rather than find-then-delete: single-use has to survive
   *  two browsers arriving with the same code at once, and only the store
   *  can make that one step. */
  override def remove(code: String): Option[PendingExchangeCode] =
    Try(codes.take(code)).recover { case exception =>
      logger.warn(s"Auth exchange code lookup failed: ${exception.getMessage}"); None }.get
}

object KeyedAuthExchangeCodeStore {
  /** Housekeeping only — [[AuthExchangeCodes.redeem]] is what actually refuses an
   *  expired code, and it does so the instant the code is presented. Mongo's TTL
   *  monitor runs about once a minute, so this expiry is the sweep that stops
   *  unredeemed codes accumulating, not the expiry rule. Given a minute of slack
   *  over the TTL for that reason: a row deleted slightly late costs nothing, and
   *  one deleted early would be indistinguishable from a code that never existed. */
  val Expiry: tools.DocumentExpiry = tools.DocumentExpiry("issuedAt", (AuthExchangeCodes.Ttl.getSeconds + 60).seconds)

  val Codec: tools.DocCodec[PendingExchangeCode] = new tools.DocCodec[PendingExchangeCode] {
    def encode(pending: PendingExchangeCode): Document = Document(MongoAuthExchangeCodeStore.encode(pending))
    def decode(document: Document): Option[PendingExchangeCode] = MongoAuthExchangeCodeStore.decode(document)
  }
}

/** The `authExchangeCodes` collection of the shared users database, or nothing without one. `timeout` bounds each
 *  round-trip; a miss past it reads as no code. Production keeps [[MongoAuthExchangeCodeStore.Timeout]]; a spec checking
 *  what the store hands back, not how fast, passes a budget a loaded shared test Mongo can meet. */
class MongoAuthExchangeCodeStore(db: Option[MongoDatabase], timeout: FiniteDuration = MongoAuthExchangeCodeStore.Timeout) extends KeyedAuthExchangeCodeStore(
  db.map(d => tools.MongoStoreDocuments.of(d.getCollection(MongoAuthExchangeCodeStore.CollectionName), Some(KeyedAuthExchangeCodeStore.Expiry), timeout)))

object MongoAuthExchangeCodeStore {
  val CollectionName = "authExchangeCodes"

  /** A pending code as stored. */
  def encode(pending: PendingExchangeCode): BsonDocument = {
    val document = new BsonDocument()
      .append("_id",      BsonString(pending.code))
      .append("userId",   BsonString(pending.userId))
      .append("issuedAt", BsonDateTime(pending.issuedAt.toEpochMilli))
    pending.binding.foreach(binding => document.append("binding", BsonString(binding)))
    pending.challenge.foreach(challenge => document.append("challenge", BsonString(challenge)))
    document
  }

  /** A stored pending code; None for a malformed document. */
  def decode(document: Document): Option[PendingExchangeCode] = for {
    code     <- Option(document.getString("_id"))
    userId   <- Option(document.getString("userId"))
    issuedAt <- Option(document.getDate("issuedAt"))
  } yield PendingExchangeCode(code, userId, Instant.ofEpochMilli(issuedAt.getTime), Option(document.getString("binding")),
    Option(document.getString("challenge")))

  /** Short: this sits on the redirect a visitor is watching, and a slow Mongo
   *  should drop them on a sign-in button rather than hang the hop. */
  val Timeout: FiniteDuration = 5.seconds
}
