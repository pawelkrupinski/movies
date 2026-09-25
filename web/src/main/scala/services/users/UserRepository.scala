package services.users

import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument, Updates}
import org.bson.{BsonDocument, BsonDocumentWriter, BsonInt32}
import org.bson.codecs.EncoderContext
import org.mongodb.scala.bson.conversions.Bson
import models.User
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Persistent store for authenticated users. Identity is the user's
 * email (lowercased) — logging in with Google, Facebook, or Apple
 * using the same email accesses the same account and state.
 *
 * The trait is what callers (AuthController, UserStateController) see;
 * `MongoUserRepository` is the production impl, `InMemoryUserRepository` the test
 * fake. Pattern mirrors `MovieRepository`.
 */
trait UserRepository {
  def enabled: Boolean

  /** Look up by id (= lowercased email). Used on every authenticated
   *  request after session decode, and during OAuth callback.
   *
   *  Every lookup THROWS when the store cannot be read: `None` is "no such user",
   *  and each caller acts on that answer. */
  def findById(id: String): Option[User]

  def findByProviderSub(provider: String, providerSub: String): Option[User]

  def findByEmail(email: String): Option[User]

  def delete(id: String): Unit

  /** Write `user` as the whole row — except `sessionVersion`, which never
   *  moves backwards: the store keeps the higher of its own and `user`'s. A
   *  sign-in writes back a copy it read a moment earlier, and a revoke landing
   *  in between must survive it (see [[revokeSessions]]).
   *
   *  Answers the row as stored by that same step, so a sign-in puts the version
   *  the row now holds in its cookie rather than its stale copy's, which the
   *  revoke already killed. A store that could not write answers `user`. */
  def upsert(user: User): User

  /** "Sign out everywhere": bump `id`'s `sessionVersion` by one in ONE atomic
   *  step and answer the row right after it — `None` when there is no such row
   *  (or the store could not write). Never a read-then-write, which two
   *  requests at once could each bump to the same value. */
  def revokeSessions(id: String): Option[User]

  def close(): Unit
}

/**
 * MongoDB-backed `UserRepository`. Persists to the `users` collection of the
 * database the composition root hands in (`UsersWiring`, on the shared
 * `MongoConnection`). `None` (no Mongo configured, or the connection failed)
 * leaves it a no-op: local dev without Mongo keeps the OAuth flow working with
 * a session-only user that doesn't persist. It never opens a client of its own.
 */
class MongoUserRepository(database: Option[MongoDatabase]) extends UserRepository with Logging {

  private lazy val coll: Option[MongoCollection[User]] = database.map { db =>
    val c = db.withCodecRegistry(UserCodecs.registry).getCollection[User]("users")
    Try(Await.result(c.createIndex(org.mongodb.scala.model.Indexes.ascending("id")).toFuture(), 10.seconds))
    c
  }

  def enabled: Boolean = coll.isDefined

  def findById(id: String): Option[User] = coll.flatMap { c =>
    // A read failure PROPAGATES: `None` means "no such row", and callers act on it —
    // sign the visitor out, rebuild the user as new, confirm a deletion that never ran.
    Await.result(c.find(Filters.eq("id", id)).headOption(), 10.seconds)
  }

  def findByProviderSub(provider: String, providerSub: String): Option[User] = coll.flatMap { c =>
    Await.result(
      c.find(Filters.and(Filters.eq("provider", provider), Filters.eq("providerSub", providerSub)))
       .headOption(),
      10.seconds
    )
  }

  def findByEmail(email: String): Option[User] = coll.flatMap { c =>
    // Case-insensitive match: providers normalise differently
    // (`Alice@Example.com` from one, `alice@example.com` from another)
    // but they're the same person. Mongo regex with the i flag is the
    // path-of-least-resistance — anchored to start + end so we don't
    // match partial substrings.
    val pattern = "^" + java.util.regex.Pattern.quote(email) + "$"
    Await.result(c.find(Filters.regex("email", pattern, "i")).headOption(), 10.seconds)
  }

  def delete(id: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"UserRepository.delete($id) failed: ${exception.getMessage}")
    }
  }

  def upsert(user: User): User = coll.fold(user) { c =>
    Try {
      Option(Await.result(c.findOneAndUpdate(Filters.eq("id", user.id), MongoUserRepository.upsertPipeline(user),
        new FindOneAndUpdateOptions().upsert(true).returnDocument(ReturnDocument.AFTER)).toFuture(), 10.seconds))
        .getOrElse(user)
    }.recover {
      case exception: Throwable =>
        logger.warn(s"UserRepository.upsert(${user.id}) failed: ${exception.getMessage}")
        user
    }.get
  }

  def revokeSessions(id: String): Option[User] = coll.flatMap { c =>
    Try(Option(Await.result(c.findOneAndUpdate(Filters.eq("id", id), Updates.inc("sessionVersion", 1),
      new FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)).toFuture(), 10.seconds)))
      .recover { case exception: Throwable =>
        logger.warn(s"UserRepository.revokeSessions($id) failed: ${exception.getMessage}")
        None
      }.get
  }

  def close(): Unit = ()
}

object MongoUserRepository {

  /** The whole row as `user` has it — a `$replaceWith`, so a `None` field the
   *  codec leaves out is gone afterwards, exactly as a replace would leave it —
   *  but with `sessionVersion` the higher of the stored one and `user`'s (see
   *  `UserRepository.upsert`). The row travels inside `$literal`, since user
   *  text starting with `$` would otherwise read as a field path. */
  private[users] def upsertPipeline(user: User): Seq[Bson] = {
    val row = new BsonDocument()
    UserCodecs.registry.get(classOf[User]).encode(new BsonDocumentWriter(row), user, EncoderContext.builder().build())
    row.remove("sessionVersion")
    import UpdatePipeline.{field, literal, op}
    val stored         = op("$ifNull", field("sessionVersion"), new BsonInt32(0))
    val sessionVersion = new BsonDocument("sessionVersion", op("$max", stored, new BsonInt32(user.sessionVersion)))
    Seq(new BsonDocument("$replaceWith", op("$mergeObjects", literal(row), sessionVersion)))
  }
}

/**
 * In-memory `UserRepository` for tests. Trivial map keyed by `id` plus an
 * index on `(provider, providerSub)` so the lookup signatures both
 * stay O(1). Never persists anything across instances — every spec
 * starts with a fresh empty store.
 */
class InMemoryUserRepository extends UserRepository {
  private val byId  = scala.collection.mutable.Map.empty[String, User]
  private val bySub = scala.collection.mutable.Map.empty[(String, String), String]

  def enabled: Boolean = true

  def findById(id: String): Option[User] = byId.get(id)

  def findByProviderSub(provider: String, providerSub: String): Option[User] =
    bySub.get((provider, providerSub)).flatMap(byId.get)

  def findByEmail(email: String): Option[User] =
    byId.values.find(_.email.exists(_.equalsIgnoreCase(email)))

  def upsert(user: User): User = synchronized {
    val kept = user.copy(sessionVersion = byId.get(user.id).map(_.sessionVersion).fold(user.sessionVersion)(_ max user.sessionVersion))
    // Account linking can change a user's (provider, providerSub) pair —
    // sweep out any stale entries pointing to this id before re-indexing.
    // Mongo's overwrite-on-upsert gives the same effect naturally; the
    // in-memory impl has to do it explicitly.
    bySub.filterInPlace { case (_, id) => id != user.id }
    byId(user.id) = kept
    bySub((user.provider, user.providerSub)) = user.id
    kept
  }

  def revokeSessions(id: String): Option[User] = synchronized {
    byId.get(id).map { stored =>
      val revoked = stored.copy(sessionVersion = stored.sessionVersion + 1)
      byId(id) = revoked
      revoked
    }
  }

  def delete(id: String): Unit = synchronized {
    byId.remove(id).foreach(u => bySub.remove((u.provider, u.providerSub)))
  }

  def close(): Unit = ()
}
