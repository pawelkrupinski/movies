package services.users

import com.mongodb.client.model.ReplaceOptions
import models.User
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Persistent store for authenticated users. Identity is the user's
 * email (lowercased) — logging in with Google, Facebook, or Apple
 * using the same email accesses the same account and state.
 *
 * The trait is what callers (AuthController, UserStateController) see;
 * `MongoUserRepo` is the production impl, `InMemoryUserRepo` the test
 * fake. Pattern mirrors `MovieRepo`.
 */
trait UserRepo {
  def enabled: Boolean

  /** Look up by id (= lowercased email). Used on every authenticated
   *  request after session decode, and during OAuth callback. */
  def findById(id: String): Option[User]

  def findByProviderSub(provider: String, providerSub: String): Option[User]

  def findByEmail(email: String): Option[User]

  def delete(id: String): Unit

  def upsert(user: User): Unit

  def close(): Unit
}

/**
 * MongoDB-backed `UserRepo`. Persists to the `users` collection.
 *
 * When `MONGODB_URI` is unset the repo silently no-ops — local dev
 * without Atlas keeps working with the OAuth flow returning a
 * "session-only" user that doesn't persist (Phase B will surface
 * that explicitly).
 */
class MongoUserRepo(sharedDb: Option[MongoDatabase] = None) extends UserRepo with Logging {

  // When `sharedDb` is provided (production path via `MongoConnection`),
  // we apply our codec registry to it and grab the `users` collection
  // without opening our own client. When `None` (legacy scripts under
  // test/scala/scripts/, IT specs), we build our own MongoClient.
  // See `MongoConnection` for the rationale — Phase A originally
  // opened a third independent MongoClient per process, which is what
  // tipped RSS past the 512 MB Fly ceiling.
  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[User]]) =
    sharedDb match {
      case Some(db) =>
        val coll = db.withCodecRegistry(UserCodecs.registry).getCollection[User]("users")
        Try(Await.result(coll.createIndex(org.mongodb.scala.model.Indexes.ascending("id")).toFuture(), 10.seconds))
        (None, Some(coll))
      case None => init()
    }
  private def clientOpt: Option[MongoClient]               = initResult._1
  private def coll:      Option[MongoCollection[User]]     = initResult._2

  def enabled: Boolean = coll.isDefined

  def findById(id: String): Option[User] = coll.flatMap { c =>
    Try {
      Await.result(c.find(Filters.eq("id", id)).headOption(), 10.seconds)
    }.recover {
      case ex: Throwable =>
        logger.warn(s"UserRepo.findById($id) failed: ${ex.getMessage}")
        None
    }.getOrElse(None)
  }

  def findByProviderSub(provider: String, providerSub: String): Option[User] = coll.flatMap { c =>
    Try {
      Await.result(
        c.find(Filters.and(Filters.eq("provider", provider), Filters.eq("providerSub", providerSub)))
         .headOption(),
        10.seconds
      )
    }.recover {
      case ex: Throwable =>
        logger.warn(s"UserRepo.findByProviderSub($provider, …) failed: ${ex.getMessage}")
        None
    }.getOrElse(None)
  }

  def findByEmail(email: String): Option[User] = coll.flatMap { c =>
    // Case-insensitive match: providers normalise differently
    // (`Alice@Example.com` from one, `alice@example.com` from another)
    // but they're the same person. Mongo regex with the i flag is the
    // path-of-least-resistance — anchored to start + end so we don't
    // match partial substrings.
    val pattern = "^" + java.util.regex.Pattern.quote(email) + "$"
    Try {
      Await.result(c.find(Filters.regex("email", pattern, "i")).headOption(), 10.seconds)
    }.recover {
      case ex: Throwable =>
        logger.warn(s"UserRepo.findByEmail(…) failed: ${ex.getMessage}")
        None
    }.getOrElse(None)
  }

  def delete(id: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"UserRepo.delete($id) failed: ${ex.getMessage}")
    }
  }

  def upsert(user: User): Unit = coll.foreach { c =>
    val opts = new ReplaceOptions().upsert(true)
    Try {
      Await.result(c.replaceOne(Filters.eq("id", user.id), user, opts).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable =>
        logger.warn(s"UserRepo.upsert(${user.id}) failed: ${ex.getMessage}")
    }
  }

  def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[User]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoUserRepo disabled.")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val db     = client.getDatabase(dbName).withCodecRegistry(UserCodecs.registry)
          val coll   = db.getCollection[User]("users")
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          Await.result(coll.createIndex(org.mongodb.scala.model.Indexes.ascending("id")).toFuture(), 10.seconds)
          logger.info(s"MongoUserRepo connected to $dbName.users")
          (client, coll)
        }.recover {
          case ex: Throwable =>
            logger.error(s"MongoUserRepo init failed (${ex.getMessage}) — disabled.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, coll)) => (Some(c), Some(coll))
          case None            => (None, None)
        }
    }
}

/**
 * In-memory `UserRepo` for tests. Trivial map keyed by `id` plus an
 * index on `(provider, providerSub)` so the lookup signatures both
 * stay O(1). Never persists anything across instances — every spec
 * starts with a fresh empty store.
 */
class InMemoryUserRepo extends UserRepo {
  private val byId  = scala.collection.mutable.Map.empty[String, User]
  private val bySub = scala.collection.mutable.Map.empty[(String, String), String]

  def enabled: Boolean = true

  def findById(id: String): Option[User] = byId.get(id)

  def findByProviderSub(provider: String, providerSub: String): Option[User] =
    bySub.get((provider, providerSub)).flatMap(byId.get)

  def findByEmail(email: String): Option[User] =
    byId.values.find(_.email.exists(_.equalsIgnoreCase(email)))

  def upsert(user: User): Unit = {
    // Account linking can change a user's (provider, providerSub) pair —
    // sweep out any stale entries pointing to this id before re-indexing.
    // Mongo's overwrite-on-upsert gives the same effect naturally; the
    // in-memory impl has to do it explicitly.
    bySub.filterInPlace { case (_, id) => id != user.id }
    byId(user.id) = user
    bySub((user.provider, user.providerSub)) = user.id
  }

  def delete(id: String): Unit = {
    byId.remove(id).foreach(u => bySub.remove((u.provider, u.providerSub)))
  }

  def close(): Unit = ()
}
