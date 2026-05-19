package services.users

import com.mongodb.client.model.ReplaceOptions
import models.User
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoCollection, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Persistent store for authenticated users — one document per
 * `(provider, providerSub)`. Lookups happen on every authenticated
 * request (session cookie carries `userId`, repo dereferences).
 *
 * The trait is what callers (AuthController, UserStateController) see;
 * `MongoUserRepo` is the production impl, `InMemoryUserRepo` the test
 * fake. Pattern mirrors `MovieRepo`.
 */
trait UserRepo {
  def enabled: Boolean

  /** Look up by our own UUID (used on every request after session decode). */
  def findById(id: String): Option[User]

  /** Look up by upstream identity — used in the OAuth callback to decide
   *  "is this a returning user or a fresh signup?". */
  def findByProviderSub(provider: String, providerSub: String): Option[User]

  /** Upsert by `id`. Used both for first-login creation and for
   *  `lastSeenAt` bumps. Best-effort — failures are logged, never
   *  thrown; the caller continues with the user object it already has
   *  in hand. */
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
class MongoUserRepo extends UserRepo with Logging {

  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[User]]) = init()
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
  private val byId    = scala.collection.mutable.Map.empty[String, User]
  private val bySub   = scala.collection.mutable.Map.empty[(String, String), String]

  def enabled: Boolean = true

  def findById(id: String): Option[User] = byId.get(id)

  def findByProviderSub(provider: String, providerSub: String): Option[User] =
    bySub.get((provider, providerSub)).flatMap(byId.get)

  def upsert(user: User): Unit = {
    byId(user.id) = user
    bySub((user.provider, user.providerSub)) = user.id
  }

  def close(): Unit = ()
}
