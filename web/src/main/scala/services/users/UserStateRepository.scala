package services.users

import com.mongodb.client.model.ReplaceOptions
import models.UserState
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Per-user state store — hidden films + disabled cinemas + /plan picks
 * (selectedMovies, favouriteRooms), one document per user. Read on every
 * page load for authenticated users (so the rendered page reflects their
 * server-side state, not stale localStorage); written when they hide a
 * film / disable a cinema / change their /plan state.
 *
 * Trait + Mongo impl + in-memory impl mirror `UserRepository`.
 */
trait UserStateRepository {
  def enabled: Boolean

  /** State for `userId`, or `None` when nothing's been persisted yet —
   *  callers treat `None` as `UserState.empty(userId)`. */
  def find(userId: String): Option[UserState]

  /** Full-document replace. Best-effort. */
  def upsert(state: UserState): Unit

  /** Remove this user's state row entirely. Used by the account-deletion
   *  endpoint alongside `UserRepository.delete`. */
  def delete(userId: String): Unit

  def close(): Unit
}

class MongoUserStateRepository(
  sharedDb: Option[MongoDatabase] = None,
  fallbackToOwnInit: Boolean = true
) extends UserStateRepository with Logging {

  // Shares its MongoClient with the rest of the app via the
  // `MongoConnection` passed by Wiring. See MongoUserRepository for the
  // sharedDb / legacy-init dual-path rationale. `fallbackToOwnInit`
  // exists for the same reason as on MongoMovieRepository: production sets
  // it false so a failed shared connection doesn't trigger a duplicate
  // 15s init timeout here.
  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[UserState]]) =
    sharedDb match {
      case Some(db) =>
        val coll = db.withCodecRegistry(UserCodecs.registry).getCollection[UserState]("userStates")
        scala.util.Try(scala.concurrent.Await.result(coll.createIndex(org.mongodb.scala.model.Indexes.ascending("userId")).toFuture(), 10.seconds))
        (None, Some(coll))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None)
    }
  private def clientOpt: Option[MongoClient]                = initResult._1
  private def coll:      Option[MongoCollection[UserState]] = initResult._2

  def enabled: Boolean = coll.isDefined

  def find(userId: String): Option[UserState] = coll.flatMap { c =>
    Try {
      Await.result(c.find(Filters.eq("userId", userId)).headOption(), 10.seconds)
    }.recover {
      case exception: Throwable =>
        logger.warn(s"UserStateRepository.find($userId) failed: ${exception.getMessage}")
        None
    }.getOrElse(None)
  }

  def upsert(state: UserState): Unit = coll.foreach { c =>
    val opts = new ReplaceOptions().upsert(true)
    Try {
      Await.result(c.replaceOne(Filters.eq("userId", state.userId), state, opts).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable =>
        logger.warn(s"UserStateRepository.upsert(${state.userId}) failed: ${exception.getMessage}")
    }
  }

  def delete(userId: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("userId", userId)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"UserStateRepository.delete($userId) failed: ${exception.getMessage}")
    }
  }

  def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[UserState]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoUserStateRepository disabled.")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val db     = client.getDatabase(dbName).withCodecRegistry(UserCodecs.registry)
          val coll   = db.getCollection[UserState]("userStates")
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          Await.result(coll.createIndex(org.mongodb.scala.model.Indexes.ascending("userId")).toFuture(), 10.seconds)
          logger.info(s"MongoUserStateRepository connected to $dbName.userStates")
          (client, coll)
        }.recover {
          case exception: Throwable =>
            logger.error(s"MongoUserStateRepository init failed (${exception.getMessage}) — disabled.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, coll)) => (Some(c), Some(coll))
          case None            => (None, None)
        }
    }
}

class InMemoryUserStateRepository extends UserStateRepository {
  private val store = scala.collection.mutable.Map.empty[String, UserState]

  def enabled: Boolean = true

  def find(userId: String): Option[UserState] = store.get(userId)

  def upsert(state: UserState): Unit = { store(state.userId) = state }

  def delete(userId: String): Unit = { store.remove(userId); () }

  def close(): Unit = ()
}
