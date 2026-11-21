package services.users

import java.time.Instant

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument}
import com.mongodb.{ErrorCategory, MongoServerException}
import models.UserState
import org.bson.BsonDocument
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging

/**
 * Per-user state store — per-country hidden films and the language pick,
 * one document per user. Read by `/api/me/state` and
 * `/api/me/:country/hidden-films`; written when the user hides or unhides a
 * film, or picks a language. The global `hiddenFilms` / `disabledCinemas`
 * sets are no longer written by anything — see `UserState`.
 *
 * Trait + Mongo impl + in-memory impl mirror `UserRepository`.
 */
trait UserStateRepository {
  def enabled: Boolean

  /** State for `userId`, or `None` when nothing's been persisted yet — callers
   *  treat `None` as `UserState.empty(userId, now)`. THROWS when the store cannot be
   *  read — never `None`, which the controller would serve as an empty state. */
  def find(userId: String): Option[UserState]

  /** Set `userId`'s language pick (`None` clears it) in ONE atomic step —
   *  creating the row if there is none — stamp `updatedAt` per
   *  `UserState.nextUpdatedAt` from `now`, and return the row as it stands
   *  right after. Nothing else on the row is touched, so a hide landing at the
   *  same moment survives it.
   *
   *  `None` only when the store could not perform the write at all. */
  def setLanguage(userId: String, language: Option[String], now: Instant): Option[UserState]

  /** Apply `change` to `userId`'s `country` bucket in ONE atomic step —
   *  creating the row if there is none — and return the row as it stands
   *  right after. Unless `change` declines (see [[HiddenFilmsChange.applyTo]]),
   *  it also stamps `updatedAt` per `UserState.nextUpdatedAt` from `now`, so
   *  every applied write moves the version even within one millisecond.
   *  Nothing else on the row is touched, so overlapping writes for one user
   *  never erase each other.
   *
   *  `None` only when the store could not perform the write at all (disabled,
   *  or failed) — not for a declined change, which returns the row unchanged. */
  def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState]

  /** Remove this user's state row entirely. Used by the account-deletion
   *  endpoint alongside `UserRepository.delete`. */
  def delete(userId: String): Unit

  def close(): Unit
}

object UserStateRepository {
  val Collection = "userStates"
}

/** The `userStates` store over Mongo: its reads and atomic writes (each one `findOneAndUpdate`, the update a
 *  server-side pipeline: [[UserStatesCollection]]). */
class MongoUserStateRepository(
  // The users database, handed in by the composition root (`UsersWiring`, on the
  // shared `MongoConnection`); `None` (no Mongo configured, or the connection
  // failed) leaves the store disabled. It never opens a client of its own.
  database: Option[MongoDatabase],
  // How each atomic write went, one outcome per write — the web's
  // `UserStateWriteMetrics` in production.
  writeOutcomes: UserStateWriteOutcomes = UserStateWriteOutcomes.none,
  // Whether the unique `userId` index is in place — the web's `UserStateIndexMetrics`.
  indexHealth: UserStateIndexHealth = UserStateIndexHealth.none
) extends UserStateRepository with Logging {
  import UserStateWriteOutcomes.{Endpoint, Outcome}

  private lazy val coll: Option[MongoCollection[UserState]] = database.map(UserStatesCollection.open(_, indexHealth))

  def enabled: Boolean = coll.isDefined

  def find(userId: String): Option[UserState] = coll.flatMap { c =>
    // A read failure PROPAGATES: `None` means "no state yet", which the controller would
    // serve as an empty hidden-films list with a fresh validator.
    Await.result(c.find(Filters.eq("userId", userId)).headOption(), 10.seconds)
  }

  def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] =
    atomically(userId, s"changeHiddenFilms($userId, $country)", Endpoint.of(change),
      UserStatesCollection.hiddenFilmsPipeline(country, change, now))

  def setLanguage(userId: String, language: Option[String], now: Instant): Option[UserState] =
    atomically(userId, s"setLanguage($userId)", Endpoint.Language, UserStatesCollection.languagePipeline(language, now))

  /** Run `update` against `userId`'s row as one `findOneAndUpdate` — upserting
   *  when there is no row — and answer with the row right after it, reporting
   *  how it went to `writeOutcomes` under `endpoint`. */
  private def atomically(userId: String, what: String, endpoint: String, update: Seq[BsonDocument]): Option[UserState] =
    coll match {
      case None =>
        writeOutcomes.record(endpoint, Outcome.Unavailable)
        None
      case Some(c) =>
        val options = new FindOneAndUpdateOptions().upsert(true).returnDocument(ReturnDocument.AFTER)
        def attempt(): UserState =
          tools.MongoWriteErrors.counted(c)(Await.result(c.findOneAndUpdate(Filters.eq("userId", userId), update, options).toFuture(), 10.seconds))
        val written = Try(attempt() -> Outcome.Ok).recover {
          // Two first-ever writes for one user both upserting: the unique `userId`
          // index turns the loser's insert away. The row exists now, so the retry
          // updates it instead.
          case duplicate: MongoServerException if ErrorCategory.fromErrorCode(duplicate.getCode) == ErrorCategory.DUPLICATE_KEY =>
            attempt() -> Outcome.Conflict
        }
        written.failed.foreach(exception => logger.warn(s"UserStateRepository.$what failed: ${exception.getMessage}"))
        writeOutcomes.record(endpoint, written.fold(_ => Outcome.StoreFailure, _._2))
        written.toOption.flatMap { case (state, _) => Option(state) }
    }

  def delete(userId: String): Unit = coll.foreach { c =>
    Try {
      tools.MongoWriteErrors.counted(c)(Await.result(c.deleteOne(Filters.eq("userId", userId)).toFuture(), 10.seconds))
      ()
    }.recover {
      case exception => logger.warn(s"UserStateRepository.delete($userId) failed: ${exception.getMessage}")
    }
  }

  def close(): Unit = ()
}

/** The states as `userStates` would hold them: each written through its codec to BSON and read back. */
class InMemoryUserStateRepository extends UserStateRepository {
  private val store = scala.collection.mutable.Map.empty[String, UserState]
  private val stored = tools.BsonRoundTrip.throughCodec(UserCodecs.registry.get(classOf[UserState]))
  private def put(state: UserState): UserState = { val kept = stored(state); store(kept.userId) = kept; kept }

  def enabled: Boolean = true

  def find(userId: String): Option[UserState] = synchronized(store.get(userId))

  /** Whole-row replace, for SEEDING a spec — deliberately not on the trait: production
   *  writes only through the atomic field-scoped pipelines. The Mongo-side equivalent
   *  is the test-support `UserStateRows.replace`. */
  def upsert(state: UserState): Unit = synchronized { put(state); () }

  def setLanguage(userId: String, language: Option[String], now: Instant): Option[UserState] = {
    val after = synchronized {
      val current = store.get(userId)
      put(current.getOrElse(UserState.empty(userId, now))
        .copy(language = language, updatedAt = UserState.nextUpdatedAt(current.map(_.updatedAt), now)))
    }
    Some(after)
  }

  def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] = {
    val after = synchronized {
      val current = store.get(userId)
      val base    = current.getOrElse(UserState.empty(userId, now))
      change.applyTo(base.hiddenFilmsByCountry.getOrElse(country, Set.empty)) match {
        case None         => base
        case Some(bucket) =>
          put(base.copy(
            hiddenFilmsByCountry = base.hiddenFilmsByCountry.updated(country, bucket),
            updatedAt            = UserState.nextUpdatedAt(current.map(_.updatedAt), now)))
      }
    }
    Some(after)
  }

  def delete(userId: String): Unit = synchronized { store.remove(userId); () }

  def close(): Unit = ()
}
