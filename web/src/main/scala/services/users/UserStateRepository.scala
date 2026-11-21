package services.users

import java.time.Instant

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument}
import com.mongodb.{ErrorCategory, MongoServerException}
import models.UserState
import org.bson.{BsonArray, BsonDateTime, BsonDocument, BsonInt32, BsonString, BsonValue}
import org.mongodb.scala.model.{Aggregates, Field, Filters}
import org.mongodb.scala.{MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.{Logger, Logging}

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
   *  endpoint alongside `UserRepository.delete`; THROWS when the store cannot
   *  perform it (see [[AccountDeletion]]). */
  def delete(userId: String): Unit

  def close(): Unit
}

object UserStateRepository {
  val Collection = "userStates"
}

/** The `userStates` store over Mongo: its reads and atomic writes, each one `findOneAndUpdate` whose update is a
 *  server-side pipeline (the companion: how `userStates` is laid out, its unique `userId` index and those pipelines). */
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

  private lazy val coll: Option[MongoCollection[UserState]] = database.map(MongoUserStateRepository.open(_, indexHealth))

  def enabled: Boolean = coll.isDefined

  def find(userId: String): Option[UserState] = coll.flatMap { c =>
    // A read failure PROPAGATES: `None` means "no state yet", which the controller would
    // serve as an empty hidden-films list with a fresh validator.
    Await.result(c.find(Filters.eq("userId", userId)).headOption(), 10.seconds)
  }

  def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] =
    atomically(userId, s"changeHiddenFilms($userId, $country)", Endpoint.of(change),
      MongoUserStateRepository.hiddenFilmsPipeline(country, change, now))

  def setLanguage(userId: String, language: Option[String], now: Instant): Option[UserState] =
    atomically(userId, s"setLanguage($userId)", Endpoint.Language, MongoUserStateRepository.languagePipeline(language, now))

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
    tools.MongoWriteErrors.counted(c)(Await.result(c.deleteOne(Filters.eq("userId", userId)).toFuture(), 10.seconds))
    ()
  }

  def close(): Unit = ()
}

object MongoUserStateRepository {
  private val logger = Logger(classOf[MongoUserStateRepository])

  /** The collection through the users' codecs, its write-error series seeded and its unique `userId` index ensured
   *  (reported to `indexHealth`). */
  private def open(db: MongoDatabase, indexHealth: UserStateIndexHealth): MongoCollection[UserState] = {
    val c = db.withCodecRegistry(UserCodecs.registry).getCollection[UserState](UserStateRepository.Collection)
    // The web's only write path: seeded here, at boot, or a country nobody has signed in to publishes no write-error
    // series at all, which AppMetricAbsent reads as the counter unwired.
    tools.MongoWriteErrors.seed(c)
    uniqueUserIdIndexOrReport(db, indexHealth)
    c
  }

  // `unique` — without it an upserting write keyed on `Filters.eq("userId", …)`
  // can't tell "this user's one row" from "the first of several": a plain
  // (non-unique) index let 4 duplicate rows for one userId accumulate from a
  // historic write race (found + cleaned up 2026-09-20), after which `find`
  // and a write could silently disagree on WHICH duplicate is "the" row.
  //
  // BUILD OR CONVERT, NEVER DROP. Every web pod runs this on every boot, and a
  // rolling deploy boots the new pod while the old one writes. This used to drop a
  // legacy plain `userId_1` to make room for the unique one, and two pods
  // booting together both read "legacy" and both dropped it: the second drop
  // either found nothing (that pod reported the index missing although the
  // first had built it) or removed the unique index the first had just built
  // (`UserStateAcrossPodsIntegrationSpec`). `MongoIndex` converts a plain index
  // to unique IN PLACE instead — two `collMod`s that are no-ops once applied, so
  // two pods converting together both succeed and the index is never absent.
  //
  // LOUD, NOT FATAL. The store keeps serving without the index — failing the boot
  // would take every web pod (the whole read tier) down over a write-correctness
  // risk to a few signed-in visitors' rows — but an index not in place (duplicate
  // rows, a credential without `collMod`) is logged at ERROR and reported to
  // `indexHealth`, whose gauge `UserStateUniqueIndexMissing` (web-errors.rules)
  // alerts on.
  private def uniqueUserIdIndexOrReport(db: MongoDatabase, indexHealth: UserStateIndexHealth): Unit = {
    val outcome = tools.MongoIndex.ensure(db, UserStateRepository.Collection,
      org.mongodb.scala.model.Indexes.ascending("userId"), new org.mongodb.scala.model.IndexOptions().unique(true), "userStates")
    outcome match {
      case tools.MongoIndex.Outcome.NotInPlace(reason) =>
        logger.error(s"userStates has NO unique userId index ($reason). Writes keyed on userId may pick either of two rows.")
      case _ => ()
    }
    indexHealth.uniqueUserIdIndex(outcome.inPlace)
  }

  import tools.MongoExpressions.{field, literal, op}

  // Both pipelines below are single-stage `$set`s: the server evaluates every
  // field against the row it is writing — nothing read beforehand, nothing to
  // go stale. User text (titles, cinema names) always travels inside
  // `$literal`, since a string starting with `$` would otherwise read as a
  // field path.

  private def strings(values: Iterable[String]): BsonArray = new BsonArray(values.toList.map(v => new BsonString(v): BsonValue).asJava)
  private def orEmpty(path: String): BsonDocument = op("$ifNull", field(path), new BsonArray())

  /** `UserState.nextUpdatedAt`: now, or a millisecond past the stored stamp. */
  private def stamped(now: Instant): BsonDocument =
    op("$max", new BsonDateTime(now.toEpochMilli), op("$add", field("updatedAt"), new BsonInt32(1)))

  /** A legacy global set, nothing writes any more: the stored one, or empty on
   *  the upsert's insert, since the codec needs both sets on every row. */
  private def legacySet(name: String): Field[BsonValue] = Field(name, orEmpty(name))

  /** [[HiddenFilmsChange.applyTo]] plus the `updatedAt` stamp — neither applied
   *  when the change declines. */
  private[users] def hiddenFilmsPipeline(country: String, change: HiddenFilmsChange, now: Instant): Seq[BsonDocument] = {
    // Country codes come from `models.Country` — but this becomes a field path.
    require(country.matches("[a-z]{2,3}"), s"not a country code: '$country'")
    val path   = s"hiddenFilmsByCountry.$country"
    val bucket = orEmpty(path)
    val always = org.bson.BsonBoolean.TRUE
    val (next, applies) = change match {
      case HiddenFilmsChange.Hide(title, limit) =>
        op("$setUnion", bucket, literal(strings(Seq(title)))) ->
          op("$or", op("$in", literal(new BsonString(title)), bucket), op("$lt", op("$size", bucket), new BsonInt32(limit)))
      case HiddenFilmsChange.Unhide(title) => op("$setDifference", bucket, literal(strings(Seq(title)))) -> always
      case HiddenFilmsChange.Clear         => literal(new BsonArray()) -> always
    }
    val unstamped = op("$ifNull", field("updatedAt"), new BsonDateTime(now.toEpochMilli))
    Seq(Aggregates.set(
      Field[BsonValue](path,        op("$cond", applies, next, field(path))),
      Field[BsonValue]("updatedAt", op("$cond", applies, stamped(now), unstamped)),
      legacySet("hiddenFilms"),
      legacySet("disabledCinemas")
    ).toBsonDocument)
  }

  /** The language pick plus the `updatedAt` stamp. A cleared language is
   *  `$$REMOVE`d — the codec reads an absent field as `None`. */
  private[users] def languagePipeline(language: Option[String], now: Instant): Seq[BsonDocument] =
    Seq(Aggregates.set(
      Field[BsonValue]("language", language.fold[BsonValue](new BsonString("$$REMOVE"))(l => literal(new BsonString(l)))),
      Field[BsonValue]("updatedAt", stamped(now)),
      legacySet("hiddenFilms"),
      legacySet("disabledCinemas")
    ).toBsonDocument)
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
