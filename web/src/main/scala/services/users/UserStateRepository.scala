package services.users

import com.mongodb.{ErrorCategory, MongoServerException}
import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.UserState
import org.bson.{BsonArray, BsonDateTime, BsonDocument, BsonInt32, BsonString, BsonValue}
import org.mongodb.scala.model.{Aggregates, Field, Filters}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import play.api.Logging
import services.movies.{ChangeStreamLiveness, ChangeStreamReopen}
import tools.Env

import scala.concurrent.Await
import java.time.Instant
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Per-user state store — hidden films + disabled cinemas + /plan picks
 * one document per user. Read on every
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

  /** Set the fields a legacy `PUT /api/me/state` body carried (see
   *  [[LegacyStatePatch]]) in ONE atomic step — creating the row if there is
   *  none — stamp `updatedAt` per `UserState.nextUpdatedAt` from `now`, and
   *  return the row as it stands right after. Fields the patch leaves out, and
   *  `hiddenFilmsByCountry` always, are untouched — so a hide landing at the
   *  same moment survives it (the whole-row read-modify-write this replaced
   *  had to detect and retry that race, and 503'd once it lost five in a row).
   *
   *  `None` only when the store could not perform the write at all. */
  def patchLegacyState(userId: String, patch: LegacyStatePatch, now: Instant): Option[UserState]

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

  /** Stream out-of-band writes/deletes to `userStates` as they happen — see
   *  `MovieRepository.watchChanges`, which this mirrors, with one addition:
   *  `onDisconnect` fires once if the underlying cursor dies before a new
   *  registration replaces it — and whenever the stream sees a change it cannot
   *  attribute to a user (the Mongo store's deletes: see `MongoUserStateRepository`). `MovieCache`'s consumers don't need that
   *  signal — they tolerate staleness via a periodic rehydrate backstop.
   *  `UserChangeTimeCache` (the one caller today) gates an HTTP freshness
   *  *decision*, so it must know the moment its view stops being current
   *  rather than keep answering from what it had.
   *
   *  Unlike `MovieRepository`, there's no fan-out to multiple listeners —
   *  only one consumer watches `userStates`, so that machinery isn't earned
   *  yet (add it if a second one shows up). A second `watchChanges` call
   *  replaces the first registration rather than adding to it.
   *
   *  Default: not supported (returns `None`), same as `MovieRepository`. */
  def watchChanges(
    onUpsert:     UserState => Unit,
    onDelete:     String => Unit,
    onDisconnect: () => Unit
  ): Option[AutoCloseable] = None

  /** When the `userStates` cursor last delivered an event — see
   *  `ChangeStreamLiveness`. Default: a repository with no stream, whose
   *  cursor ages from creation and is never stamped. */
  def changeStreamLiveness: ChangeStreamLiveness = UserStateRepository.unwatchedLiveness

  def close(): Unit
}

object UserStateRepository {
  val Collection = "userStates"
  private[users] lazy val unwatchedLiveness: ChangeStreamLiveness = ChangeStreamLiveness.unwatched()
}

class MongoUserStateRepository(
  sharedDb: Option[MongoDatabase] = None,
  fallbackToOwnInit: Boolean = true,
  // The reopen driver for the `userStates` cursor. Production schedules on a daemon
  // thread; a spec hands over one that fires when it says so.
  reopenDriver: (String, () => Unit) => ChangeStreamReopen = ChangeStreamReopen.onDaemonScheduler,
  // How each atomic write went, one outcome per write — the web's
  // `UserStateWriteMetrics` in production.
  writeOutcomes: UserStateWriteOutcomes = UserStateWriteOutcomes.none
) extends UserStateRepository with Logging {
  import UserStateWriteOutcomes.{Endpoint, Outcome}

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
        scala.util.Try(ensureUniqueUserIdIndex(coll))
        (None, Some(coll))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None)
    }

  // `unique` — without it an upserting write keyed on `Filters.eq("userId", …)`
  // can't tell "this user's one row" from "the first of several": a plain
  // (non-unique) index let 4 duplicate rows for one userId accumulate from a
  // historic write race (found + cleaned up 2026-09-20), after which `find`
  // and a write could silently disagree on WHICH duplicate is "the" row.
  //
  // A deployment that already has the OLD plain index can't just add
  // `unique` to it in place — Mongo rejects a `createIndex` whose auto-generated
  // name ("userId_1") already exists with different options
  // (`IndexKeySpecsConflict`). Drop the old one by name first — but ONLY when
  // it exists and isn't unique yet. Every web pod runs this on every boot:
  // an unconditional drop rebuilt the index each time and, between the drop
  // and the create, left the collection with no index (and no uniqueness)
  // at all.
  private def ensureUniqueUserIdIndex(coll: MongoCollection[UserState]): Unit = {
    val legacyPlainIndex = Await.result(coll.listIndexes[org.bson.BsonDocument]().toFuture(), 10.seconds)
      .exists(ix => ix.getString("name").getValue == "userId_1" && !ix.getBoolean("unique", org.bson.BsonBoolean.FALSE).getValue)
    if (legacyPlainIndex) Await.result(coll.dropIndex("userId_1").toFuture(), 10.seconds)
    Await.result(
      coll.createIndex(
        org.mongodb.scala.model.Indexes.ascending("userId"),
        new org.mongodb.scala.model.IndexOptions().unique(true)
      ).toFuture(), 10.seconds)
    ()
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

  def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] =
    atomically(userId, s"changeHiddenFilms($userId, $country)", Endpoint.of(change),
      MongoUserStateRepository.hiddenFilmsPipeline(country, change, now))

  def patchLegacyState(userId: String, patch: LegacyStatePatch, now: Instant): Option[UserState] =
    atomically(userId, s"patchLegacyState($userId)", Endpoint.LegacyPut, MongoUserStateRepository.legacyStatePipeline(patch, now))

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
          Await.result(c.findOneAndUpdate(Filters.eq("userId", userId), update, options).toFuture(), 10.seconds)
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
      Await.result(c.deleteOne(Filters.eq("userId", userId)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"UserStateRepository.delete($userId) failed: ${exception.getMessage}")
    }
  }

  private val liveness = new ChangeStreamLiveness()
  // Written by the caller's thread, read on the driver's: volatile so a registration (or
  // its close) is seen by the next event rather than whenever the cache line happens to move.
  @volatile private var listener: Option[(UserState => Unit, String => Unit, () => Unit)] = None
  @volatile private var subscription: Option[Subscription] = None
  private lazy val reopen: ChangeStreamReopen = reopenDriver("userStates", () => subscribe(reopened = true))

  override def changeStreamLiveness: ChangeStreamLiveness = liveness

  /** Replaces any existing registration (see the trait doc — one consumer at a
   *  time). Opens at "now": unlike `MovieChangeStream` this cache has nothing
   *  durable to resume — a missed event just means that one user's cache entry
   *  stays a miss until their next write, which is harmless (the caller reads
   *  storage directly on a miss). */
  override def watchChanges(
    onUpsert:     UserState => Unit,
    onDelete:     String => Unit,
    onDisconnect: () => Unit
  ): Option[AutoCloseable] = coll.map { _ =>
    listener = Some((onUpsert, onDelete, onDisconnect))
    subscribe(reopened = false)
    new AutoCloseable {
      override def close(): Unit = {
        reopen.close()
        subscription.foreach(_.unsubscribe())
        subscription = None
        listener = None
      }
    }
  }

  /** `reopened`: this open replaces a cursor that DIED. It opens at "now", so every write
   *  between the death and this open was never delivered — and the death's `onDisconnect`
   *  only cleared what was cached THEN, not what callers cached during the gap. So a reopen
   *  reports losing track again, once the new cursor is live. */
  private def subscribe(reopened: Boolean): Unit = coll.foreach { c =>
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP).subscribe(new Observer[ChangeStreamDocument[UserState]] {
      override def onSubscribe(s: Subscription): Unit = {
        subscription = Some(s)
        s.request(Long.MaxValue)
        liveness.watching(UserStateRepository.Collection)
        if (reopened) listener.foreach { case (_, _, onLostTrack) => onLostTrack() }
      }
      override def onNext(change: ChangeStreamDocument[UserState]): Unit = {
        reopen.opened()
        liveness.delivered(UserStateRepository.Collection)
        (Option(change.getFullDocument), listener) match {
          case (Some(state), Some((onUpsert, _, _))) => onUpsert(state)
          // A delete carries only its documentKey — and a row's `_id` is a driver-generated
          // ObjectId, not the userId, so the key cannot say WHOSE row went (and there is no
          // pre-image to ask). Deletes are rare (account deletion), so rather than enable
          // collection pre-images, report "can no longer vouch for what changed" — the same
          // signal as a dead cursor, which makes the change-time cache drop everything.
          case (None, Some((_, onDelete, onLostTrack))) =>
            Option(change.getDocumentKey).flatMap(k => Option(k.get("userId")))
              .fold(onLostTrack())(v => onDelete(if (v.isString) v.asString.getValue else v.toString))
          case _ => ()
        }
      }
      override def onError(e: Throwable): Unit = {
        logger.warn(s"UserStateRepository change stream ended (${e.getMessage}) — invalidating the cache; a reopen resumes it.")
        subscription = None
        listener.foreach { case (_, _, onDisconnect) => onDisconnect() }
        reopen.failed()
      }
      override def onComplete(): Unit = {
        subscription = None
        listener.foreach { case (_, _, onDisconnect) => onDisconnect() }
        reopen.failed()
      }
    })
  }

  def close(): Unit = { reopen.close(); clientOpt.foreach(_.close()) }

  private def init(): (Option[MongoClient], Option[MongoCollection[UserState]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoUserStateRepository disabled.")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = models.Country.resolvedDbName
          val client = MongoClient(uri)
          val db     = client.getDatabase(dbName).withCodecRegistry(UserCodecs.registry)
          val coll   = db.getCollection[UserState]("userStates")
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          ensureUniqueUserIdIndex(coll)
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

object MongoUserStateRepository {

  // Both pipelines below are single-stage `$set`s: the server evaluates every
  // field against the row it is writing — nothing read beforehand, nothing to
  // go stale. User text (titles, cinema names) always travels inside
  // `$literal`, since a string starting with `$` would otherwise read as a
  // field path.

  private def op(name: String, args: BsonValue*): BsonDocument = new BsonDocument(name, new BsonArray(args.toList.asJava))
  private def literal(value: BsonValue): BsonDocument = new BsonDocument("$literal", value)
  private def strings(values: Iterable[String]): BsonArray = new BsonArray(values.toList.map(v => new BsonString(v): BsonValue).asJava)
  private def field(path: String): BsonString = new BsonString("$" + path)
  private def orEmpty(path: String): BsonDocument = op("$ifNull", field(path), new BsonArray())

  /** `UserState.nextUpdatedAt`: now, or a millisecond past the stored stamp. */
  private def stamped(now: Instant): BsonDocument =
    op("$max", new BsonDateTime(now.toEpochMilli), op("$add", field("updatedAt"), new BsonInt32(1)))

  /** A legacy set: `replacement` when given, else the stored one — or empty
   *  on the upsert's insert, since the codec needs both sets on every row. */
  private def legacySet(name: String, replacement: Option[Set[String]] = None): Field[BsonValue] =
    Field(name, replacement.fold[BsonValue](orEmpty(name))(v => literal(strings(v))))

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

  /** [[LegacyStatePatch.applyTo]] plus the `updatedAt` stamp. A cleared
   *  language is `$$REMOVE`d — the codec reads an absent field as `None`. */
  private[users] def legacyStatePipeline(patch: LegacyStatePatch, now: Instant): Seq[BsonDocument] = {
    val language = patch.language.map(pick =>
      Field[BsonValue]("language", pick.fold[BsonValue](new BsonString("$$REMOVE"))(l => literal(new BsonString(l)))))
    val fields = Seq(legacySet("hiddenFilms", patch.hiddenFilms), legacySet("disabledCinemas", patch.disabledCinemas)) ++
      language :+ Field[BsonValue]("updatedAt", stamped(now))
    Seq(Aggregates.set(fields*).toBsonDocument)
  }
}

class InMemoryUserStateRepository extends UserStateRepository {
  private val store = scala.collection.mutable.Map.empty[String, UserState]
  private val liveness = new ChangeStreamLiveness()
  private var listener: Option[(UserState => Unit, String => Unit, () => Unit)] = None

  def enabled: Boolean = true

  def find(userId: String): Option[UserState] = synchronized(store.get(userId))

  /** Whole-row replace, for SEEDING a spec — deliberately not on the trait: production
   *  writes only through the atomic field-scoped pipelines. The Mongo-side equivalent
   *  is the test-support `UserStateRows.replace`. */
  def upsert(state: UserState): Unit = {
    synchronized(store(state.userId) = state)
    published(state)
  }

  def patchLegacyState(userId: String, patch: LegacyStatePatch, now: Instant): Option[UserState] = {
    val after = synchronized {
      val stored = store.get(userId)
      val next   = patch.applyTo(stored.getOrElse(UserState.empty(userId, now)))
        .copy(updatedAt = UserState.nextUpdatedAt(stored.map(_.updatedAt), now))
      store(userId) = next
      next
    }
    published(after)
    Some(after)
  }

  def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] = {
    val (after, written) = synchronized {
      val stored = store.get(userId)
      val base   = stored.getOrElse(UserState.empty(userId, now))
      change.applyTo(base.hiddenFilmsByCountry.getOrElse(country, Set.empty)) match {
        case None         => (base, false)
        case Some(bucket) =>
          val next = base.copy(
            hiddenFilmsByCountry = base.hiddenFilmsByCountry.updated(country, bucket),
            updatedAt            = UserState.nextUpdatedAt(stored.map(_.updatedAt), now))
          store(userId) = next
          (next, true)
      }
    }
    if (written) published(after)
    Some(after)
  }

  /** What the Mongo change stream would deliver for a write that landed. */
  private def published(state: UserState): Unit = {
    liveness.delivered(UserStateRepository.Collection)
    listener.foreach { case (onUpsert, _, _) => onUpsert(state) }
  }

  def delete(userId: String): Unit = {
    synchronized(store.remove(userId))
    liveness.delivered(UserStateRepository.Collection)
    listener.foreach { case (_, onDelete, _) => onDelete(userId) }
  }

  def close(): Unit = ()

  override def watchChanges(
    onUpsert:     UserState => Unit,
    onDelete:     String => Unit,
    onDisconnect: () => Unit
  ): Option[AutoCloseable] = {
    listener = Some((onUpsert, onDelete, onDisconnect))
    liveness.watching(UserStateRepository.Collection)
    Some(new AutoCloseable { override def close(): Unit = { listener = None } })
  }

  override def changeStreamLiveness: ChangeStreamLiveness = liveness

  /** Test-only: simulate the underlying cursor dying, without a real Mongo to
   *  kill — the fake's stand-in for a driver `onError`/`onComplete`. */
  def simulateDisconnect(): Unit = listener.foreach { case (_, _, onDisconnect) => onDisconnect() }
}
