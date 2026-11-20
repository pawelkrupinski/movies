package services.users

import java.time.Instant

import scala.jdk.CollectionConverters.*

import models.UserState
import org.bson.{BsonArray, BsonDateTime, BsonDocument, BsonInt32, BsonString, BsonValue}
import org.mongodb.scala.model.{Aggregates, Field}
import org.mongodb.scala.{MongoCollection, MongoDatabase}
import play.api.Logger

/** How `userStates` is laid out in Mongo: its codec, its unique `userId` index, and the server-side pipelines the
 *  writes of [[MongoUserStateRepository]] are. */
object UserStatesCollection {
  // Logged as the store it opens for, as before it moved here.
  private val logger = Logger("services.users.MongoUserStateRepository")

  /** The collection, through the users' codecs. */
  def of(db: MongoDatabase): MongoCollection[UserState] =
    db.withCodecRegistry(UserCodecs.registry).getCollection[UserState](UserStateRepository.Collection)

  /** [[of]], its write-error series seeded and its unique `userId` index ensured (reported to `indexHealth`): the
   *  store's write side opens it so. */
  def open(db: MongoDatabase, indexHealth: UserStateIndexHealth): MongoCollection[UserState] = {
    val c = of(db)
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

  import UpdatePipeline.{field, literal, op}

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

