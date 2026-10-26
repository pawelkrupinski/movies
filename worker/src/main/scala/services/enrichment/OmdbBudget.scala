package services.enrichment

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, FindOneAndUpdateOptions, ReturnDocument, Updates}
import org.mongodb.scala.{MongoCollection, MongoDatabase, SingleObservableFuture}

import java.time.{Instant, LocalDate, ZoneId}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The fleet's ONE daily OMDb budget: every country's worker holds the same key, and OMDb's free plan answers 1,000
 * requests a day across all of them, then "Request limit reached!" as a 401 to everyone until its day turns. Before
 * this each worker asked as it pleased: a deploy restarted all five, each swept its whole cache five minutes after
 * boot (es 285 rows, pl 1,125, us 2,084, de 4,691, uk 2,564 on 2026-10-26 03:34-03:40), and the key was spent by the
 * time de asked -- every new film's id lookup that day got 401s with them.
 *
 * Every OMDb request takes a permit first ([[OMDbClient]]), by what it is for ([[OmdbAsk]]); higher asks may use more
 * of the day. A new film being identified may use all of it, and the last [[NewFilmReserve]] is its alone; an answer a
 * user waits on stops 10% short, routine refreshes 20% short, and a bulk sweep takes only what is left after those
 * reserves, PACED across the day so a sweep at 00:05 cannot take the morning's share. A refused permit is not a
 * failure of OMDb: it is "budget spent, ask again at [[OmdbBudget.Spent.retryAt]]", and no request is made.
 *
 * OMDb's day: its 401s stopped at 05:00 UTC on both 2026-10-21 and 2026-10-23 (`kinowo_worker_http_total
 * {service="omdb"}`, first success after a refused run in the 05:00-05:07 bucket each time) -- midnight in the US
 * Central zone, which is what [[DayZone]] names so the boundary follows its daylight saving.
 */
enum OmdbAsk {
  /** A newly listed film's IMDb id ([[ImdbIdResolver]]'s OMDb rung): asked at once, per the settle rule. */
  case NewFilm
  /** An answer a user is waiting on (an operator's single-film refresh). */
  case UserWaiting
  /** A scheduled refresh of a known film. */
  case Routine
  /** A sweep over the whole cache ([[OmdbBackfill.refreshAll]]): leftover only, paced. */
  case Bulk
}

trait OmdbBudget {
  /** One request's permit for `ask` at `now`, counted against OMDb's day when granted. */
  def take(ask: OmdbAsk, now: Instant): OmdbBudget.Permit

  /** OMDb said its quota is spent (someone else holds the key too, or this count drifted): nothing more today. */
  def markSpent(now: Instant): Unit
}

object OmdbBudget {
  sealed trait Permit
  case object Granted extends Permit
  /** No permit: ask again at `retryAt` (OMDb's next day, or a paced bulk sweep's next share). */
  final case class Spent(retryAt: Instant) extends Permit

  /** No budget at all: every permit granted. For specs that are not about the budget. */
  val Unmetered: OmdbBudget = new OmdbBudget {
    def take(ask: OmdbAsk, now: Instant): Permit = Granted
    def markSpent(now: Instant): Unit = ()
  }

  /** OMDb's free plan, per key per day. */
  val DailyLimit: Int = 1000
  /** The share of the day only a new film may use. */
  val NewFilmReserve: Double = 0.10
  /** The most of the day a bulk sweep may use, by the day's end; at any moment, that share of the day elapsed. */
  val BulkShare: Double = 0.60

  val DayZone: ZoneId = ZoneId.of("America/Chicago")

  def dayOf(now: Instant): LocalDate = now.atZone(DayZone).toLocalDate
  def nextReset(now: Instant): Instant = dayOf(now).plusDays(1).atStartOfDay(DayZone).toInstant

  private def elapsedShare(now: Instant): Double = {
    val start = dayOf(now).atStartOfDay(DayZone).toInstant
    val total = java.time.Duration.between(start, nextReset(now)).toMillis.toDouble
    java.time.Duration.between(start, now).toMillis / total
  }

  /** How many of the day's requests `ask` may have been granted, counting all asks, for one more to be granted. */
  def ceiling(ask: OmdbAsk, limit: Int, now: Instant): Int = ask match {
    case OmdbAsk.NewFilm     => limit
    case OmdbAsk.UserWaiting => math.floor(limit * (1 - NewFilmReserve)).toInt
    case OmdbAsk.Routine     => math.floor(limit * (1 - 2 * NewFilmReserve)).toInt
    case OmdbAsk.Bulk        => math.floor(limit * BulkShare * elapsedShare(now)).toInt
  }

  /** Where a refused `ask` asks again: a bulk sweep below its day's share in a few minutes (one more permit's worth
   *  of the day), everything else at OMDb's next day. */
  def retryAt(ask: OmdbAsk, used: Int, limit: Int, now: Instant): Instant = {
    val reset = nextReset(now)
    if (ask == OmdbAsk.Bulk && used < limit * BulkShare) {
      val day = java.time.Duration.between(dayOf(now).atStartOfDay(DayZone).toInstant, reset).toMillis
      val share = (day / (limit * BulkShare)).toLong.max(1L)
      Seq(now.plusMillis(share), reset).min
    } else reset
  }
}

/** The budget in this process: a test's, or a worker with no fleet database (it then spends a day's budget alone). */
final class InMemoryOmdbBudget(limit: Int = OmdbBudget.DailyLimit) extends OmdbBudget {
  private var day: Option[LocalDate] = None
  private var used = 0

  private def roll(now: Instant): Unit = {
    val today = OmdbBudget.dayOf(now)
    if (!day.contains(today)) { day = Some(today); used = 0 }
  }

  def take(ask: OmdbAsk, now: Instant): OmdbBudget.Permit = synchronized {
    roll(now)
    if (used < OmdbBudget.ceiling(ask, limit, now)) { used += 1; OmdbBudget.Granted }
    else OmdbBudget.Spent(OmdbBudget.retryAt(ask, used, limit, now))
  }

  def markSpent(now: Instant): Unit = synchronized { roll(now); used = used.max(limit) }

  def usedToday(now: Instant): Int = synchronized { roll(now); used }
}

/** The budget in the fleet database's `omdb_budget`: one document per OMDb day (`_id` its date, `used` the permits
 *  granted), taken by one atomic conditional increment each, so every country's worker draws on the same count. */
final class MongoOmdbBudget(db: MongoDatabase, limit: Int = OmdbBudget.DailyLimit) extends OmdbBudget {
  private val Timeout = 10.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](MongoOmdbBudget.Collection)

  def take(ask: OmdbAsk, now: Instant): OmdbBudget.Permit = {
    val day     = OmdbBudget.dayOf(now).toString
    val ceiling = OmdbBudget.ceiling(ask, limit, now)
    val below   = Filters.and(Filters.eq("_id", day), Filters.or(Filters.exists("used", false), Filters.lt("used", ceiling)))
    if (ceiling <= 0) OmdbBudget.Spent(OmdbBudget.retryAt(ask, usedOn(day), limit, now))
    else
      try {
        Await.result(collection.findOneAndUpdate(below, Updates.inc("used", 1),
          FindOneAndUpdateOptions().upsert(true).returnDocument(ReturnDocument.AFTER)).toFutureOption(), Timeout)
        OmdbBudget.Granted
      } catch {
        // The day's document exists and is at this ask's ceiling: the upsert met its id.
        // A findAndModify's upsert reports the clash as a command error, not a write error (MongoFleetHostPace).
        case e: com.mongodb.MongoException if e.getCode == 11000 =>
          OmdbBudget.Spent(OmdbBudget.retryAt(ask, usedOn(day), limit, now))
      }
  }

  def markSpent(now: Instant): Unit = {
    Await.result(collection.updateOne(Filters.eq("_id", OmdbBudget.dayOf(now).toString), Updates.max("used", limit),
      org.mongodb.scala.model.UpdateOptions().upsert(true)).toFuture(), Timeout)
    ()
  }

  private def usedOn(day: String): Int =
    Await.result(collection.find(Filters.eq("_id", day)).headOption(), Timeout)
      .flatMap(_.get[org.mongodb.scala.bson.BsonInt32]("used")).map(_.getValue).getOrElse(0)
}

object MongoOmdbBudget {
  val Collection = "omdb_budget"
}
