package services.enrichment

import java.time.{Instant, LocalDate, ZoneId}

import org.mongodb.scala.MongoDatabase

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

  val DayZone: ZoneId = models.TimeZones.UsCentral

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

/** The budget over a keyed store of day counters: one document per OMDb day (`_id` its date, `used` the permits
 *  granted), taken by one atomic conditional increment each ([[tools.StoreCounters.increment]]), so every country's
 *  worker draws on the same count. A count read as any BSON number: the days opened before the keyed store hold an Int32,
 *  those since an Int64. */
abstract class KeyedOmdbBudget(days: tools.StoreCounters[String], limit: Int) extends OmdbBudget {
  import KeyedOmdbBudget.Used

  def take(ask: OmdbAsk, now: Instant): OmdbBudget.Permit = {
    val day     = OmdbBudget.dayOf(now).toString
    val ceiling = OmdbBudget.ceiling(ask, limit, now)
    if (ceiling > 0 && days.increment(day, Used, below = Some(ceiling.toLong)).isDefined) OmdbBudget.Granted
    else OmdbBudget.Spent(OmdbBudget.retryAt(ask, usedOn(day), limit, now))     // at this ask's ceiling
  }

  /** The day's count raised to the limit, never lowered, in one step (Mongo's `$max`): workers told at once that the
   *  key is spent leave it AT the limit -- a read-then-increment summed their shortfalls past it. */
  def markSpent(now: Instant): Unit = days.raise(OmdbBudget.dayOf(now).toString, Used, limit.toLong)

  private def usedOn(day: String): Int = days.counted(day, Used).toInt
}

object KeyedOmdbBudget {
  private val Used = "used"
}

/** The budget in this process: a test's, or a worker with no fleet database (it then spends a day's budget alone). */
final class InMemoryOmdbBudget private (days: tools.KeyedStore[String, Unit], limit: Int) extends KeyedOmdbBudget(days, limit) {
  def this(limit: Int = OmdbBudget.DailyLimit) = this(tools.KeyedStore.counters(new tools.InMemoryStoreDocuments()), limit)

  def usedToday(now: Instant): Int = days.counted(OmdbBudget.dayOf(now).toString, "used").toInt
}

/** The budget in the fleet database's `omdb_budget`. */
final class MongoOmdbBudget(db: MongoDatabase, limit: Int = OmdbBudget.DailyLimit) extends KeyedOmdbBudget(
  tools.KeyedStore.counters(tools.MongoStoreDocuments.of(db.getCollection(MongoOmdbBudget.Collection))), limit)

object MongoOmdbBudget {
  val Collection = "omdb_budget"
}
