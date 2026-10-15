package services.facebook

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{MongoCollection, SingleObservableFuture}
import org.mongodb.scala.model.{Filters, UpdateOptions, Updates}
import play.api.libs.json.{JsObject, Json}

import java.time.{Clock, Instant}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

/** The share (%) of the app's hourly allowance Facebook says is spent: past 100 it refuses the
 *  app (code 4) until enough of the last hour's calls have aged out. */
final case class AppUsage(percent: Int)
object AppUsage {
  /** The response header every Graph API call made with the app's credentials carries. */
  val Header = "x-app-usage"

  /** The `X-App-Usage` header, `{"call_count":28,"total_time":25,"total_cputime":25}` — each a
   *  percentage of its own allowance, so the highest is the one that limits. None for a header
   *  that names no percentage. */
  def parse(header: String): Option[AppUsage] =
    Try(Json.parse(header)).toOption.collect { case o: JsObject => o }
      .flatMap(_.values.flatMap(_.asOpt[Int]).maxOption).map(AppUsage(_))

  /** The usage a Graph API response reports, error responses included. */
  def of(headers: java.net.http.HttpHeaders): Option[AppUsage] =
    Option(headers.firstValue(Header).orElse(null)).flatMap(parse)
}

/** The fleet's Facebook re-scrape slots, as far as stopping them goes: one schedule every
 *  country's worker sends by, so a hold here stops the whole fleet. */
trait FacebookSlots {
  /** No slot before `until`, whatever was due; never pulls an earlier hold in. */
  def holdSlots(until: Instant): Unit
}

/** The slots' schedule as the one quota document of the fleet database's re-scrape queue. */
class MongoFacebookSlots(collection: MongoCollection[Document]) extends FacebookSlots {
  def holdSlots(until: Instant): Unit = {
    Await.result(collection.updateOne(Filters.eq("_id", MongoFacebookSlots.QuotaId),
      Updates.max(MongoFacebookSlots.NextSlot, new java.util.Date(until.toEpochMilli)), new UpdateOptions().upsert(true)).toFuture(), 10.seconds)
    ()
  }
}

object MongoFacebookSlots {
  /** The re-scrape queue's collection, in the fleet database ([[services.FleetDatabase]]). */
  val Collection = "facebook_rescrapes"
  val QuotaId    = "quota"
  val NextSlot   = "nextSlot"
}

/**
 * Facebook's word on the app's allowance, acted on: every Graph API call the fleet makes with the
 * app's credentials — the worker's re-scrapes and the web's Facebook sign-ins alike — spends the
 * same allowance, so whichever reads [[FacebookUsageBrake.UsageCeiling]] holds the re-scrapes, the
 * only calls the fleet can put off, before Facebook refuses the app and the fleet waits an hour.
 */
class FacebookUsageBrake(slots: FacebookSlots, clock: Clock) {
  import FacebookUsageBrake.*

  /** Hold the fleet's re-scrapes when `usage` is at the ceiling: the instant they resume, if held. */
  def observe(usage: AppUsage): Option[Instant] =
    Option.when(usage.percent >= UsageCeiling)(clock.instant().plusMillis(usageHold(usage.percent).toMillis))
      .map { until => slots.holdSlots(until); until }
}

object FacebookUsageBrake {
  /** The share of the app's hourly allowance at which the fleet stops for [[usageHold]] rather than
   *  run on into Facebook's refusal and its hour-long hold. One request spends ~1.5% of the
   *  allowance (2026-10-15), and a reading may lag what Facebook has counted, so the ceiling sits
   *  25 points under the refusal: through an hour none of whose calls age out, with the reading five
   *  requests late, the fleet stops short of 90%. A lower ceiling costs little: once the hour's calls
   *  age out the pace is set by how fast they do, not by the level it is held at. */
  val UsageCeiling: Int = 75
  /** How long the fleet waits at [[UsageCeiling]]: the allowance is a rolling hour, so a few
   *  minutes frees a few of its calls once the hour's oldest have aged out. */
  val UsageHold: FiniteDuration = 5.minutes
  /** How long Facebook's refusal lasts — the allowance is a rolling hour; on 2026-09-25 requests
   *  were refused for about that long — and so the longest a usage hold is worth. */
  val RefusalHold: FiniteDuration = 1.hour
  /** The wait for a reading of `percent`: [[UsageHold]], doubled for every point over
   *  [[UsageCeiling]], at most [[RefusalHold]]. A flat hold let every resume add one more request
   *  while the hour's first calls had not aged out yet: 91% → 96% → 99% on 2026-10-15. */
  def usageHold(percent: Int): FiniteDuration =
    (UsageHold * (1L << (percent - UsageCeiling).max(0).min(4))).min(RefusalHold)
}
