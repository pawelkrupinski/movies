package controllers

import play.api.libs.json.{JsArray, JsString, Json, JsValue, Writes}
import play.api.mvc._
import services.UptimeMonitor
import services.UptimeMonitor._

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

class UptimeController(cc: ControllerComponents, monitor: UptimeMonitor) extends AbstractController(cc) {

  private val cinemaNames = Seq(
    "Multikino Stary Browar", "Kino Malta Charlie Monroe", "Kino Pałacowe",
    "Helios Posnania", "Cinema City Kinepolis", "Cinema City Poznań Plaza",
    "Kino Muza", "Kino Bułgarska 19", "Kino Apollo", "Kino Rialto"
  )

  private val enrichmentNames = Seq(
    "TMDB", "IMDb", "Filmweb", "Metacritic", "Rotten Tomatoes"
  )

  private val warsawZone = ZoneId.of("Europe/Warsaw")
  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm").withZone(warsawZone)
  private val dateFmt = DateTimeFormatter.ofPattern("d MMM").withZone(warsawZone)

  def index: Action[AnyContent] = Action {
    val now = System.currentTimeMillis()
    val currentBucket = bucketTimestamp(now)
    val slots = (0 until MaxBuckets).reverse.map(i => currentBucket - i * BucketDurationMs)
    val active = monitor.services

    def barsFor(serviceName: String): Seq[BarData] = {
      val history = monitor.history(serviceName).map(b => b.timestamp -> b).toMap
      slots.map { ts =>
        val from = Instant.ofEpochMilli(ts)
        val to   = Instant.ofEpochMilli(ts + BucketDurationMs)
        history.get(ts) match {
          case Some(b) =>
            BarData(serviceName, timeFmt.format(from), timeFmt.format(to), dateFmt.format(from),
              b.status, b.successes, b.failures, b.errors)
          case None =>
            BarData(serviceName, timeFmt.format(from), timeFmt.format(to), dateFmt.format(from),
              "empty", 0, 0, Seq.empty)
        }
      }
    }

    val cinemas  = cinemaNames.filter(active.contains).map(n => ServiceRow(n, barsFor(n)))
    val services = enrichmentNames.filter(active.contains).map(n => ServiceRow(n, barsFor(n)))
    val other    = (active -- cinemaNames.toSet -- enrichmentNames.toSet).toSeq.sorted.map(n => ServiceRow(n, barsFor(n)))

    Ok(views.html.uptime(cinemas, services, other))
  }
}

case class BarData(
  service: String,
  timeFrom: String,
  timeTo: String,
  dateLabel: String,
  status: String,
  successes: Int,
  failures: Int,
  errors: Seq[String]
)

object BarData {
  implicit val writes: Writes[BarData] = (b: BarData) => Json.obj(
    "service"   -> b.service,
    "timeFrom"  -> b.timeFrom,
    "timeTo"    -> b.timeTo,
    "dateLabel" -> b.dateLabel,
    "status"    -> b.status,
    "successes" -> b.successes,
    "failures"  -> b.failures,
    "errors"    -> b.errors
  )
}

case class ServiceRow(name: String, bars: Seq[BarData])
