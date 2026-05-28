package controllers

import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import play.api.mvc._
import services.UptimeMonitor
import services.UptimeMonitor._

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext

class UptimeController(cc: ControllerComponents, monitor: UptimeMonitor)(using mat: Materializer) extends AbstractController(cc) {

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
            BarData(serviceName, ts, timeFmt.format(from), timeFmt.format(to), dateFmt.format(from),
              b.status, b.successes, b.failures, b.errors)
          case None =>
            BarData(serviceName, ts, timeFmt.format(from), timeFmt.format(to), dateFmt.format(from),
              "empty", 0, 0, Seq.empty)
        }
      }
    }

    val cinemas  = cinemaNames.filter(active.contains).map(n => ServiceRow(n, barsFor(n)))
    val services = enrichmentNames.filter(active.contains).map(n => ServiceRow(n, barsFor(n)))
    val other    = (active -- cinemaNames.toSet -- enrichmentNames.toSet).toSeq.sorted.map(n => ServiceRow(n, barsFor(n)))

    Ok(views.html.uptime(cinemas, services, other))
  }

  def stream: Action[AnyContent] = Action {
    val (queue, source) = Source.queue[String](256, OverflowStrategy.dropHead)
      .preMaterialize()

    val listener: BucketListener = { (service, snapshot) =>
      val json = Json.obj(
        "service"   -> service,
        "bucketTs"  -> snapshot.timestamp,
        "timeFrom"  -> timeFmt.format(Instant.ofEpochMilli(snapshot.timestamp)),
        "timeTo"    -> timeFmt.format(Instant.ofEpochMilli(snapshot.timestamp + BucketDurationMs)),
        "dateLabel" -> dateFmt.format(Instant.ofEpochMilli(snapshot.timestamp)),
        "status"    -> snapshot.status,
        "successes" -> snapshot.successes,
        "failures"  -> snapshot.failures,
        "errors"    -> snapshot.errors
      )
      queue.offer(s"data: $json\n\n")
    }

    monitor.addListener(listener)

    Ok.chunked(
      source.watchTermination() { (_, done) =>
        done.onComplete(_ => monitor.removeListener(listener))(using ExecutionContext.global)
        org.apache.pekko.NotUsed
      }
    ).as("text/event-stream")
  }

  /** Browser-reported image load outcomes. The page's tracker batches
   *  ~10s of img onload/onerror events and POSTs them here so the
   *  uptime page sees per-host image-fetch reliability (the
   *  `images.weserv.nl` proxy that fronts every cinema poster, the
   *  origin CDNs we link directly, etc.). */
  def imgEvent: Action[JsValue] = Action(parse.json) { req =>
    val events = (req.body \ "events").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
    events.foreach { e =>
      val host    = (e \ "host").asOpt[String].getOrElse("unknown")
      val success = (e \ "success").asOpt[Boolean].getOrElse(false)
      val service = s"img: $host"
      if (success) monitor.recordSuccess(service)
      else {
        val error = (e \ "error").asOpt[String].getOrElse("image load failed")
        monitor.recordFailure(service, error.take(200))
      }
    }
    NoContent
  }
}

case class BarData(
  service: String,
  bucketTs: Long,
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
    "bucketTs"  -> b.bucketTs,
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
