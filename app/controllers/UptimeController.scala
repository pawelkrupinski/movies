package controllers

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

  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm").withZone(ZoneId.of("Europe/Warsaw"))

  def index: Action[AnyContent] = Action {
    val now = System.currentTimeMillis()
    val currentBucket = bucketTimestamp(now)
    val numBuckets = MaxBuckets
    val slots = (0 until numBuckets).reverse.map(i => currentBucket - i * BucketDurationMs)

    val active = monitor.services

    def barsFor(name: String): Seq[Bar] = {
      val history = monitor.history(name).map(b => b.timestamp -> b).toMap
      slots.map { ts =>
        val status = history.get(ts).map(_.status).getOrElse("empty")
        val label = timeFmt.format(Instant.ofEpochMilli(ts))
        val endLabel = timeFmt.format(Instant.ofEpochMilli(ts + BucketDurationMs))
        val tooltip = history.get(ts) match {
          case Some(b) => s"$label–$endLabel: ${b.successes} ok, ${b.failures} failed"
          case None    => s"$label–$endLabel: no data"
        }
        Bar(status, tooltip)
      }
    }

    val cinemas  = cinemaNames.filter(active.contains).map(n => ServiceRow(n, barsFor(n)))
    val services = enrichmentNames.filter(active.contains).map(n => ServiceRow(n, barsFor(n)))
    val other    = (active -- cinemaNames.toSet -- enrichmentNames.toSet).toSeq.sorted.map(n => ServiceRow(n, barsFor(n)))

    Ok(views.html.uptime(cinemas, services, other))
  }
}

case class Bar(status: String, tooltip: String)
case class ServiceRow(name: String, bars: Seq[Bar])
