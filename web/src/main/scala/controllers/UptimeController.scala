package controllers

import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import play.api.mvc._
import models.Cinema
import services.UptimeMonitor
import services.UptimeMonitor._
import services.fallback.{FilmwebFallbackState, FilmwebFallbackStore}

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UptimeController(cc: ControllerComponents, monitor: UptimeMonitor, filmwebFallback: FilmwebFallbackStore)(using mat: Materializer) extends AbstractController(cc) {

  // Derived from the cinema model so every wired scraper groups under the
  // "Cinemas" header automatically — no edit needed when a cinema is added.
  private val cinemaNames = Cinema.all.map(_.displayName)

  // External enrichment sources plus network-level (chain-wide) detail health.
  // Cinema City fetches each film's detail once per network and records it here
  // as one "Cinema City Enrichment" entry instead of one "<venue>|enrichment"
  // sub-row per venue (see EnrichDetailsHandler /
  // CinemaCityScraper.enrichmentServiceOverride). Order here is the render order
  // of the Global section, so the chain-wide row leads the external sources.
  private val enrichmentNames = Seq(
    "Cinema City Enrichment", "TMDB", "IMDb", "Filmweb", "Metacritic", "Rotten Tomatoes"
  )

  // SSE batching: a poll cycle can flip one bucket per active service at once.
  // Buffer generously (drop oldest under a burst), then coalesce everything that
  // lands inside BatchWindow into one frame so the page never sees a per-service
  // message storm. BatchMaxSize caps a single frame; BatchWindow bounds latency.
  private val StreamBufferSize = 2048
  private val BatchMaxSize     = 1024
  private val BatchWindow      = 250.millis

  private val warsawZone = ZoneId.of("Europe/Warsaw")
  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm").withZone(warsawZone)
  private val dateFmt = DateTimeFormatter.ofPattern("d MMM").withZone(warsawZone)

  /** One rendered bar, with its bucket window stamped in Warsaw time. Shared by
   *  the full-page render and the live SSE feed so both label a bucket the same. */
  private def barData(service: String, bucketTs: Long, status: String,
                      successes: Int, failures: Int, zeroes: Int, errors: Seq[String],
                      fallback: Boolean = false): BarData = {
    val from = Instant.ofEpochMilli(bucketTs)
    val to   = Instant.ofEpochMilli(bucketTs + BucketDurationMs)
    BarData(service, bucketTs, timeFmt.format(from), timeFmt.format(to), dateFmt.format(from),
      status, successes, failures, zeroes, errors, fallback)
  }

  def index: Action[AnyContent] = Action {
    val now = System.currentTimeMillis()
    val currentBucket = bucketTimestamp(now)
    val slots = (0 until MaxBuckets).reverse.map(i => currentBucket - i * BucketDurationMs)
    val active = monitor.services

    def barsFor(serviceName: String): Seq[BarData] = {
      val history = monitor.history(serviceName).map(b => b.timestamp -> b).toMap
      slots.map { ts =>
        history.get(ts) match {
          case Some(b) => barData(serviceName, ts, b.status, b.successes, b.failures, b.zeroes, b.errors, b.fallback)
          case None    => barData(serviceName, ts, "empty", 0, 0, 0, Seq.empty)
        }
      }
    }

    def row(n: String) = ServiceRow(n, barsFor(n), monitor.averageMs1h(n), monitor.averageMsTotal(n))
    val (cinemasByCity, services, other) = groupRows(active, row)
    Ok(views.html.uptime(cinemasByCity, services, other))
  }

  /** Split the active services into the page's three sections. A cinema's
   *  "<cinema>|enrichment" service (deferred-detail task health) is attached as
   *  a sub-row of the cinema instead of floating in "Other". Package-private and
   *  parameterised on `row` so the grouping is unit-testable without rendering or
   *  a Materializer. */
  private[controllers] def groupRows(active: Set[String], row: String => ServiceRow)
      : (Seq[(String, Seq[ServiceRow])], Seq[ServiceRow], Seq[ServiceRow]) = {
    def cinemaRow(displayName: String): ServiceRow = {
      val enrichSvc = UptimeMonitor.enrichmentService(displayName)
      row(displayName).copy(enrichment = Option.when(active.contains(enrichSvc))(row(enrichSvc)))
    }
    val cinemasByCity = Cinema.byCity.flatMap { case (city, venues) =>
      val rows = venues.map(_.displayName).filter(active.contains).map(cinemaRow)
      Option.when(rows.nonEmpty)(city -> rows)
    }
    val services = enrichmentNames.filter(active.contains).map(row)
    // Exclude the "<cinema>|enrichment" services — they render as cinema sub-rows.
    val other = (active -- cinemaNames.toSet -- enrichmentNames.toSet)
      .filterNot(UptimeMonitor.isEnrichmentService).toSeq.sorted.map(row)
    (cinemasByCity, services, other)
  }

  def stream: Action[AnyContent] = Action {
    Ok.chunked(eventSource()).as("text/event-stream")
  }

  /** The live `/uptime/stream` feed. One listener per connected browser pushes
   *  each changed bucket as a `BarData`; `groupedWithin` then coalesces a burst
   *  into a SINGLE `data: [...]` SSE frame. A worker poll that flips N buckets
   *  at once therefore costs the page one frame and one DOM pass, not N — so the
   *  stream's cost stays flat as the cinema roster (and thus N) grows, instead
   *  of one frame per scraper per poll. */
  private[controllers] def eventSource(): Source[String, org.apache.pekko.NotUsed] = {
    val (queue, source) = Source.queue[BarData](StreamBufferSize, OverflowStrategy.dropHead)
      .preMaterialize()

    val listener: BucketListener = { (service, snapshot) =>
      queue.offer(barData(service, snapshot.timestamp, snapshot.status,
        snapshot.successes, snapshot.failures, snapshot.zeroes, snapshot.errors, snapshot.fallback))
      ()
    }
    monitor.addListener(listener)

    source
      .groupedWithin(BatchMaxSize, BatchWindow)
      .map(batch => s"data: ${Json.toJson(batch)}\n\n")
      .watchTermination() { (_, done) =>
        done.onComplete(_ => monitor.removeListener(listener))(using ExecutionContext.global)
        org.apache.pekko.NotUsed
      }
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

  /** Status page for the Filmweb fallback: cinemas CURRENTLY served by Filmweb
   *  because their own scraper is down/empty, cinemas that RECENTLY RECOVERED, and
   *  cinemas that are Filmweb-only BY DESIGN (their only scraper is Filmweb). */
  def fallback: Action[AnyContent] = Action {
    val all         = filmwebFallback.findAll()
    val onFallback  = all.filter(_.active).sortBy(_.cinema).map(fallbackRow)
    val recovered   = all.filterNot(_.active).filter(_.history.nonEmpty).sortBy(_.cinema).map(fallbackRow)
    val filmwebOnly = filmwebFallback.filmwebOnly().toSeq.sorted
    Ok(views.html.uptimeFallback(onFallback, recovered, filmwebOnly))
  }

  private val tsFmt = DateTimeFormatter.ofPattern("d MMM HH:mm").withZone(warsawZone)
  private def fmtInstant(i: Instant): String = tsFmt.format(i)

  private def fallbackRow(s: FilmwebFallbackState): FallbackRow = FallbackRow(
    cinema    = s.cinema,
    filmwebId = s.filmwebCinemaId.map(_.toString).getOrElse("—"),
    since     = s.since.map(fmtInstant).getOrElse("—"),
    reason    = s.lastReason.getOrElse("—"),
    fails     = s.consecutiveFailures,
    nextProbe = s.nextPrimaryProbeAt.map(fmtInstant).getOrElse("—"),
    updated   = fmtInstant(s.updatedAt),
    history   = s.history.take(8).map(e => s"${fmtInstant(e.at)} · ${e.event} · ${e.reason}")
  )
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
  zeroes: Int,
  errors: Seq[String],
  fallback: Boolean = false
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
    "zeroes"    -> b.zeroes,
    "errors"    -> b.errors,
    "fallback"  -> b.fallback
  )
}

case class FallbackRow(
  cinema: String, filmwebId: String, since: String, reason: String,
  fails: Int, nextProbe: String, updated: String, history: Seq[String]
)

case class ServiceRow(
  name:       String,
  bars:       Seq[BarData],
  avg1hMs:    Option[Long]       = None,
  avgTotalMs: Option[Long]       = None,
  // The cinema's deferred-detail enrichment health, rendered as an indented
  // sub-row beneath the scrape bar. None for non-cinema rows / cinemas with no
  // deferred detail.
  enrichment: Option[ServiceRow] = None
)
