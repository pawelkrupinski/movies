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

class UptimeController(cc: ControllerComponents, adminAction: AdminAction, monitor: UptimeMonitor, filmwebFallback: FilmwebFallbackStore)(using mat: Materializer) extends AbstractController(cc) {

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

  // Triage classification: a row is judged by its most recent activity. A cinema
  // scrapes roughly every ~5 min (legacy) / ≤15 min (queue), so the last ≤3
  // buckets that recorded anything ≈ "the last few scrapes".
  private val RecentScrapes = 3
  private sealed trait Health
  private case object Failing extends Health
  private case object Zero    extends Health
  private case object Healthy extends Health

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

  def index: Action[AnyContent] = adminAction {
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

    val tags = monitor.serviceTagsSnapshot()
    def row(n: String) =
      ServiceRow(n, barsFor(n), monitor.averageMs1h(n), monitor.averageMsTotal(n), tags = tags.getOrElse(n, Set.empty))
    val (failing, zero, cinemasByCity, services, other) = groupRows(active, row)
    Ok(views.html.uptime(failing, zero, cinemasByCity, services, other))
  }

  /** Split the active services into the page's sections. Two triage sections lead:
   *  rows that have been FAILING (every one of their last ≤3 active 15-min buckets
   *  is fully red) and rows returning NO SCREENINGS (last ≤3 active buckets all
   *  white) are pulled to the top — cinemas and enrichment services together, in a
   *  flat list tagged with each row's city — so a glance shows what's broken now.
   *  Everything healthy keeps its normal home: cinemas grouped by city, the
   *  enrichment-service rows, then "Other". A cinema's "<cinema>|enrichment"
   *  sub-row travels with it (and a failing sub-row promotes its cinema, keeping
   *  cinema + enrichment together). Package-private and parameterised on `row` so
   *  the grouping is unit-testable without rendering or a Materializer. */
  private[controllers] def groupRows(active: Set[String], row: String => ServiceRow)
      : (Seq[FlaggedRow], Seq[FlaggedRow], Seq[(String, Seq[ServiceRow])], Seq[ServiceRow], Seq[ServiceRow]) = {
    def cinemaRow(displayName: String): ServiceRow = {
      val enrichService = UptimeMonitor.enrichmentService(displayName)
      row(displayName).copy(enrichment = Option.when(active.contains(enrichService))(row(enrichService)))
    }

    // Built once, in city order; reused for both the triage split and the
    // by-city section so a cinema's row identity is stable.
    val cinemaUnits: Seq[(String, ServiceRow)] = Cinema.byCity.flatMap { case (city, venues) =>
      venues.map(_.displayName).filter(active.contains).map(dn => city -> cinemaRow(dn))
    }
    val serviceRows = enrichmentNames.filter(active.contains).map(row)
    // Exclude the "<cinema>|enrichment" services — they render as cinema sub-rows.
    val otherRows = (active -- cinemaNames.toSet -- enrichmentNames.toSet)
      .filterNot(UptimeMonitor.isEnrichmentService).toSeq.sorted.map(row)

    // Flat triage candidates, in the order cinemas → enrichment services → other,
    // each carrying the city it belongs to (None for non-cinema rows).
    val candidates: Seq[(ServiceRow, Option[String])] =
      cinemaUnits.map { case (city, r) => r -> Some(city) } ++
        serviceRows.map(_ -> None) ++ otherRows.map(_ -> None)
    val failing = candidates.collect { case (r, c) if health(r) == Failing => FlaggedRow(r, c) }
    val zero    = candidates.collect { case (r, c) if health(r) == Zero    => FlaggedRow(r, c) }

    // The remainder (healthy / mixed) keeps its normal home.
    val cinemasByCity = Cinema.byCity.flatMap { case (city, _) =>
      val rows = cinemaUnits.collect { case (c, r) if c == city && health(r) == Healthy => r }
      Option.when(rows.nonEmpty)(city -> rows)
    }
    val services = serviceRows.filter(health(_) == Healthy)
    val other    = otherRows.filter(health(_) == Healthy)
    (failing, zero, cinemasByCity, services, other)
  }

  /** Health of a row over its most recent activity: a cinema unit is the WORST of
   *  its scrape row and its attached enrichment sub-row, so a failing enrichment
   *  surfaces the cinema too (keeping the pair together). */
  private def health(r: ServiceRow): Health =
    worse(classify(r.bars), r.enrichment.map(e => classify(e.bars)).getOrElse(Healthy))

  /** Classify a bar series by its last `RecentScrapes` buckets that recorded any
   *  activity (ignoring untouched "empty" slots): all red ⇒ Failing, all white
   *  ⇒ Zero, anything else (any success, or no activity at all) ⇒ Healthy. A lone
   *  red blip among greens stays Healthy; a brand-new service that has only ever
   *  failed is Failing from its first bucket. Yellow (partial failure) is NOT
   *  failing. */
  private def classify(bars: Seq[BarData]): Health = {
    val recent = bars.iterator.map(_.status).filter(_ != "empty").toSeq.takeRight(RecentScrapes)
    if (recent.isEmpty) Healthy
    else if (recent.forall(_ == "red")) Failing
    else if (recent.forall(_ == "zero")) Zero
    else Healthy
  }

  private def worse(a: Health, b: Health): Health =
    if (a == Failing || b == Failing) Failing
    else if (a == Zero || b == Zero) Zero
    else Healthy

  def stream: Action[AnyContent] = adminAction {
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
  def imgEvent: Action[JsValue] = adminAction(parse.json) { request =>
    val events = (request.body \ "events").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
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
  def fallback: Action[AnyContent] = adminAction {
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
  enrichment: Option[ServiceRow] = None,
  // Generic per-row labels (UptimeMonitor service tags) rendered as chips next to
  // the name — e.g. the cinema's scraper-client marker "shared:FilmwebShowtimesClient".
  tags:       Set[String]        = Set.empty
) {
  /** The venue's public source-page URL, parsed out of the `url:` tag — the
   *  href the name links to. Same extractor /debug uses (one source of truth). */
  def url: Option[String] = UptimeMonitor.urlFromTags(tags)
}

/** A row promoted into the leading "Failing" / "No screenings" triage sections,
 *  carrying the city it was pulled out of (None for non-cinema rows). */
case class FlaggedRow(row: ServiceRow, city: Option[String])
