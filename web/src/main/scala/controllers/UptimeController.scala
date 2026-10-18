package controllers

import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc._
import services.UptimeMonitor
import services.UptimeMonitor._
import services.fallback.{FallbackState, FallbackStore}

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UptimeController(cc: ControllerComponents, adminAction: AdminAction, monitor: UptimeMonitor, filmwebFallback: FallbackStore,
                       closures: services.closure.ClosureLedgerReader, country: models.Country, clock: java.time.Clock)(using mat: Materializer) extends AbstractController(cc) {

  // Which section each service lands in, for this deployment's roster.
  private val triage = new UptimeTriage(country)

  // Where an `imgEvent` beacon's records go — see the endpoint's own comment for why it is anonymous at all.
  private val imageLoads = new ImageLoadBeacon(monitor)

  // SSE batching: a poll cycle can flip one bucket per active service at once.
  // Buffer generously (drop oldest under a burst), then coalesce everything that
  // lands inside BatchWindow into one frame so the page never sees a per-service
  // message storm. BatchMaxSize caps a single frame; BatchWindow bounds latency.
  private val StreamBufferSize = 2048
  private val BatchMaxSize     = 1024
  private val BatchWindow      = 250.millis

  /** The page's clock is the deployment's country's: the UK page in London time, not Warsaw's. */
  private val formats = UptimeController.Formats(country.zone)
  import formats.{dateFmt, timeFmt, tsFmt}

  /** One rendered bar, with its bucket window stamped in the country's time. Shared by
   *  the full-page render and the live SSE feed so both label a bucket the same. */
  private def barData(service: String, b: UptimeMonitor.BucketSnapshot): BarData =
    barData(service, b, slotLabels(b.timestamp))

  private def barData(service: String, b: UptimeMonitor.BucketSnapshot, labels: SlotLabels): BarData =
    BarData(service, b.timestamp, labels.timeFrom, labels.timeTo, labels.dateLabel,
      b.status, b.successes, b.failures, b.zeroes, b.errors, b.fallback, b.thin)

  private final case class SlotLabels(timeFrom: String, timeTo: String, dateLabel: String)

  private def slotLabels(timestamp: Long): SlotLabels = {
    val from = Instant.ofEpochMilli(timestamp)
    SlotLabels(timeFmt.format(from), timeFmt.format(Instant.ofEpochMilli(timestamp + BucketDurationMs)), dateFmt.format(from))
  }

  def index: Action[AnyContent] = adminAction {
    val sections = triage.groupRows(monitor.services, monitor.recentStatuses(_, UptimeTriage.RecentScrapes),
      monitor.recentErrors(_, UptimeTriage.RecentScrapes), rowBuilder())
    Ok(views.html.uptime(sections.failing, sections.gone, sections.zero, activeFallbacks(), sections.cinemasByCity,
      sections.services, sections.other, sections.hiddenHealthyCinemas, thin = sections.thin,
      hiddenThin = sections.hiddenThin, dropped = droppedVenues(), current = country))
  }

  /** Builds one service's rendered row — its 96-slot bar series up to the current
   *  bucket, its timings and tags — against slots labelled once per request, not
   *  once per bar: every row shares them. */
  private def rowBuilder(): String => ServiceRow = {
    val currentBucket = bucketTimestamp(clock.millis())
    val slots = (0 until MaxBuckets).reverse.map { i =>
      val timestamp = currentBucket - i * BucketDurationMs
      timestamp -> slotLabels(timestamp)
    }
    val tags = monitor.serviceTagsSnapshot()
    name => ServiceRow(name, barsFor(name, slots), monitor.averageMs1h(name), monitor.averageMsTotal(name),
      tags = tags.getOrElse(name, Set.empty))
  }

  private def barsFor(serviceName: String, slots: Seq[(Long, SlotLabels)]): Seq[BarData] = {
    val history = monitor.history(serviceName).map(b => b.timestamp -> b).toMap
    slots.map { case (timestamp, labels) =>
      barData(serviceName, history.getOrElse(timestamp, UptimeMonitor.BucketSnapshot(timestamp, 0, 0, 0, Seq.empty)), labels)
    }
  }

  /** The venues every source dropped, from the closure sweep's ledger: a CLOSED one awaits
   *  its retirement; a SOURCE-DROPPED one is a venue still open that we no longer have a
   *  programme for, which only a new source fixes. Gaps first, oldest first. */
  private def droppedVenues(): Seq[DroppedRow] =
    closures.entries().toSeq.sortBy { case (name, entry) => (entry.kind == services.closure.LedgerKind.Closed, entry.at, name) }.map {
      case (name, entry) => DroppedRow(name, entry.kind match {
        case services.closure.LedgerKind.SourceDropped => "open — needs a new source"
        case services.closure.LedgerKind.Closed        => "closed — retirement pending"
      }, fmtInstant(entry.at), entry.evidence)
    }

  /** Cinemas CURRENTLY served by Filmweb because their own scraper is down/empty,
   *  rendered as a section ON the uptime page (below the failing / no-screenings
   *  triage). Recovered cinemas and Filmweb-only-by-design venues are kept in
   *  Mongo but no longer surfaced here. */
  private def activeFallbacks(): Seq[FallbackRow] =
    filmwebFallback.findAll().filter(_.active).sortBy(_.cinema).map(fallbackRow)

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
      queue.offer(barData(service, snapshot))
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
   *  ~10s of img onload/onerror events and POSTs them here so the uptime
   *  page sees per-host image-fetch reliability.
   *
   *  `host` is the ORIGIN CDN (image.tmdb.org, m.media-amazon.com,
   *  de.web.img3.acsta.net …), not the `images.weserv.nl` proxy that fronts
   *  most of them — the tracker unwraps the proxy's `?url=` before posting.
   *  Reporting the proxy host instead merged every CDN into one row, which is
   *  how a total m.media-amazon.com outage stayed invisible behind TMDB's
   *  healthy traffic.
   *
   *  `fallback` marks an attempt at a non-primary poster URL, and gets its own
   *  service row. The two are worth separating: a failed primary is a poster
   *  the visitor watched break, whereas a failed fallback is only a dead spare
   *  behind a poster that may have rendered fine. Merging them would let a
   *  broken spare mask — or fake — a user-visible outage.
   *
   *  `proxied` says the browser asked weserv for the image rather than the
   *  origin, and gives the PROXY a row of its own without recreating the
   *  collapsed one. It feeds that row successes only: an `<img>` error carries
   *  no status, so a proxied failure is as likely to be the origin's fault as
   *  the proxy's, and charging it to the proxy would paint the row red on every
   *  CDN outage. The one failure attributable to weserv — it broke a poster the
   *  origin was willing to serve — can only be established by re-requesting the
   *  image direct, which the tracker does client-side and posts as an ordinary
   *  event against `images.weserv.nl`.
   *
   *  ── ⚠️ THE ONE /uptime ENDPOINT THAT IS NOT ADMIN-GATED, and it has to be ──
   *
   *  Every other route on this controller is an operator READING internal
   *  state. This one is every VISITOR'S BROWSER writing the telemetry the rows
   *  above are made of — a CDN outage is only observable from the machines that
   *  tried to load the posters, which are the public's, not ours.
   *
   *  It was swept behind `adminAction` on 2026-06-12 along with the pages
   *  (20c521c4, "/uptime (+ its fallback/stream/img-event)"), which quietly did
   *  two things: the `img:` rows stopped being fed by anyone but a signed-in
   *  admin's own browser, so the feature was dead for three months; and the
   *  beacon became a permanent 401 stream that on 2026-09-04 was 100% of a
   *  25% error share on the application-health dashboard while not one request
   *  was actually failing. See web-errors.rules for the alerting that came out
   *  of that.
   *
   *  What replaced the gate as the bound on an anonymous write:
   *  [[ImageOriginRoster]] caps the number of rows a caller can create,
   *  `MaxEventsPerBeacon` caps one request's records, and the body parser caps
   *  its bytes. What is deliberately NOT defended against is a caller
   *  fabricating plausible events for a real origin — an unauthenticated
   *  writer can make a poster CDN look worse than it is, and that is the price
   *  of measuring from the only place the measurement exists. */
  def imgEvent: Action[JsValue] = Action(parse.json(maxLength = UptimeController.MaxBeaconBytes)) { request =>
    imageLoads.record((request.body \ "events").asOpt[Seq[JsObject]].getOrElse(Seq.empty))
    NoContent
  }

  private def fmtInstant(i: Instant): String = tsFmt.format(i)

  private def fallbackRow(s: FallbackState): FallbackRow = FallbackRow(
    cinema    = s.cinema,
    source    = s.fallbackSource,
    ref       = s.fallbackRef.getOrElse("—"),
    since     = s.since.map(fmtInstant).getOrElse("—"),
    reason    = s.lastReason.getOrElse("—"),
    fails     = s.consecutiveFailures,
    nextProbe = s.nextPrimaryProbeAt.map(fmtInstant).getOrElse("—"),
    history   = s.history.take(8).map(e => s"${fmtInstant(e.at)} · ${e.event} · ${e.reason}")
  )
}

object UptimeController {
  /** The page's timestamp formats in `zone`. Month names in English whatever the JVM's default
   *  locale: a default-locale "MMM" reads "wrz" under pl_PL and "Sept" under en_GB on the same
   *  English page. */
  private[controllers] final case class Formats(zone: java.time.ZoneId) {
    val timeFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm", Locale.ENGLISH).withZone(zone)
    val dateFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("d MMM", Locale.ENGLISH).withZone(zone)
    val tsFmt:   DateTimeFormatter = DateTimeFormatter.ofPattern("d MMM HH:mm", Locale.ENGLISH).withZone(zone)
  }
  /** Distinguishes a non-primary poster attempt's row from its origin's primary
   *  row (`img: m.media-amazon.com · fallback`). Mirrors the `·` separator the
   *  page already uses for a triage row's cinema/city hover title. */
  val FallbackSuffix = " · fallback"

  /** The image proxy's own row. Named like any other origin row so a
   *  proxy-fault event the browser posts (host = `images.weserv.nl`) lands in
   *  the same place as the proxied successes fanned out here. */
  val ProxyService = s"img: ${tools.PosterProxy.ProxyHost}"

  /** One beacon's worth of events. The tracker flushes at 50
   *  (`BATCH_SIZE_TRIGGER` in shared.js) or every 10s, so a page loading 200
   *  posters posts several batches rather than one large one; this is well
   *  clear of the real shape and stops a single request from doing unbounded
   *  work on the monitor. */
  val MaxEventsPerBeacon = 500

  /** Body cap for the same request. 50 events of `{host, success, error}` is a
   *  few KB, and a 64 KB ceiling refuses (413) anything that is not that.
   *  Play's default `parse.json` limit is application-wide; this endpoint is
   *  the only unauthenticated writer here, so it states its own. */
  val MaxBeaconBytes = 64 * 1024
}
