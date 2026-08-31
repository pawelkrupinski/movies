package controllers

import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import play.api.mvc._
import models.Cinema
import services.UptimeMonitor
import services.UptimeMonitor._
import services.fallback.{FallbackState, FallbackStore}

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UptimeController(cc: ControllerComponents, adminAction: AdminAction, monitor: UptimeMonitor, filmwebFallback: FallbackStore, country: models.Country)(using mat: Materializer) extends AbstractController(cc) {

  // The city groups this deployment can render, scoped to the ONE country it
  // serves. `Cinema.byCity` spans every country at once, and rows are matched to
  // a city BY DISPLAY NAME — so an unscoped grouping put any cinema whose name is
  // shared across rosters under a foreign city too. "Studio" names a cinema in
  // both Opole and Germany, which is how the German page grew an "Opole" header
  // (and rendered that one German cinema twice).
  private val byCity: Seq[(String, Seq[Cinema])] =
    country.cities.map(c => c.labels.nominative -> c.cinemas)

  // Derived from the cinema model so every wired scraper groups under the
  // "Cinemas" header automatically — no edit needed when a cinema is added.
  // Scoped with `byCity` above: a service that ISN'T one of this country's
  // cinemas must fall through to "Other" and stay visible, rather than being
  // excluded here and dropped from the page entirely.
  private val cinemaNames = byCity.flatMap(_._2).map(_.displayName)

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

  // Above this many HEALTHY cinema rows the by-city section is dropped and only
  // its count is reported. One uptime service per venue means the section scales
  // with the roster, and each rendered row carries all 96 of its 15-min slots:
  // the US (5,031 venues) is ~484k bar objects plus the same again as HTML and
  // once more as the page's SSE bootstrap JSON. That OOM-killed the US web pod on
  // 2026-08-31 — and because `pekko.jvm-exit-on-fatal-error` exits the JVM rather
  // than limping, an admin opening /uptime took the PUBLIC site down with it.
  // Nothing is lost by hiding them: the triage sections above already carry every
  // row that is failing or returning nothing, which is what the page is read for.
  // 500 keeps Poland (~60) and the UK (343) rendering as before; Germany (1,529)
  // and the US collapse.
  private val MaxHealthyCinemaRows = 500
  private sealed trait Health
  private case object Failing extends Health
  // Red, but for a reason no one can act on: the venue's page is a 404. Split out
  // of Failing so the section that means "something broke today" stays readable.
  // Named `Missing`, not `Gone`, for the same reason `Healthy` is not `Ok`: Play's
  // `Results` already has both `Gone` and `NotFound` and a controller inherits
  // them. The section it feeds is still called "gone upstream".
  private case object Missing extends Health
  private case object Zero    extends Health
  private case object Healthy extends Health

  private val warsawZone = ZoneId.of("Europe/Warsaw")
  private val timeFmt = DateTimeFormatter.ofPattern("HH:mm").withZone(warsawZone)
  private val dateFmt = DateTimeFormatter.ofPattern("d MMM").withZone(warsawZone)

  /** One rendered bar, with its bucket window stamped in Warsaw time. Shared by
   *  the full-page render and the live SSE feed so both label a bucket the same. */
  private def barData(service: String, bucketTimestamp: Long, status: String,
                      successes: Int, failures: Int, zeroes: Int, errors: Seq[String],
                      fallback: Boolean = false): BarData = {
    val from = Instant.ofEpochMilli(bucketTimestamp)
    val to   = Instant.ofEpochMilli(bucketTimestamp + BucketDurationMs)
    BarData(service, bucketTimestamp, timeFmt.format(from), timeFmt.format(to), dateFmt.format(from),
      status, successes, failures, zeroes, errors, fallback)
  }

  def index: Action[AnyContent] = adminAction {
    val now = System.currentTimeMillis()
    val currentBucket = bucketTimestamp(now)
    val slots = (0 until MaxBuckets).reverse.map(i => currentBucket - i * BucketDurationMs)
    val active = monitor.services

    def barsFor(serviceName: String): Seq[BarData] = {
      val history = monitor.history(serviceName).map(b => b.timestamp -> b).toMap
      slots.map { timestamp =>
        history.get(timestamp) match {
          case Some(b) => barData(serviceName, timestamp, b.status, b.successes, b.failures, b.zeroes, b.errors, b.fallback)
          case None    => barData(serviceName, timestamp, "empty", 0, 0, 0, Seq.empty)
        }
      }
    }

    val tags = monitor.serviceTagsSnapshot()
    def row(n: String) =
      ServiceRow(n, barsFor(n), monitor.averageMs1h(n), monitor.averageMsTotal(n), tags = tags.getOrElse(n, Set.empty))
    val sections = groupRows(active, monitor.recentStatuses(_, RecentScrapes),
      monitor.recentErrors(_, RecentScrapes), row)
    Ok(views.html.uptime(sections.failing, sections.gone, sections.zero, activeFallbacks(), sections.cinemasByCity,
      sections.services, sections.other, sections.hiddenHealthyCinemas))
  }

  /** Cinemas CURRENTLY served by Filmweb because their own scraper is down/empty,
   *  rendered as a section ON the uptime page (below the failing / no-screenings
   *  triage). Recovered cinemas and Filmweb-only-by-design venues are kept in
   *  Mongo but no longer surfaced here. */
  private def activeFallbacks(): Seq[FallbackRow] =
    filmwebFallback.findAll().filter(_.active).sortBy(_.cinema).map(fallbackRow)

  /** Split the active services into the page's sections. Two triage sections lead:
   *  rows that have been FAILING (every one of their last ≤3 active 15-min buckets
   *  is fully red) and rows returning NO SCREENINGS (last ≤3 active buckets all
   *  white) are pulled to the top — cinemas and enrichment services together, in a
   *  flat list tagged with each row's city — so a glance shows what's broken now.
   *  Everything healthy keeps its normal home: cinemas grouped by city, the
   *  enrichment-service rows, then "Other". A cinema's "<cinema>|enrichment"
   *  sub-row travels with it (and a failing sub-row promotes its cinema, keeping
   *  cinema + enrichment together). Package-private and parameterised on both
   *  lookups so the grouping is unit-testable without rendering or a Materializer.
   *
   *  The two lookups are SEPARATE on purpose. `statusesOf` decides every row's
   *  section and is asked for all of them; `row` builds the 96-slot bar series and
   *  is asked ONLY for rows that survive into a rendered section. Collapsing them
   *  back into one is what made the page cost one bar series per registered
   *  service — see MaxHealthyCinemaRows. */
  private[controllers] def groupRows(active: Set[String], statusesOf: String => Seq[String],
                                     errorsOf: String => Seq[String],
                                     row: String => ServiceRow): UptimeSections = {
    def cinemaRow(displayName: String): ServiceRow = {
      val enrichService = UptimeMonitor.enrichmentService(displayName)
      row(displayName).copy(enrichment = Option.when(active.contains(enrichService))(row(enrichService)))
    }

    // Classification runs over NAMES and statuses only — never over built rows.
    // A cinema unit is the WORST of its scrape row and its enrichment sub-row, so
    // a failing enrichment surfaces the cinema too (keeping the pair together).
    def cinemaHealthOf(displayName: String): Health = {
      val enrichService = UptimeMonitor.enrichmentService(displayName)
      val enrichHealth =
        if (active.contains(enrichService)) verdict(enrichService) else Healthy
      worse(verdict(displayName), enrichHealth)
    }

    // Errors are read ONLY for a row that already classified as Failing — the
    // question "is this a 404" is meaningless otherwise, and asking it for all
    // 5,031 US services would undo the point of classifying over names alone.
    def verdict(service: String): Health = classify(statusesOf(service)) match {
      case Failing if goneUpstream(errorsOf(service)) => Missing
      case other                                      => other
    }

    // Names in city order, each with its verdict. Reused by both the triage split
    // and the by-city section so a cinema's placement is decided exactly once.
    val cinemaVerdicts: Seq[(String, String, Health)] = byCity.flatMap { case (city, venues) =>
      venues.map(_.displayName).filter(active.contains).map(dn => (city, dn, cinemaHealthOf(dn)))
    }
    val serviceVerdicts = enrichmentNames.filter(active.contains).map(n => n -> verdict(n))
    // Exclude the "<cinema>|enrichment" services — they render as cinema sub-rows.
    val otherVerdicts = (active -- cinemaNames.toSet -- enrichmentNames.toSet)
      .filterNot(UptimeMonitor.isEnrichmentService).toSeq.sorted.map(n => n -> verdict(n))

    // Flat triage sections, in the order cinemas → enrichment services → other,
    // each carrying the city it belongs to (None for non-cinema rows).
    def flaggedAs(verdict: Health): Seq[FlaggedRow] =
      cinemaVerdicts.collect { case (city, dn, h) if h == verdict => FlaggedRow(cinemaRow(dn), Some(city)) } ++
        serviceVerdicts.collect { case (n, h) if h == verdict => FlaggedRow(row(n), None) } ++
        otherVerdicts.collect { case (n, h) if h == verdict => FlaggedRow(row(n), None) }

    // The remainder (healthy / mixed) keeps its normal home — up to a point. Past
    // MaxHealthyCinemaRows the by-city section is DROPPED rather than rendered:
    // see the constant for why a US-sized roster cannot be put on the page.
    val healthyCinemas = cinemaVerdicts.collect { case (city, dn, Healthy) => city -> dn }
    val collapsed = healthyCinemas.sizeIs > MaxHealthyCinemaRows
    val cinemasByCity =
      if (collapsed) Seq.empty
      else byCity.flatMap { case (city, _) =>
        val rows = healthyCinemas.collect { case (c, dn) if c == city => cinemaRow(dn) }
        Option.when(rows.nonEmpty)(city -> rows)
      }

    UptimeSections(
      failing = flaggedAs(Failing),
      gone = flaggedAs(Missing),
      zero = flaggedAs(Zero),
      cinemasByCity = cinemasByCity,
      services = serviceVerdicts.collect { case (n, Healthy) => row(n) },
      other = otherVerdicts.collect { case (n, Healthy) => row(n) },
      hiddenHealthyCinemas = if (collapsed) healthyCinemas.size else 0
    )
  }

  /** Classify a bar series by its last `RecentScrapes` buckets that recorded any
   *  activity (ignoring untouched "empty" slots): all red ⇒ Failing, all white
   *  ⇒ Zero, anything else (any success, or no activity at all) ⇒ Healthy. A lone
   *  red blip among greens stays Healthy; a brand-new service that has only ever
   *  failed is Failing from its first bucket. Yellow (partial failure) is NOT
   *  failing. */
  private def classify(statuses: Seq[String]): Health = {
    val recent = statuses.iterator.filter(_ != "empty").toSeq.takeRight(RecentScrapes)
    if (recent.isEmpty) Healthy
    else if (recent.forall(_ == "red")) Failing
    else if (recent.forall(_ == "zero")) Zero
    else Healthy
  }

  /** Every recorded error in the window says the page is a 404 — nothing else
   *  broke, the venue simply isn't there. One non-404 among them (a 500, a
   *  timeout, a parse error) makes it an ordinary failure again: that is a venue
   *  whose page EXISTS and is misbehaving, which is worth acting on.
   *
   *  Recency, not duration, is the test here — the buckets only reach back a day.
   *  The worker asks the longer question against the scrape archive
   *  ([[services.scrapes.GoneUpstream]], "404ing for over a day") before it backs
   *  a venue's scrape cadence off; both sides share the one predicate that
   *  matters, `isNotFound`. */
  private def goneUpstream(errors: Seq[String]): Boolean =
    errors.nonEmpty && errors.forall(error => services.scrapes.GoneUpstream.isNotFound(error))

  /** Worst-first, and `Missing` ranks BELOW Failing: a cinema whose page is gone
   *  but whose enrichment is also failing has something live to fix, so it belongs
   *  in Failing. */
  private def worse(a: Health, b: Health): Health =
    if (a == Failing || b == Failing) Failing
    else if (a == Missing || b == Missing) Missing
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
   *  event against `images.weserv.nl`. */
  def imgEvent: Action[JsValue] = adminAction(parse.json) { request =>
    val events = (request.body \ "events").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
    events.foreach { e =>
      val host     = (e \ "host").asOpt[String].getOrElse("unknown")
      val success  = (e \ "success").asOpt[Boolean].getOrElse(false)
      val fallback = (e \ "fallback").asOpt[Boolean].getOrElse(false)
      val proxied  = (e \ "proxied").asOpt[Boolean].getOrElse(false)
      val service  = if (fallback) s"img: $host${UptimeController.FallbackSuffix}" else s"img: $host"
      if (success) {
        monitor.recordSuccess(service)
        // Every poster weserv served, primary or spare, so the proxy's row has
        // green bars and a denominator its failures can be read against.
        if (proxied) monitor.recordSuccess(UptimeController.ProxyService)
      } else {
        val error = (e \ "error").asOpt[String].getOrElse("image load failed")
        monitor.recordFailure(service, error.take(200))
      }
    }
    NoContent
  }

  private val tsFmt = DateTimeFormatter.ofPattern("d MMM HH:mm").withZone(warsawZone)
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

case class BarData(
  service: String,
  bucketTimestamp: Long,
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
    "bucketTs"  -> b.bucketTimestamp,
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

/** The per-bucket detail behind the uptime grid, emitted ONCE for the whole page
 *  instead of inlined into every bar's `data-info` attribute.
 *
 *  The grid renders a complete 96-slot row per service, empty slots included, so
 *  a large roster (Germany registers ~1550 services) means ~149k cells. Carrying
 *  an HTML-escaped JSON blob on each of those built ~57 MB of HTML through nested
 *  Twirl StringBuilders and OOM'd the 384 MB heap — which, with Pekko's
 *  exit-on-fatal-error, took the whole site down on every /uptime load.
 *
 *  Two things make this small. The 96 slot labels are shared by every service, so
 *  they're emitted once rather than once per cell. And only buckets that actually
 *  recorded something get an entry — an absent bucket IS the "no data" case, which
 *  the bar's `empty` class already renders. On the German deployment that's ~3.6k
 *  real buckets against ~149k cells. */
object UptimeBarPayload {

  /** Serialised as `{"slots": {ts: {labels}}, "data": {service: {ts: {counts}}}}`,
   *  safe to drop straight into a `<script type="application/json">` block: `<` is
   *  escaped so an error string containing `</script>` can't break out. */
  def apply(rows: Seq[ServiceRow]): String = {
    val allRows = rows.flatMap(r => r +: r.enrichment.toSeq)

    val slots = allRows.flatMap(_.bars).map { b =>
      b.bucketTimestamp.toString -> Json.obj(
        "timeFrom" -> b.timeFrom, "timeTo" -> b.timeTo, "dateLabel" -> b.dateLabel)
    }.toMap

    val data = allRows.map { row =>
      row.name -> JsObject(row.bars.filter(_.status != "empty").map { b =>
        b.bucketTimestamp.toString -> Json.obj(
          "status"    -> b.status,
          "successes" -> b.successes,
          "failures"  -> b.failures,
          "zeroes"    -> b.zeroes,
          "errors"    -> b.errors,
          "fallback"  -> b.fallback
        )
      })
    }.filter(_._2.value.nonEmpty).toMap

    Json.stringify(Json.obj("slots" -> slots, "data" -> data)).replace("<", "\\u003c")
  }
}

case class FallbackRow(
  cinema: String, source: String, ref: String, since: String, reason: String,
  fails: Int, nextProbe: String, history: Seq[String]
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

object UptimeController {
  /** Distinguishes a non-primary poster attempt's row from its origin's primary
   *  row (`img: m.media-amazon.com · fallback`). Mirrors the `·` separator the
   *  page already uses for a triage row's cinema/city hover title. */
  val FallbackSuffix = " · fallback"

  /** The image proxy's own row. Named like any other origin row so a
   *  proxy-fault event the browser posts (host = `images.weserv.nl`) lands in
   *  the same place as the proxied successes fanned out here. */
  val ProxyService = s"img: ${tools.PosterProxy.ProxyHost}"
}

/** A row promoted into the leading "Failing" / "No screenings" triage sections,
 *  carrying the city it was pulled out of (None for non-cinema rows). */
case class FlaggedRow(row: ServiceRow, city: Option[String])

/** The /uptime page's sections, as `groupRows` decides them. A named record
 *  rather than a tuple because the sections are not interchangeable and the
 *  hidden-cinema count only makes sense beside the section it explains. */
case class UptimeSections(
  failing: Seq[FlaggedRow],
  // Red, but only ever with a 404: the venue's page no longer exists. Its own
  // section so it stops crowding `failing`, which is the list that means
  // "something broke and someone should look".
  gone: Seq[FlaggedRow],
  zero: Seq[FlaggedRow],
  cinemasByCity: Seq[(String, Seq[ServiceRow])],
  services: Seq[ServiceRow],
  other: Seq[ServiceRow],
  // Healthy cinemas deliberately NOT rendered because the roster is too large.
  // 0 whenever the by-city section is shown in full.
  hiddenHealthyCinemas: Int = 0
)
