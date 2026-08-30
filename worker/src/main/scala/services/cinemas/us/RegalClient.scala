package services.cinemas.us

import models.{Cinema, CinemaMovie, RegalChain, Source}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, DetailFetchOutcome,
  FilmDetail, ScrapeHorizon}
import tools.HttpFetch

import java.time.LocalDate
import java.time.format.DateTimeFormatter

/**
 * Regal Cinemas — the United States' largest chain (~400 venues).
 *
 * ⚠ NOT WIRED. This client is complete and fixture-tested, but `www.regmovies.com`
 * is GEO-FENCED to the United States and this worker egresses from Hetzner
 * Helsinki. Measured 2026-08-30: 403 direct, 403 from every Decodo pool IP, and
 * failure from Zyte's FI pool. Zyte's DEFAULT pool does clear it (200 on the real
 * batch endpoint, three for three, `datesWithShows` present) and that is how it was
 * originally wired — but flicks.us already serves every one of these venues, so
 * Zyte here is a per-request cost with a working alternative rather than a last
 * resort. `CinemaScraperCatalog` therefore leaves every Regal venue on flicks.us.
 *
 * Re-wire it the moment a US egress exists — that is the ONLY thing missing.
 * Note the `detailCache-regal` Mongo collection is now orphaned; it carries a TTL
 * and no reader, so it drains on its own.
 *
 * It was moved off the aggregator in the first place because every venue served
 * from its chain origin is a venue removed from the shared flicks.us budget.
 *
 * Regal is Cineworld-owned, so the obvious first guess was that it serves the
 * same `quickbook` data API [[services.cinemas.uk.CineworldClient]] and Cinema
 * City ride on. It does NOT — `/data-api-service/…` 404s on regmovies.com.
 * Regal's site is a Next.js app talking to its own REST endpoint, so this shares
 * no request shape with the Cineworld client and is a separate parser:
 *
 *   GET /api/getShowtimes?theatres=<codes>&date=<M-d-yyyy>&hoCode=&ignoreCache=false&moviesOnly=false
 *     → `datesWithShows[]` (the days these theatres have a programme on) +
 *       `shows[]`, one entry per theatre, each `Film[]` carrying `Title`,
 *       `MasterMovieCode` and `Performances[]` (start time, auditorium,
 *       attributes).
 *   GET /api/Movies?hoCode=<MasterMovieCode>
 *     → that film's detail: `Duration`, `Rating`, `LongDescription`,
 *       `GraphicUrl`, `TrailerUrl`, `Actors`, `Directors`, `Genre*`.
 *
 * ONE REQUEST SERVES MANY VENUES. `theatres=` takes a comma-separated list and
 * the response carries one `shows[]` entry per theatre, so this client asks for
 * its whole [[RegalVenues.batchFor]] BATCH rather than for itself alone. Every
 * venue in a batch therefore builds the byte-identical URL, and the Mongo-backed
 * chain cache `WorkerWiring` wraps this client's `http` in collapses them to ONE
 * upstream fetch per (batch, date) — across worker processes, not just within
 * one JVM. That is what makes the move affordable: ~400 venues x ~60 dates is
 * ~24,000 requests asked one venue at a time, and ~555 asked this way.
 *
 * Batching is LOSSLESS, not a summary — verified 2026-08-30 by comparing venue
 * 1438's slice of its 80-code batch against the response its own single-theatre
 * request returns: the same 17 performances, field for field.
 *
 * Regal's origin is Cloudflare-gated. Verified 2026-08-30, every path (even
 * `/robots.txt`) answers 403 to: plain curl, curl with a full browser header
 * set, the Decodo residential proxy on three ports, AND the JVM `HttpClient`
 * this worker fetches with. Only Zyte's network gets through, so
 * `WorkerWiring` routes this client through the Zyte seam rather than the
 * `flicksFetch` residential proxy the UK chains use.
 */
class RegalClient(
  http:        HttpFetch,
  theatreCode: String,
  override val cinema: Cinema,
  today:       LocalDate
) extends ChunkedCinemaScraper with DetailEnricher {

  import RegalClient._

  /** The batch this venue's requests name — see the class doc. Fixed at
   *  construction so every call builds the same URL. */
  private val batch: Seq[String] = RegalVenues.batchFor(theatreCode)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def chain: Boolean = true

  /** The venue's public page. Regal's canonical URL is
   *  `/theatres/<slug>-<code>` and the slug is NOT derivable from the theatre
   *  code (verified 2026-08-30: the code alone 404s), so there is no link to
   *  offer without a second per-venue slug table that could drift from the
   *  roster. The chain's theatre finder is the honest stable destination. */
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/theatres")

  /** Regal's listing carries only a film's TITLE, so runtime / certificate /
   *  synopsis / cast / director all come from the separate `/api/Movies` fetch
   *  this defers.
   *
   *  `detailGroup` is chain-wide ("regal"): one fetch per film per freshness
   *  window serves all ~400 venues, and `detailTarget` routes the result into
   *  the single [[RegalChain]] slot rather than an arbitrary venue's — the same
   *  shape Cineworld and Cinema City use, and the reason ~400 venues screening
   *  one film store its blurb once rather than 400 times.
   *
   *  `defersTmdbResolution` is false, matching the sibling Cineworld client: a
   *  row resolves immediately off its listing title and the detail merges in
   *  asynchronously. Holding every row until its detail landed would gate ~400
   *  venues' entire listing on the detail queue draining, and the detail's
   *  identity hints (director, opening year) are a refinement here rather than
   *  the only thing that makes a row resolvable. */
  override val detailGroup: String = "regal"
  override def detailTarget: Source = RegalChain
  override def enrichmentServiceOverride: Option[String] = Some("Regal Enrichment")
  override def defersTmdbResolution: Boolean = false

  /** Fetch + parse one film's detail by the `filmUrl` the listing left on the
   *  movie — which for this chain IS the `/api/Movies?hoCode=` URL (see
   *  `RegalParser.parseDay`). None on a transient failure so the task stays
   *  stale and is retried rather than recording an empty result as fresh; a
   *  durable 404/410 escapes so a film that is gone for good gets stamped
   *  instead of retried every tick. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map(RegalParser.parseDetail)

  /** The days to scrape, read off the batch's own index response rather than
   *  guessed as a fixed grid — one cheap (and batch-shared) call names precisely
   *  which days have a programme, so no request is spent on an empty day and no
   *  advertised day is missed.
   *
   *  The list is the UNION over the batch, so a venue plans a few days it has
   *  nothing on. That costs NOTHING: the per-date request it then makes is the
   *  same one its batch-mates make, so it is a cache hit rather than an extra
   *  upstream fetch, and `parseDay` returns empty for a date this theatre has no
   *  row on. Sharing one chunk list across the batch is precisely what keeps
   *  every venue's URLs identical.
   *
   *  Bounded to `[today, today + MaxHorizonDays]` — the shared sanity valve
   *  against a stray far-future date, NOT a coverage target. Regal advertises
   *  well past a fixed window (measured 2026-08-30: 111 distinct dates chain-
   *  wide, reaching 2027-06-09) and every advertised day is fetched.
   *
   *  An index failure (`http.get` throwing) propagates and fails the whole
   *  scrape, which keeps the venue's last-known listing rather than narrowing it
   *  to a guess. An EMPTY list is expected data for a batch with nothing on. */
  def planChunks(): Seq[String] =
    RegalParser.parseDates(http.get(indexUrl(batch)))
      .filter(d => !d.isBefore(today) && !d.isAfter(today.plusDays(MaxHorizonDays.toLong)))
      .map(_.toString)

  /** Fetch + parse ONE date's response, keeping this theatre's slice. A throw
   *  reschedules only this date's chunk task, leaving the venue's other days
   *  alone. A date this theatre simply has nothing on parses to EMPTY — an idle
   *  venue is data, not an outage. */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] =
    RegalParser.parseDay(http.get(dayUrl(batch, LocalDate.parse(dateKey))), theatreCode, cinema)
}

object RegalClient {

  val BaseUrl = "https://www.regmovies.com"

  /** The shared scrape horizon — see [[ScrapeHorizon]] for why it is a sanity
   *  bound rather than a budget. The batch index names exactly which days exist,
   *  so no day is fetched blind and this only trims a garbage far-future date. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  /** Regal's `date=` parameter is US-style `M-d-yyyy` with NO zero padding
   *  ("9-12-2026", not "09-12-2026") — the format its own front end sends. */
  private val DateParam = DateTimeFormatter.ofPattern("M-d-yyyy")

  /** Every `getShowtimes` URL carries the same trailing parameters: no film
   *  filter (`hoCode=`), no cache bypass, and the full payload rather than the
   *  title-only `moviesOnly` projection (which carries no film ids, so it is no
   *  use for joining detail). */
  private val Tail = "&hoCode=&ignoreCache=false&moviesOnly=false"

  private def theatres(batch: Seq[String]): String = batch.mkString(",")

  /** The batch's index call: no date, which answers with `datesWithShows` — the
   *  union of the days these theatres have a programme on. */
  def indexUrl(batch: Seq[String]): String =
    s"$BaseUrl/api/getShowtimes?theatres=${theatres(batch)}&date=$Tail"

  /** One date's showtimes for the whole batch. */
  def dayUrl(batch: Seq[String], date: LocalDate): String =
    s"$BaseUrl/api/getShowtimes?theatres=${theatres(batch)}&date=${DateParam.format(date)}$Tail"

  /** One film's detail, keyed by its `MasterMovieCode`. Doubles as the film's
   *  `filmUrl` — see `RegalParser.parseDay`. */
  def filmDetailUrl(hoCode: String): String = s"$BaseUrl/api/Movies?hoCode=$hoCode"
}
