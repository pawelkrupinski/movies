package services.cinemas.us

import models.{Cinema, CinemaMovie}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, ScrapeHorizon}
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}

/**
 * AMC Theatres — the largest US chain (523 theatres on its own roster, 519 of
 * which we carry). Moved off the `flicks.us` aggregator and onto AMC's own
 * origins, because every venue served from its chain origin is a venue removed
 * from the shared flicks.us budget: that host throttles per Cloudflare ZONE at
 * ~3-5 req/s for our whole process no matter the egress IP, and the US roster
 * (5,031 venues) sits right at that ceiling.
 *
 * TWO origins, because AMC splits the listing:
 *
 *   GET  https://www.amctheatres.com/movie-theatres/<market>/<theatre>/showtimes
 *     → the venue page, whose `<select name="date">` is the ONLY place AMC
 *       publishes which days THIS theatre has a programme on. Fetched once per
 *       scrape to plan the chunks.
 *
 *   POST https://graph.amctheatres.com/
 *     → `viewer.user.movies(theatreSlug:, date:)`, one day's programme, with the
 *       film's name/runtime/MPAA rating/synopsis/genre/directors/cast/poster and
 *       each screening's auditorium, format attributes and UTC start + offset.
 *
 * The GraphQL endpoint takes unauthenticated POSTs and has introspection
 * enabled; there is no partner key involved (AMC's old `api.amctheatres.com`
 * vendor-key REST API is a different, unused thing). A full introspection on
 * 2026-08-30 confirmed `movies` takes a SINGLE `date` scalar — there is no
 * range/`from`-`to`/`days=N` argument anywhere in the schema — hence
 * [[ChunkedCinemaScraper]], one call per advertised day, exactly as Cineworld's
 * per-date `film-events` calls work.
 *
 * WHY THE DAY LIST COMES FROM THE HTML PAGE AND NOT THE API: the schema's only
 * date-list field is `viewer.selectableDates(movieSlug:)`, which is MOVIE-scoped
 * (no theatre argument) and answers with a short rolling window. The venue
 * page's picker is the authoritative per-theatre list. Reading it costs one
 * ~1 MB page per venue per scrape against ~14 KB per day of JSON, which is the
 * cheaper half of the trade: guessing the days instead would either cap the
 * horizon or walk it blind, and both lose the sparse advance-sale tail.
 *
 * HORIZON, MEASURED (2026-08-30, five venues, AMC's picker vs the same venue's
 * flicks.us `data-date` tabs). AMC's day set was a strict SUPERSET on every one
 * — there was no date flicks advertised that AMC did not, and AMC's extra days
 * were confirmed to carry real screenings:
 * {{{
 *   venue                     AMC days / last        flicks days / last
 *   amc-town-center-20        124 / 2027-09-24        97 / 2027-06-09
 *   amc-empire-25             123 / 2027-09-25        91 / 2027-06-09
 *   amc-classic-albany-16      63 / 2026-12-23        51 / 2026-12-23
 *   amc-dine-in-webb-gin-11   105 / 2027-09-18        54 / 2027-01-06
 *   amc-classic-ardmore-8      28 / 2027-09-18        19 / 2026-09-17
 * }}}
 * That check is the precondition for making a chain client PRIMARY at all: a
 * shorter-horizon primary silently deletes the advance-sale tail on every
 * complete scrape, because `MovieCache`'s prune reads a film's absence from a
 * listing as "it stopped screening".
 *
 * One instance serves one venue — its AMC `theatreSlug` (e.g. `amc-town-center-20`)
 * plus the `marketSlug` its public URL sits under and the [[Cinema]] it feeds,
 * mirroring [[services.cinemas.common.FlicksClient]]. `docs/venue-maps/AMC-VENUE-MAP.tsv` pairs every
 * venue with its AMC ids. Parsing lives in [[AmcParser]].
 */
class AmcClient(
  http:        HttpFetch,
  marketSlug:  String,
  theatreSlug: String,
  override val cinema: Cinema,
  today:       Option[LocalDate] = None
) extends ChunkedCinemaScraper {

  import AmcClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(SiteUrl, GraphUrl)
  override def chain: Boolean = true

  /** The venue's public page — the same URL `planChunks` reads its day list
   *  from, so the /uptime link and the scrape cannot drift. */
  override def sourceUrl: Option[String] = Some(venueUrl(marketSlug, theatreSlug))

  // An absent `today` means the MARKET's current calendar day, not the JVM's: a
  // worker in Europe planning US venues must not start from a date those venues
  // have not reached. Resolved in the body rather than as a default argument
  // because a Scala default cannot read an earlier parameter of the same list.
  private val referenceDay: LocalDate = today.getOrElse(LocalDate.now(MarketZone))

  /** The days to scrape, read off the venue page's own day picker — the exact
   *  days AMC advertises, so no request is spent on a day the venue never
   *  listed and no advertised day is missed. Bounded to
   *  `[today, today + MaxHorizonDays]` so a stray far-future option cannot
   *  balloon the per-venue chunk fan-out.
   *
   *  The page is the ONLY source of days — there is no fixed-grid fallback. A
   *  fetch failure propagates and fails the whole scrape (recorded as a normal
   *  scrape outcome), which keeps the venue's last-known listing rather than
   *  narrowing it to a guessed window.
   *
   *  A page that fetched fine but names no day is split in two, the line
   *  [[services.cinemas.common.FlicksClient]] draws:
   *   - NO `<select name="date">` at all → we aren't parsing the page we think
   *     we are. A FAILURE, so it throws.
   *   - picker present, no day options → the venue simply has nothing on.
   *     EXPECTED DATA, so it returns empty. */
  def planChunks(): Seq[String] = {
    val html  = http.get(venueUrl(marketSlug, theatreSlug))
    val dates = AmcParser.parseDates(html)
      .filter(d => !d.isBefore(referenceDay) && !d.isAfter(referenceDay.plusDays(MaxHorizonDays.toLong)))
    if (dates.isEmpty && !AmcParser.hasDatePicker(html))
      throw new IllegalStateException(
        s"AMC venue page for '$theatreSlug' carried no date picker")
    dates.map(_.toString)
  }

  /** Fetch + parse ONE day's programme. The POST THROWS on failure so ONLY that
   *  day's chunk task reschedules (the per-day retry); the other days are
   *  unaffected. A day that ANSWERS with an empty `items` array is a valid empty
   *  result, not a failure. */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] =
    AmcParser.parseDay(
      http.post(GraphUrl, showtimesQuery(theatreSlug, LocalDate.parse(dateKey)), "application/json"),
      cinema)
}

object AmcClient {

  val SiteUrl  = "https://www.amctheatres.com"
  val GraphUrl = "https://graph.amctheatres.com/"

  /** The zone `referenceDay` falls back to when no `today` is injected. AMC is a
   *  US chain spanning six zones; Eastern is the EARLIEST of them, so starting
   *  from its calendar day can never skip a day a more-western venue is still
   *  on. The per-venue day list then names the real days regardless. */
  private val MarketZone = ZoneId.of("America/New_York")

  /** The shared scrape horizon — see [[ScrapeHorizon]] for why it is a sanity
   *  bound rather than a budget, and what capping it cost. AMC's picker names
   *  exactly which days exist, so no day is fetched blind; this only stops a
   *  stray far-future option fanning a venue out. Deliberately the SAME number
   *  the other chain clients use, so a venue's primary and its flicks fallback
   *  cover one window and neither prunes the other's tail. */
  val MaxHorizonDays: Int = ScrapeHorizon.MaxDays

  /** A venue's public showtimes page, and the day-list source. */
  def venueUrl(marketSlug: String, theatreSlug: String): String =
    s"$SiteUrl/movie-theatres/$marketSlug/$theatreSlug/showtimes"

  /** One day's programme at one theatre.
   *
   *  Sent as a literal query rather than GraphQL variables so the request body
   *  is a pure function of (theatre, date) — `FakeHttpFetch` keys a recorded
   *  POST fixture by the body's hash, so a stable body is what makes the day
   *  replayable in a spec at all. */
  def showtimesQuery(theatreSlug: String, date: LocalDate): String =
    s"""{"query":"{viewer{user{movies(theatreSlug:\\"$theatreSlug\\",date:\\"$date\\")""" +
      """{items{movie{name slug runTime mpaaRating synopsis genre directors starringActors""" +
      """ releaseDateUtc preferredPoster{url}} theatres{theatre{slug name} formats{date""" +
      """ items{attributes{name} groups{edges{node{showtimes{edges{node{showtimeId auditorium""" +
      """ showDateTimeUtc utcOffset status}}}}}}}}}}}}}}"}"""
}
