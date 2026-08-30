package services.cinemas.us

import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.jsoup.Jsoup
import play.api.libs.json._

import java.time.{LocalDate, OffsetDateTime, ZoneOffset}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Pure HTML/JSON → model transformation for AMC Theatres. No I/O: [[AmcClient]]
 * fetches the bodies and hands them here, so every parse is unit-testable
 * against the recorded fixtures without any HTTP stubbing.
 *
 * Two shapes, because AMC splits the listing across two origins:
 *  - [[parseDates]] reads the venue's own day list out of the `showtimes` PAGE
 *    (`www.amctheatres.com`), the only place AMC publishes which days a given
 *    theatre has a programme on — its GraphQL schema has no theatre-scoped date
 *    field (verified by full introspection 2026-08-30);
 *  - [[parseDay]] reads ONE day's showtimes out of the GraphQL response
 *    (`graph.amctheatres.com`), which is where the actual programme lives.
 */
object AmcParser {

  /** The days this venue advertises a programme on, read off the showtimes
   *  page's `<select name="date">` day picker (`<option value="YYYY-MM-DD">`).
   *
   *  Sparse and long: AMC lists day-by-day for the near term and then only the
   *  individual far-out days advance sales are open on (AMC Empire 25,
   *  2026-08-30: 123 options reaching 2027-09-25, with month-wide gaps in the
   *  tail). That sparseness is exactly why the day list is READ rather than
   *  walked — [[services.cinemas.common.ScrapeHorizon.liveDays]]' consecutive-blank stop rule
   *  would cut the walk at the first month-long gap and silently drop every
   *  advance-sale day past it, which is the failure that cost the UK its whole
   *  advance-sale programme on 2026-07-27.
   *
   *  NOTE the picker also carries a trailing block of ~15 consecutive days about
   *  a year out (2027-09-11..25 on every venue sampled) that answer with NO
   *  showtimes — AMC's picker always offers a fixed forward window regardless of
   *  content. They are left in deliberately: an empty day is valid data that
   *  costs one small GraphQL call, whereas trimming to a fixed horizon is the
   *  cap `ScrapeHorizon` exists to forbid, and would hide a real screening the
   *  day AMC does sell one that far out.
   *
   *  An EMPTY list is expected data (a venue with nothing on), not a failure —
   *  see [[hasDatePicker]] for the line between the two. */
  def parseDates(html: String): Seq[LocalDate] =
    Jsoup.parse(html).select("select[name=date] option").asScala.toSeq
      .flatMap(option => Option(option.attr("value")))
      .flatMap(value => Try(LocalDate.parse(value)).toOption)
      .distinct.sorted

  /** Whether the page carries the day picker at all.
   *
   *  The line `FlicksClient` and `WebediaShowtimesClient` already draw, and it
   *  matters for the same reason: NO picker means we aren't parsing the page we
   *  think we are (markup drift, an error page, a Cloudflare interstitial) and
   *  the scrape must FAIL so the venue keeps its last-known listing; a picker
   *  that is present but names no day means the venue simply has nothing on,
   *  which is DATA and must come back empty. Throwing on the latter left five UK
   *  venues permanently red on /uptime (2026-07-26). */
  def hasDatePicker(html: String): Boolean =
    !Jsoup.parse(html).select("select[name=date]").isEmpty

  /** ONE day's GraphQL response → that day's films.
   *
   *  Shape (`viewer.user.movies(theatreSlug:, date:)`):
   *  {{{
   *  items[].movie   { name slug runTime mpaaRating synopsis genre
   *                    directors starringActors preferredPoster { url } }
   *  items[].theatres[].formats.items[]
   *        .attributes[].name                       ← the format labels
   *        .groups.edges[].node.showtimes.edges[].node
   *              { showtimeId auditorium showDateTimeUtc utcOffset }
   *  }}}
   *
   *  Deterministic: films come out ordered by their AMC slug and each film's
   *  showtimes by start time. Throws on a body we can't parse (only that day's
   *  chunk task reschedules); an items array that is present but EMPTY is a day
   *  with nothing on and yields no films. */
  def parseDay(json: String, cinema: Cinema): Seq[CinemaMovie] =
    items(json).flatMap(movieFrom(_, cinema)).sortBy(_.filmUrl.getOrElse(""))

  private def movieFrom(item: JsValue, cinema: Cinema): Option[CinemaMovie] = {
    val movie = item \ "movie"
    for {
      title <- (movie \ "name").asOpt[String].map(_.trim).filter(_.nonEmpty)
      slug  <- (movie \ "slug").asOpt[String].filter(_.nonEmpty)
      showtimes = showtimesOf(item)
      if showtimes.nonEmpty
    } yield CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = (movie \ "runTime").asOpt[Int].filter(_ > 0),
        // `releaseDateUtc` is AMC's US THEATRICAL release date, so on a
        // re-release ("Cars: 20th Anniversary", "The Hunger Games: Mockingjay -
        // Part 1") it is the re-release year, not the production year. Reading
        // it would poison TMDB resolution exactly as Cineworld's would — a
        // year-scoped search excludes the real film and the fallback picks
        // whoever shares the title. Left None; `MovieService.settle`'s
        // `EmbeddedYear` backfill recovers the year from titles that carry one.
        releaseYear    = None,
        genres         = (movie \ "genre").asOpt[String].toSeq.flatMap(splitList),
        rawTitle       = Some(title)),
      cinema      = cinema,
      posterUrl   = (movie \ "preferredPoster" \ "url").asOpt[String].filter(_.nonEmpty),
      filmUrl     = Some(s"${AmcClient.SiteUrl}/movies/$slug"),
      synopsis    = (movie \ "synopsis").asOpt[String].map(_.trim).filter(_.nonEmpty),
      cast        = (movie \ "starringActors").asOpt[String].toSeq.flatMap(splitList),
      director    = (movie \ "directors").asOpt[String].toSeq.flatMap(splitList),
      showtimes   = showtimes,
      externalIds = Map("amc" -> slug),
      // AMC's listing exposes no trailer on this query; TMDB fills it downstream.
      trailerUrl  = None,
      // MPAA certificate as AMC labels it ("PG", "PG13", "R", "NR"), verbatim.
      ageRating   = (movie \ "mpaaRating").asOpt[String].map(_.trim).filter(_.nonEmpty))
  }

  /** Every screening of one film at one theatre on this day, deduped by
   *  (start, room, format) and time-ordered.
   *
   *  A screening's start arrives as `showDateTimeUtc` ("2026-09-05T18:00:00.000Z")
   *  PLUS the theatre's own `utcOffset` ("-05:00"). We add the offset rather than
   *  convert through a `ZoneId` so the wall-clock time is the one AMC itself
   *  prints for that screening — the offset it ships is already the correct one
   *  for that DATE, which sidesteps getting a DST boundary wrong on a venue
   *  whose zone we would otherwise have to carry and keep in step. */
  private def showtimesOf(item: JsValue): Seq[Showtime] =
    (for {
      theatre <- arrayAt(item \ "theatres")
      format  <- arrayAt(theatre \ "formats" \ "items")
      labels   = arrayAt(format \ "attributes").flatMap(a => (a \ "name").asOpt[String]).map(_.trim).filter(_.nonEmpty)
      group   <- arrayAt(format \ "groups" \ "edges")
      edge    <- arrayAt(group \ "node" \ "showtimes" \ "edges")
      node     = edge \ "node"
      start   <- localStart(node).toSeq
    } yield Showtime(
      dateTime   = start,
      // AMC's own ticketing deep-link. Derived from the id rather than read from
      // a `shareShowtime` sub-selection so one field fewer has to survive a
      // schema change; verified against the page's own `<a href="/showtimes/…">`.
      bookingUrl = (node \ "showtimeId").asOpt[Long].map(id => s"${AmcClient.SiteUrl}/showtimes/$id"),
      room       = (node \ "auditorium").asOpt[Int].map(_.toString),
      format     = labels.toList))
      .distinctBy(s => (s.dateTime, s.room, s.format))
      .sortBy(_.dateTime)

  /** A screening's start in the THEATRE's local wall-clock time. */
  private def localStart(node: JsLookupResult) =
    for {
      utc    <- (node \ "showDateTimeUtc").asOpt[String]
      offset <- (node \ "utcOffset").asOpt[String].flatMap(o => Try(ZoneOffset.of(o)).toOption)
      instant <- Try(OffsetDateTime.parse(utc)).toOption
    } yield instant.withOffsetSameInstant(offset).toLocalDateTime

  /** AMC ships `directors`, `starringActors` and `genre` as ONE comma-separated
   *  string ("JOHN CENA, LANA CONDOR, Will Forte"), so each is split here rather
   *  than stored with the separator baked in. */
  private def splitList(value: String): Seq[String] =
    value.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq.distinct

  /** The day's `items` array. Its ABSENCE is a response we failed to parse and
   *  throws; an empty array is a day with nothing on. */
  private def items(json: String): Seq[JsValue] = {
    val parsed = Try(Json.parse(json)).getOrElse(
      throw new IllegalStateException("AMC response was not JSON"))
    (parsed \ "data" \ "viewer" \ "user" \ "movies" \ "items").asOpt[JsArray]
      .map(_.value.toSeq)
      .getOrElse(throw new IllegalStateException(
        "AMC response carried no `data.viewer.user.movies.items` array"))
  }

  /** An array that may legitimately be absent — a film with no screening in one
   *  of the nested groupings. Takes the LOOKUP rather than a `JsValue` so a
   *  multi-step path (`format \ "groups" \ "edges"`) reads as one expression. */
  private def arrayAt(lookup: JsLookupResult): Seq[JsValue] =
    lookup.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
}
