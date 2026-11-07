package services.cinemas.us

import models._
import org.jsoup.Jsoup
import org.jsoup.parser.Parser
import play.api.libs.json._
import services.cinemas.common.{CinemaScraper, OnlyMovieEventsFilter, SlotsToMovies}
import services.cinemas.common.ScraperParse.jsonText
import tools.{HttpFetch, HttpRead}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import java.time.temporal.ChronoUnit
import java.util.Locale
import scala.util.matching.Regex

/**
 * A single-screen venue whose programme is a Squarespace EVENTS collection — the
 * site builder many US community cinemas run on (ACME Screening Room in Lambertville
 * lists every screening at `/now-playing`). Flicks may not list such a venue at all
 * (it dropped ACME's page on 2026-08-31 while the cinema kept selling), so the
 * venue's own site is the only source.
 *
 * Every Squarespace collection page answers `?format=json` with the collection the
 * page renders from; an events collection carries
 *
 * {{{
 * "upcoming": [{"title":"2026 LHFF Scream (1996) ","startDate":1792342800572,
 *   "endDate":…,"fullUrl":"/now-playing/scream-1996","assetUrl":"https://images.squarespace-cdn.com/…",
 *   "excerpt":"<p>…</p>","body":"… <a href=\"https://ci.ovationtix.com/35991/production/1293867\">…"}]
 * }}}
 *
 * One request is the venue's whole scrape: `upcoming` is EVERY future event,
 * unpaginated (the `pagination` block pages only `past`), so the horizon is whatever
 * the venue has published — never capped here. `startDate` is an epoch-millis
 * instant, read in the venue's zone; Squarespace stamps a few hundred stray millis
 * on it, cut to the minute.
 *
 * A community venue sells its parties, passes and live shows through the same
 * collection. [[OnlyMovieEventsFilter]] drops what the shared classifier knows;
 * [[NonScreeningMarkers]] names what this kind of programme adds, applied while
 * parsing rather than as `venueEventMarkers`, because the classifier's broadcast veto
 * keeps any title saying "live" — right for "NT Live", wrong for "Live Performance".
 *
 * @param siteUrl    the site's origin, no trailing slash.
 * @param eventsPath the events collection's path (`/now-playing`).
 */
class SquarespaceEventsClient(
  http:       HttpFetch,
  siteUrl:    String,
  eventsPath: String,
  override val cinema: Cinema,
  zone:       ZoneId,
  today:      => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import SquarespaceEventsClient._

  private val pageUrl = s"$siteUrl$eventsPath"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(siteUrl)
  override def sourceUrl: Option[String] = Some(pageUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    parse(HttpRead.page(http, s"$pageUrl$JsonFormat"), siteUrl, cinema, zone, today)
}

object SquarespaceEventsClient {

  val JsonFormat = "?format=json"

  /** What a community venue's events collection sells beside its screenings: a party
   *  ("LHFF Opening Party: The Monster Bash"), a pass to a block of screenings ("3-Block
   *  Pass"), a stage show ("… Live Performance and Q&A"), a private hire, a closure
   *  notice ("CLOSED for Vacation"). Each is anchored on the words such a listing bills
   *  itself with, never on a bare noun a film title could carry ("Hall Pass", "Party"). */
  val NonScreeningMarkers: Seq[Regex] = Seq(
    """\bopening[\s-]+(?:night[\s-]+)?party\b""".r,
    """\bblock[\s-]+pass\b""".r,
    """\blive[\s-]+performance\b""".r,
    """^private[\s-]+event\b""".r,
    """^closed[\s-]+for\b""".r
  )

  def isNonScreening(title: String): Boolean = {
    val t = title.toLowerCase(Locale.ROOT)
    NonScreeningMarkers.exists(_.findFirstIn(t).isDefined)
  }

  private val OvationTixLink = """https?://ci\.ovationtix\.com/\d+/production/\d+""".r

  private case class Slot(event: JsObject, title: String, showtime: Showtime)

  /** The collection's JSON → one [[CinemaMovie]] per titled upcoming event from
   *  `today` on. A body without an `upcoming` array throws: that is not an events
   *  collection (or Squarespace changed its shape), not an empty programme. */
  def parse(json: String, siteUrl: String, cinema: Cinema, zone: ZoneId, today: LocalDate): Seq[CinemaMovie] = {
    val upcoming = (Json.parse(json) \ "upcoming").validate[Seq[JsObject]] match {
      case JsSuccess(events, _) => events
      case _                    => throw new IllegalArgumentException("no 'upcoming' events array — not a Squarespace events collection")
    }
    // A link more than one event carries is a shared one — the festival pass every block
    // links to — so an event's own booking is the link only it carries.
    val linksByEvent = upcoming.map(e => OvationTixLink.findAllIn(jsonText(e \ "body").getOrElse("")).toSeq.distinct)
    val shared       = linksByEvent.flatten.groupBy(identity).collect { case (link, n) if n.size > 1 => link }.toSet

    val slots = for {
      (event, links) <- upcoming.zip(linksByEvent)
      title          <- jsonText(event \ "title").map(clean).filter(_.nonEmpty).filterNot(isNonScreening).toSeq
      when           <- startOf(event, zone).toSeq if !when.toLocalDate.isBefore(today)
    } yield Slot(event, title, Showtime(
      dateTime   = when,
      bookingUrl = links.find(l => !shared.contains(l)).orElse(pageOf(event, siteUrl))
    ))

    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val event = group.head.event
      CinemaMovie(
        movie     = Movie(title = title),
        cinema    = cinema,
        posterUrl = jsonText(event \ "assetUrl"),
        filmUrl   = pageOf(event, siteUrl),
        synopsis  = jsonText(event \ "excerpt").map(html => clean(Jsoup.parse(html).text())).filter(_.nonEmpty),
        cast      = Nil,
        director  = Nil,
        showtimes = showtimes
      )
    }
  }


  private def pageOf(event: JsObject, siteUrl: String): Option[String] =
    jsonText(event \ "fullUrl").map(path => if (path.startsWith("http")) path else s"$siteUrl$path")

  private def startOf(event: JsObject, zone: ZoneId): Option[LocalDateTime] =
    (event \ "startDate").asOpt[Long].map(ms =>
      LocalDateTime.ofInstant(Instant.ofEpochMilli(ms), zone).truncatedTo(ChronoUnit.MINUTES))

  /** Squarespace HTML-escapes titles ("Comedy &amp; Dark Humor") and spaces them with
   *  `&nbsp;`; both become plain text and single spaces. */
  private def clean(s: String): String =
    Parser.unescapeEntities(s, false).replaceAll("[\\s\\u00a0]+", " ").trim
}
