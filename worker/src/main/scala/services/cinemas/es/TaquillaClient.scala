package services.cinemas.es

import models.{Cinema, CinemaMovie}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, ScrapeHorizon}
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, ZoneId}

/**
 * taquilla.com (Spain) — a ticket aggregator that lists the chain multiplexes
 * (Cinesa, Yelmo, mk2, Kinépolis…) whose SensaCine pages carry no showtime
 * block at all. Its venue page (`/<city>/<venue>`) is server-rendered for TODAY
 * only; the date picker reads two JSON endpoints, which are what this reads
 * (plain HTTP, no auth, no cookie; `/json2/` is not disallowed by robots.txt,
 * verified 2026-10-12):
 *
 *   GET /json2/search/calendar?t10venue=<id>&t10type=15&t10num=-1
 *     → `objects: {"2026-10-12": 17, …}` — EVERY day the venue sells, advance
 *       sales months out included (the page's own picker caps it at a week by
 *       passing `t10maxd`; we pass none). See [[ScrapeHorizon]]: never cap it.
 *   GET /json2/search/events?t10venue=<id>&t10d=<day>&t10type=15&t10ptype=2&t10num=-1
 *     → that day's events (films × versions) with their sessions and the chain's
 *       own ticketing link per session, plus the films' catalogue rows.
 *
 * So a scrape is one calendar call plus one call per day sold, and a
 * [[ChunkedCinemaScraper]] over days: a day whose call fails is retried on its
 * own, and the default reduce merges a film's days by its `/entradas/` URL.
 *
 * One instance serves one venue, named by its numeric Taquilla id (the page's
 * `t10venue`) and its page path — see `data/spain/taquilla.json` for which
 * venues and why.
 */
class TaquillaClient(
  http:     HttpFetch,
  venue:    TaquillaVenue,
  override val cinema: Cinema,
  today:    => LocalDate
) extends ChunkedCinemaScraper {

  import TaquillaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  /** A chain multiplex read through an aggregator: chain-run, not independent. */
  override def chain: Boolean = true

  override def sourceUrl: Option[String] = Some(s"$BaseUrl${venue.path}")

  override def chainVenueId: Option[String] = Some(venue.id)

  /** The days the venue sells, up to the far-date sanity bound. A fetch failure
   *  or a body that is not a calendar fails the scrape; an empty calendar is a
   *  venue selling nothing. */
  def planChunks(): Seq[String] = {
    val notAfter = today.plusDays(ScrapeHorizon.MaxDays.toLong)
    TaquillaParser.days(HttpRead.page(http, calendarUrl(venue.id)))
      .getOrElse(throw new IllegalStateException(s"taquilla.com answered venue ${venue.id}'s calendar without one"))
      .filterNot(d => d.isBefore(today) || d.isAfter(notAfter))
      .map(_.toString)
  }

  def fetchChunk(day: String): Seq[CinemaMovie] =
    TaquillaParser.day(HttpRead.page(http, dayUrl(venue.id, day)), cinema)
}

/** A venue on taquilla.com: its numeric id (`t10venue`) and its page path. */
final case class TaquillaVenue(id: String, path: String)

object TaquillaClient {
  val BaseUrl = "https://www.taquilla.com"

  /** Resolves "today" for the day window; Canary venues an hour behind cannot move it. */
  val Zone: ZoneId = models.TimeZones.Spain

  /** `t10type=15` is cinema; `t10num=-1` asks for every row, unpaged. */
  def calendarUrl(venueId: String): String =
    s"$BaseUrl/json2/search/calendar?lang=es&t10venue=$venueId&t10type=15&t10num=-1"

  def dayUrl(venueId: String, day: String): String =
    s"$BaseUrl/json2/search/events?lang=es&t10venue=$venueId&t10d=$day&t10type=15&t10ptype=2&t10num=-1"
}
