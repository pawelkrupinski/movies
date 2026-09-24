package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import play.api.libs.json.{JsObject, Json}
import services.cinemas.common.{CinemaScraper, ScrapeHorizon, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.util.Try

/**
 * The cinema run by Janowski Ośrodek Kultury (JOK), Janów Lubelski. It's a
 * WordPress site on "The Events Calendar" plugin, with a dedicated `kino`
 * event category — and, usefully, a JSON REST endpoint for it
 * (`wp-json/tribe/events/v1/events?categories=kino`), so no HTML parsing is
 * needed. One request covers the whole [[ScrapeHorizon.MaxDays]] window.
 *
 * Each JSON event is a multi-day RUN at one time of day, not one row per
 * showing — `start_date`/`end_date` span the whole run
 * ("2026-10-02 16:00:00" → "2026-10-04 16:00:00" for a film shown at 16:00 on
 * the 2nd, 3rd AND 4th). The showtime detail also lives as loosely-formatted
 * Polish prose inside `description` (day + roman-numeral month, "02 X
 * 16:00"), but every run observed keeps the SAME time of day across its
 * span, so [[ScraperParse.dailyRange]] — expanding `start_date`'s time across
 * every day up to `end_date`'s date — reads the same showtimes without a
 * roman-numeral parser.
 *
 * Tickets are sold at the door only (the JSON `website` field, where an
 * online ticket link would live, is empty on every event checked), so no
 * booking URL is set.
 */
class KinoJOKClient(
  http:                HttpFetch,
  override val cinema: Cinema    = KinoJOK,
  today:               LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoJOKClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoJOKClient.EventsPageUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoJOKClient.parse(http.get(KinoJOKClient.apiUrl(today, today.plusDays(ScrapeHorizon.MaxDays.toLong))), cinema)
}

object KinoJOKClient {

  val BaseUrl       = "https://jokjanow.pl"
  val EventsPageUrl = s"$BaseUrl/wydarzenia/kategoria/kino/"

  def apiUrl(from: LocalDate, to: LocalDate): String =
    s"$BaseUrl/wp-json/tribe/events/v1/events?start_date=$from&end_date=$to&categories=kino&per_page=100"

  private val DateTimeFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private case class RawSlot(title: String, dateTime: LocalDateTime, filmUrl: Option[String])

  def parse(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    // A body that isn't JSON (a maintenance page, a proxy's HTML) is a failed read and
    // throws; folded into `{}` it read as a venue with nothing on.
    val root   = Json.parse(json)
    val events = (root \ "events").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
    val slots  = events.flatMap(parseEvent)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def parseEvent(ev: JsObject): Seq[RawSlot] = {
    val title = (ev \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
    val url   = (ev \ "url").asOpt[String].filter(_.nonEmpty)
    val range = for {
      startStr <- (ev \ "start_date").asOpt[String]
      endStr   <- (ev \ "end_date").asOpt[String]
      start    <- Try(LocalDateTime.parse(startStr, DateTimeFmt)).toOption
      end      <- Try(LocalDateTime.parse(endStr, DateTimeFmt)).toOption
    } yield ScraperParse.dailyRange(start.toLocalDate, end.toLocalDate, start.toLocalTime)

    for {
      t   <- title.toSeq
      dts <- range.toSeq
      dt  <- dts
    } yield RawSlot(t, dt, url)
  }
}
