package services.cinemas.pl

import tools.HttpFetch
import models._
import play.api.libs.json._
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Kino Centrum CSW (Toruń) — the film screen of Centrum Sztuki Współczesnej
 * Znaki Czasu. The site was rebuilt on Nuxt (2026-09, between the 09-15 and
 * 09-17 scrapes) and the old static `/repertuar/` HTML page (dpProEventCalendar
 * `div.box` markup) is gone — the new cinema page renders its schedule
 * client-side from a `is-loading` placeholder. Rather than run a headless
 * browser, this client calls the same JSON endpoint the page's widget calls:
 * the WordPress REST route `csw/v1/cinema-repertoire` on the site's API
 * subdomain, which returns the full flat list of upcoming screenings —
 * `[{id, filmId, title, date, time, ticketUrl, href, bilety24:{titleId,eventId}}, …]`
 * — no date-window parameter needed; it always returns everything currently
 * published, so unlike the old HTML page this client needs no `today` for
 * year inference.
 */
class KinoCentrumCswClient(
  http:             HttpFetch,
  override val cinema: Cinema
) extends CinemaScraper {

  import KinoCentrumCswClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiUrl, BaseUrl)
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/kino/kino-w-centrum/")

  def fetch(): Seq[CinemaMovie] = {
    val slots = parseRepertoire(http.get(ApiUrl))

    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, bookingUrl = s.ticketUrl),
      distinctBy = _.dateTime
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = Some(s"$BaseUrl${group.head.href}"),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoCentrumCswClient {

  val BaseUrl = "https://csw.torun.pl"
  val ApiUrl  = "https://api.csw.torun.pl/wp-json/csw/v1/cinema-repertoire"

  private[cinemas] case class RawSlot(
    title:      String,
    href:       String,
    ticketUrl:  Option[String],
    dateTime:   LocalDateTime
  )

  /** One entry → a `RawSlot`, or dropped individually on a malformed
   *  `title`/`href`/`date`/`time` rather than failing the whole response. A body
   *  that isn't a JSON array at all (an error page, a WordPress error object)
   *  THROWS — that is a failed read, not an empty repertoire, which `[]` is. */
  private[cinemas] def parseRepertoire(raw: String): Seq[RawSlot] =
    Json.parse(raw).as[JsArray].value.toSeq.flatMap { s =>
      for {
        title <- (s \ "title").asOpt[String]
        href  <- (s \ "href").asOpt[String]
        date  <- (s \ "date").asOpt[String].flatMap(d => Try(LocalDate.parse(d)).toOption)
        time  <- (s \ "time").asOpt[String].flatMap(t => Try(java.time.LocalTime.parse(t)).toOption)
      } yield RawSlot(title, href, (s \ "ticketUrl").asOpt[String], LocalDateTime.of(date, time))
    }
}
