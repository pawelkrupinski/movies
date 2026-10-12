package services.cinemas.common

import models._
import play.api.libs.json._
import tools.{HttpFetch, HttpRead}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import scala.util.Try

/**
 * A venue whose programme is a Wix Events list on its own Wix site — a small house
 * that screens a film an evening a week and sells the tickets itself, so no
 * aggregator lists it (Steinhaus, Bautzen).
 *
 * Wix server-renders the events widget's data into the page, in the
 * `<script type="application/json" id="wix-warmup-data">` block: under
 * `appsWarmupData.<app>.<widget>.events.events`, one object per event with its
 * `title`, its `location` (name, `fullAddress.postalCode`) and
 * `scheduling.config.startDate` — a UTC instant, read in the event's
 * `timeZoneId`.
 *
 * One list can hold more than the venue's own evenings: Steinhaus lists a partner
 * club's "Herbstkino" in Bischofswerda beside its own. An event counts only when
 * its location's postcode is the venue's.
 *
 * A house labels its film evenings ("KINO: Vaterland", "KINO-Extra: Vier Minus
 * Drei"); that label goes, so the title is the film's ([[filmTitle]]).
 */
class WixEventsClient(
  http: HttpFetch,
  page: String,
  postcode: String,
  override val cinema: Cinema,
  /** The day the listing is read from, asked per scrape: today at the venue. */
  today: => LocalDate
) extends CinemaScraper {
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(page)

  override def sourceUrl: Option[String] = Some(page)

  def fetch(): Seq[CinemaMovie] = {
    val from = today
    WixEventsClient.parse(HttpRead.page(http, page), postcode, cinema)
      .map(m => m.copy(showtimes = m.showtimes.filterNot(_.dateTime.toLocalDate.isBefore(from))))
      .filter(_.showtimes.nonEmpty)
  }
}

object WixEventsClient {
  private val WarmupData = """(?s)<script[^>]*id="wix-warmup-data"[^>]*>(.*?)</script>""".r

  /** "KINO: ", "KINO-Extra: ", "Kino: " in front of the film's title. */
  private val EveningLabel = """(?i)^kino(?:-[\p{L}]+)?\s*:\s*""".r

  private val Fallback = ZoneId.of("Europe/Berlin")

  private final case class Slot(title: String, dateTime: LocalDateTime)

  def filmTitle(title: String): String = EveningLabel.replaceFirstIn(title.trim, "").trim

  /** The page's events at `postcode`, one row per title. A page with no events data
   *  is a broken page, not an empty programme: it throws. */
  def parse(html: String, postcode: String, cinema: Cinema): Seq[CinemaMovie] = {
    val data = WarmupData.findFirstMatchIn(html).map(_.group(1))
      .getOrElse(throw new IllegalStateException("no wix-warmup-data block: not a Wix page any more"))
    val lists = (Json.parse(data) \\ "events").collect { case JsArray(items) => items.toSeq }.toSeq
    if (lists.isEmpty) throw new IllegalStateException("no Wix Events widget data on the page")
    val events = lists.flatten.filter(e => (e \ "scheduling").isDefined)
    val slots = events.filter(e => (e \ "location" \ "fullAddress" \ "postalCode").asOpt[String].contains(postcode))
      .flatMap(slot)
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def slot(event: JsValue): Option[Slot] = {
    val config = event \ "scheduling" \ "config"
    for {
      raw   <- (event \ "title").asOpt[String]
      title  = filmTitle(raw) if title.nonEmpty
      if !(config \ "scheduleTbd").asOpt[Boolean].contains(true)
      start <- (config \ "startDate").asOpt[String].flatMap(s => Try(Instant.parse(s)).toOption)
      zone   = (config \ "timeZoneId").asOpt[String].flatMap(z => Try(ZoneId.of(z)).toOption).getOrElse(Fallback)
    } yield Slot(title, LocalDateTime.ofInstant(start, zone))
  }
}
