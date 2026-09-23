package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import scala.jdk.CollectionConverters._

/**
 * Kino Nowa Fala (Giżycko). Its programme at `kino.gizycko.pl/repertuar/` is
 * built on the WordPress "WP Theatre" plugin — plain server-rendered HTML, one
 * `div.wp_theatre_event` per screening:
 *   - `div.wp_theatre_event_title a`                        → title + the
 *     venue's own `/production/<slug>/` detail page (`filmUrl`)
 *   - `figure img[src]`                                     → poster URL
 *   - `div.wp_theatre_event_date.wp_theatre_event_startdate` → the Polish full
 *     date ("25 września 2026")
 *   - `div.wp_theatre_event_time.wp_theatre_event_starttime` → `HH:MM`
 *   - `div.wp_theatre_event_tickets a.wp_theatre_event_tickets_url` → the
 *     booking link, on a separate `bilety.gizycko.pl` checkout domain
 *
 * [[OnlyMovieEventsFilter]] is mixed in defensively — the plugin is a generic
 * events calendar, so a future non-film listing (a local festival, a one-off
 * stage event) would otherwise leak straight through with no structured field
 * to tell it apart, the same risk [[KinoZaciszeClient]] guards against.
 */
class KinoNowaFalaClient(http: HttpFetch, override val cinema: Cinema = KinoNowaFalaGizycko)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoNowaFalaClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoNowaFalaClient.RepertoireUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    KinoNowaFalaClient.parse(http.get(KinoNowaFalaClient.RepertoireUrl), cinema)
}

object KinoNowaFalaClient {

  val BaseUrl       = "https://kino.gizycko.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private case class RawSlot(
    title:    String,
    dateTime: java.time.LocalDateTime,
    booking:  Option[String],
    filmUrl:  Option[String],
    poster:   Option[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div.wp_theatre_event").asScala.toSeq.flatMap(parseEvent)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  /** One `div.wp_theatre_event` card's title/date/time/booking/poster. */
  private def parseEvent(card: Element): Option[RawSlot] =
    for {
      titleElement <- Option(card.selectFirst("div.wp_theatre_event_title a"))
      title    = titleElement.text.trim if title.nonEmpty
      dateText <- Option(card.selectFirst("div.wp_theatre_event_date.wp_theatre_event_startdate")).map(_.text)
      date     <- ScraperParse.parseDayMonthYear(dateText)
      timeText <- Option(card.selectFirst("div.wp_theatre_event_time.wp_theatre_event_starttime")).map(_.text)
      time     <- ScraperParse.parseHHmm(timeText)
    } yield RawSlot(
      title    = title,
      dateTime = date.atTime(time),
      booking  = Option(card.selectFirst("div.wp_theatre_event_tickets a.wp_theatre_event_tickets_url"))
                   .map(_.attr("abs:href")).filter(_.nonEmpty),
      filmUrl  = Option(titleElement.attr("abs:href")).filter(_.nonEmpty),
      poster   = Option(card.selectFirst("figure img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)
    )
}
