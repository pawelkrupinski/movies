package services.cinemas.pl

import models.*
import services.cinemas.common.ScraperParse
import services.cinemas.common.{CinemaScraper, OnlyMovieEventsFilter}
import tools.{HttpFetch, HttpRead}

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
    KinoNowaFalaClient.parse(HttpRead.page(http, KinoNowaFalaClient.RepertoireUrl), cinema)
}

object KinoNowaFalaClient {

  val BaseUrl       = "https://kino.gizycko.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] =
    WpTheatreEvents.parse(html, BaseUrl, cinema) { (card, slot) =>
      slot.copy(posterUrl = Option(card.selectFirst("figure img[src]")).flatMap(ScraperParse.largestImageUrl).map(ResizedPoster.fullSize))
    }
}
