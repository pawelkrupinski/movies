package services.cinemas.pl

import models.*
import services.cinemas.common.ListingPageScraper
import services.titles.FormatTags
import tools.HttpFetch

/**
 * Kino Sokół (Brzozowski Dom Kultury, Brzozów). Its repertoire at
 * `bdk.brzozow.pl/kino/` is a WordPress "Theatre" (WPT) plugin page — one
 * `div.wp_theatre_event` per screening, each self-contained:
 *   - `.wp_theatre_event_title a` → title (the poster `alt` is empty — use the
 *     title anchor)
 *   - `.wp_theatre_event_startdate` → full Polish date with year ("12 czerwca
 *     2026"), so no year inference
 *   - `.wp_theatre_event_starttime` → "16:45"
 *   - `.wp_theatre_event_venue` → format tag ("2D dubbing pl")
 *   - `a.wp_theatre_event_tickets_url` → a fixed ekobilet landing link
 *
 * Served over plain HTTP (the HTTPS cert altname is invalid). Previously
 * scraped from Filmweb, which had silently gone empty for the venue.
 */
class KinoSokolBrzozowClient(http: HttpFetch, override val cinema: Cinema = KinoSokolBrzozow)
    extends ListingPageScraper(http, KinoSokolBrzozowClient.RepertoireUrl, KinoSokolBrzozowClient.BaseUrl) {
  protected def parsePage(html: String): Seq[CinemaMovie] =
    KinoSokolBrzozowClient.parse(html, cinema)
}

object KinoSokolBrzozowClient {

  val BaseUrl       = "http://bdk.brzozow.pl"
  val RepertoireUrl = s"$BaseUrl/kino/"

  // The venue line is this cinema's LANGUAGE VERSION, per screening — the
  // same film runs "2D dubbing pl" in the afternoon and "2D napisy pl" in the
  // evening. Nothing else on the page says so: the title is identical for
  // both, so the central `FormatTags` title strip cannot reach it and the two
  // showings were indistinguishable on the badge.
  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] =
    WpTheatreEvents.parse(html, BaseUrl, cinema) { (card, slot) =>
      slot.copy(format = Option(card.selectFirst(".wp_theatre_event_venue")).map(_.text).map(FormatTags.formatTokensIn).getOrElse(Nil))
    }
}
