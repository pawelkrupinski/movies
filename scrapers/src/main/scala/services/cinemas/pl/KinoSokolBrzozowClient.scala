package services.cinemas.pl

import models._
import services.movies.FormatTags
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, ScrapedSlot, ScraperParse, SlotsToMovies}

import scala.jdk.CollectionConverters._

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
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSokolBrzozowClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoSokolBrzozowClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoSokolBrzozowClient.parse(HttpRead.page(http, KinoSokolBrzozowClient.RepertoireUrl), cinema)
}

object KinoSokolBrzozowClient {

  val BaseUrl       = "http://bdk.brzozow.pl"
  val RepertoireUrl = s"$BaseUrl/kino/"

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div.wp_theatre_event").asScala.toSeq.flatMap(parseEvent)

    SlotsToMovies.basic(slots, cinema)
  }

  private def parseEvent(ev: Element): Option[ScrapedSlot] =
    for {
      titleElement <- Option(ev.selectFirst(".wp_theatre_event_title a"))
      title    = titleElement.text.trim if title.nonEmpty
      dateText <- Option(ev.selectFirst(".wp_theatre_event_startdate")).map(_.text)
      date     <- ScraperParse.parseDayMonthYear(dateText)
      time     <- Option(ev.selectFirst(".wp_theatre_event_starttime")).flatMap(t => ScraperParse.parseHHmm(t.text))
    } yield ScrapedSlot(
      title      = title,
      dateTime   = date.atTime(time),
      bookingUrl = Option(ev.selectFirst("a.wp_theatre_event_tickets_url")).map(_.attr("abs:href")).filter(_.nonEmpty),
      filmUrl    = Option(titleElement.attr("abs:href")).filter(_.nonEmpty),
      // The venue line is this cinema's LANGUAGE VERSION, per screening — the
      // same film runs "2D dubbing pl" in the afternoon and "2D napisy pl" in the
      // evening. Nothing else on the page says so: the title is identical for
      // both, so the central `FormatTags` title strip cannot reach it and the two
      // showings were indistinguishable on the badge.
      format     = Option(ev.selectFirst(".wp_theatre_event_venue"))
        .map(_.text).map(FormatTags.formatTokensIn).getOrElse(Nil)
    )
}
