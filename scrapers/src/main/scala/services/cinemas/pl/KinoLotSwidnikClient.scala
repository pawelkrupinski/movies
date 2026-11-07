package services.cinemas.pl

import services.cinemas.common.{ListingPageScraper, ScrapedSlot, ScraperParse, SlotsToMovies}
import services.movies.FormatTags
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.jdk.CollectionConverters._

/**
 * Kino Lot (MOK Świdnik) — an UNRELATED venue from the Jelenia Góra cinema of
 * the same name ([[KinoLot]], a `<venue>.bilety24.pl` subdomain); this one is
 * ticketed through biletyna.pl instead. Its own page
 * (`mok.swidnik.pl/kino-lot-swidnik/repertuar/`) is a thin WordPress shell
 * carrying only a poster image and an `easyXDM` cross-domain iframe loader —
 * the actual schedule lives at the iframe's own URL
 * (`iframe22.biletyna.pl/if/index/?ifid=22&q=`, `ifid=22` naming this venue)
 * and is plain server-rendered HTML, fetchable directly with no JS execution.
 * That is NOT the JSON-LD `biletyna.pl/<City>/<Venue>` place page
 * [[BiletynaClient]] parses — it's biletyna's "widget" template instead, one
 * `div.iframe_all` per screening:
 *   - `div.event-date span.B-font-weight--bold` → two matches: the date
 *     (`DD.MM.YYYY`) then the time (`HH:MM`, nested one level deeper under
 *     `span.B-text--nowrap`).
 *   - `div.iframe_all_event_title p a[href^=/artist/view/id/]` → title, with
 *     a trailing `(2D/napisy)`-style format tag [[FormatTags.extractFormatTags]]
 *     peels off.
 *   - `img.img-responsive[src^=/file/get/id/]` → poster.
 *   - `a.B-btn.B-btn--accent[href^=/event/view/id/]` → booking link.
 */
class KinoLotSwidnikClient(http: HttpFetch, override val cinema: Cinema = KinoLotSwidnik)
    extends ListingPageScraper(http, KinoLotSwidnikClient.WidgetUrl, KinoLotSwidnikClient.WidgetUrl) {
  protected def parsePage(html: String): Seq[CinemaMovie] =

    KinoLotSwidnikClient.parse(html, cinema)
}

object KinoLotSwidnikClient {

  val BaseUrl   = "https://iframe22.biletyna.pl"
  val WidgetUrl = s"$BaseUrl/if/index/?ifid=22&q="

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("div.iframe_all").asScala.toSeq.flatMap(parseScreening)

    SlotsToMovies.basic(slots, cinema)
  }

  private def parseScreening(block: Element): Option[ScrapedSlot] = {
    val boldSpans = block.select("div.event-date span.B-font-weight--bold").asScala.toSeq
    for {
      dateSpan    <- boldSpans.headOption
      timeSpan    <- boldSpans.drop(1).headOption
      date        <- ScraperParse.parseDate(dateSpan.text)
      time        <- ScraperParse.parseHHmm(timeSpan.text)
      titleAnchor <- Option(block.selectFirst("div.iframe_all_event_title p a[href^=\"/artist/view/id/\"]"))
      raw          = titleAnchor.text.trim if raw.nonEmpty
    } yield {
      val (title, format) = FormatTags.extractFormatTags(raw)
      ScrapedSlot(
        title      = title,
        dateTime   = date.atTime(time),
        bookingUrl = Option(block.selectFirst("a.B-btn.B-btn--accent[href^=\"/event/view/id/\"]")).map(_.attr("abs:href")).filter(_.nonEmpty),
        posterUrl  = Option(block.selectFirst("img.img-responsive[src^=\"/file/get/id/\"]")).map(_.attr("abs:src")).filter(_.nonEmpty),
        format     = format
      )
    }
  }
}
