package services.cinemas.pl

import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.time.LocalDateTime
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
 *     a trailing `(2D/napisy)`-style format tag [[ScraperParse.extractFormatTags]]
 *     peels off.
 *   - `img.img-responsive[src^=/file/get/id/]` → poster.
 *   - `a.B-btn.B-btn--accent[href^=/event/view/id/]` → booking link.
 */
class KinoLotSwidnikClient(http: HttpFetch, override val cinema: Cinema = KinoLotSwidnik)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoLotSwidnikClient.WidgetUrl)
  override def sourceUrl: Option[String] = Some(KinoLotSwidnikClient.WidgetUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoLotSwidnikClient.parse(http.get(KinoLotSwidnikClient.WidgetUrl), cinema)
}

object KinoLotSwidnikClient {

  val BaseUrl   = "https://iframe22.biletyna.pl"
  val WidgetUrl = s"$BaseUrl/if/index/?ifid=22&q="

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    poster:   Option[String],
    format:   List[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("div.iframe_all").asScala.toSeq.flatMap(parseScreening)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def parseScreening(block: Element): Option[RawSlot] = {
    val boldSpans = block.select("div.event-date span.B-font-weight--bold").asScala.toSeq
    for {
      dateSpan    <- boldSpans.headOption
      timeSpan    <- boldSpans.drop(1).headOption
      date        <- ScraperParse.parseDate(dateSpan.text)
      time        <- ScraperParse.parseHHmm(timeSpan.text)
      titleAnchor <- Option(block.selectFirst("div.iframe_all_event_title p a[href^=\"/artist/view/id/\"]"))
      raw          = titleAnchor.text.trim if raw.nonEmpty
    } yield {
      val (title, format) = ScraperParse.extractFormatTags(raw)
      RawSlot(
        title    = title,
        dateTime = date.atTime(time),
        booking  = Option(block.selectFirst("a.B-btn.B-btn--accent[href^=\"/event/view/id/\"]")).map(_.attr("abs:href")).filter(_.nonEmpty),
        poster   = Option(block.selectFirst("img.img-responsive[src^=\"/file/get/id/\"]")).map(_.attr("abs:src")).filter(_.nonEmpty),
        format   = format
      )
    }
  }
}
