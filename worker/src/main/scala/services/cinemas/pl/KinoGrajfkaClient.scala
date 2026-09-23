package services.cinemas.pl

import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import tools.HttpFetch
import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Grajfka (Chorzowskie Centrum Kultury), Chorzów — a custom WordPress
 * "ctc_event" post-type archive at `kino.chck.pl/repertuar/`. Every upcoming
 * screening renders as one `article.ctc_event` card in a single static
 * response with no pagination:
 *  - `span.ctc-num-date` + the Polish three-letter month abbreviation text
 *    node + `span.ctc-num-year`, all inside one `span.index-cat`
 *    (`23<br>wrz<span class="ctc-num-year">2026</span>`) → the screening date.
 *  - `h2.entry-title-index a[href]` → title, and the venue's own
 *    `/repertuar/<slug>/` detail page link (`filmUrl`).
 *  - `span.ctc-time` → "HH:MM &ndash; HH:MM" (start–end); only the START time
 *    is kept.
 *
 * Every category the archive uses (`smyk`, `na-ekranie`, `dokumenty`,
 * `filmowa-europa`, `moj-ulubiony-film`, `oblicza-azji`,
 * `polska-oczami-wajdy`, `kfg`) is a film-screening series — the `ctc_event`
 * post type itself is this venue's film repertoire, not a general events
 * calendar, so unlike [[KinoZaciszeClient]] there's no non-film content to
 * filter out.
 *
 * Tickets sell through a separate `bilety.chck.pl` booking system, but the
 * link is only rendered on each film's own detail page — not the listing —
 * keyed by a slug that doesn't always match the listing slug (WordPress
 * de-duplication suffixes like `-2`/`-3` diverge between the two). Fetching
 * every detail page just for that link isn't worth the N extra requests, so
 * `bookingUrl` is left `None`; `filmUrl` (the listing's own link) is the
 * booking/film URL surfaced instead. The poster thumbnail is a themed
 * 380x228 banner crop, not the film's real poster, so `posterUrl` is left
 * `None` too — central enrichment fills it from TMDB.
 */
class KinoGrajfkaClient(
  http:                HttpFetch,
  override val cinema: Cinema
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoGrajfkaClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoGrajfkaClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoGrajfkaClient.parse(http.get(KinoGrajfkaClient.RepertoireUrl), cinema)
}

object KinoGrajfkaClient {

  val BaseUrl       = "https://kino.chck.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private case class RawSlot(title: String, dateTime: LocalDateTime, filmUrl: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("article.ctc_event").asScala.toSeq.flatMap(parseArticle)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, bookingUrl = None)) { (_, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(group.head.title),
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

  /** One screening card: title + link from the header, date from the
   *  `span.index-cat` day/month/year triple, start time from `span.ctc-time`. */
  private def parseArticle(article: Element): Option[RawSlot] = {
    val titleAnchor = Option(article.selectFirst("h2.entry-title-index a[href]"))
    val title       = titleAnchor.map(_.text.trim).filter(_.nonEmpty)
    val filmUrl     = titleAnchor.map(_.attr("abs:href")).filter(_.nonEmpty)

    // The month token is the only non-digit text inside `span.index-cat` — the
    // day and year live in their own child spans, so stripping digits off the
    // whole label's text leaves just the abbreviation ("wrz", "paź") regardless
    // of how jsoup renders the `<br>` between the day and month.
    val dateLabel = Option(article.selectFirst("span.index-cat"))
    val day       = Option(article.selectFirst("span.ctc-num-date")).flatMap(e => e.text.trim.toIntOption)
    val year      = Option(article.selectFirst("span.ctc-num-year")).flatMap(e => e.text.trim.toIntOption)
    val month     = dateLabel.map(_.text.replaceAll("[0-9]", "").trim).flatMap(ScraperParse.polishMonthAbbrev)

    val time = Option(article.selectFirst("span.ctc-time")).flatMap(e => ScraperParse.parseHHmm(e.text))

    for {
      t    <- title
      d    <- day
      m    <- month
      y    <- year
      tm   <- time
      date <- Try(LocalDate.of(y, m, d)).toOption
    } yield RawSlot(t, date.atTime(tm), filmUrl)
  }
}
