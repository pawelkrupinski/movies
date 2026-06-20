package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.movies.TitleNormalizer
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cyfrowe Kino (Środa Śląska, run by the Dom Kultury). Its listing at
 * `dksrodaslaska.pl/aktualny-repertuar/` is a server-rendered WordPress
 * "amy-movie" theme page — one `div.amy-movie-item` per film:
 *   - `h3.amy-movie-field-title a` → title, but DIRTY: often wrapped as
 *     "DD.MM / DKF TITLE / napisy" (leading short date, a "DKF" tag, a trailing
 *     version/age tag). `cleanTitle` strips the noise segments.
 *   - `.entry-showtime .st-item` → one block per screening DATE; `.st-title
 *     label` is `DD/MM/YYYY` (year present, no inference) and `ul li` holds that
 *     date's times ("19:00", sometimes "17:00 NAPISY").
 *   - `.amy-movie-item-poster img[data-src]` → poster (the `src` is a lazyload
 *     SVG placeholder).
 * The only booking link is a single static URL, useless as a per-screening id,
 * so showtimes carry no booking.
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class CyfroweKinoClient(http: HttpFetch, override val cinema: Cinema = KinoCyfroweKino)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(CyfroweKinoClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(CyfroweKinoClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    CyfroweKinoClient.parse(http.get(CyfroweKinoClient.RepertoireUrl), cinema)
}

object CyfroweKinoClient {

  val BaseUrl       = "https://dksrodaslaska.pl"
  val RepertoireUrl = s"$BaseUrl/aktualny-repertuar/"

  private val DateFmt   = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private val ShortDate = """^\d{1,2}\.\d{1,2}$""".r            // "15.06" noise segment
  // Segments that are version / age-rating metadata, not part of the title.
  private val NoiseSeg  = """(?i)^(napisy|dubbing|dubbing i napisy|napisy i dubbing|lektor|2D|3D|od\s+lat\s+\d+|od\s+\d+\s+lat|b/o|\d+\+)$""".r

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] =
    Jsoup.parse(html, BaseUrl).select("div.amy-movie-item").asScala.toSeq.flatMap(parseMovie(_, cinema))

  private def parseMovie(item: Element, cinema: Cinema): Option[CinemaMovie] =
    for {
      rawTitle <- Option(item.selectFirst("h3.amy-movie-field-title a")).map(_.text)
      title     = TitleNormalizer.cinemaClean("cyfrowe-kino", cleanTitle(rawTitle)) if title.nonEmpty
      showtimes = parseShowtimes(item)
      if showtimes.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(title),
      cinema    = cinema,
      posterUrl = Option(item.selectFirst(".amy-movie-item-poster img"))
                    .map(img => if (img.hasAttr("data-src")) img.absUrl("data-src") else img.absUrl("src"))
                    .filter(_.nonEmpty),
      filmUrl   = Option(item.selectFirst("h3.amy-movie-field-title a")).map(_.absUrl("href")).filter(_.nonEmpty),
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = showtimes
    )

  /** Each `.st-item` is one date (`DD/MM/YYYY`) holding its own `ul li` times. */
  private def parseShowtimes(item: Element): Seq[Showtime] =
    item.select(".entry-showtime .st-item").asScala.toSeq.flatMap { st =>
      val date = Option(st.selectFirst(".st-title label")).map(_.text.trim)
        .flatMap(t => Try(LocalDate.parse(t, DateFmt)).toOption)
      date.toSeq.flatMap { d =>
        st.select("ul li").asScala.toSeq.flatMap { li =>
          ScraperParse.parseHHmm(li.text).map(t => Showtime(LocalDateTime.of(d, t), None))
        }
      }
    }.distinctBy(_.dateTime).sortBy(_.dateTime)

  /** "15.06 / DKF WERDYKT / napisy" → "Werdykt". Split on `/`, drop the
   *  short-date and version/age segments, take the longest remaining segment as
   *  the title (stripping a leading "DKF " tag), then sentence-case it. */
  private[cinemas] def cleanTitle(raw: String): String = {
    val kept = raw.split("/").map(_.trim).filter(_.nonEmpty)
      .filterNot(s => ShortDate.findFirstMatchIn(s).isDefined)
      .filterNot(s => NoiseSeg.findFirstMatchIn(s).isDefined)
    val title = kept.sortBy(-_.length).headOption.getOrElse("")
      .replaceFirst("""(?i)^DKF\s+""", "").trim
    ScraperParse.sentenceCase(title)
  }
}
