package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.nio.charset.Charset
import java.time.LocalDate
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Grunwald (Olsztynek). Its home page (`kino.olsztynek.com.pl`) is a
 * single very old page built with "WYSIWYG Web Builder 8": absolute-positioned
 * `<div id="wb_Text…">`s with inline styles, no semantic classes. The page
 * declares (and ships as) ISO-8859-2 with no charset in the HTTP headers, so
 * it's read via [[HttpFetch.getBytes]] and decoded explicitly — decoding as
 * UTF-8 turns every Polish diacritic into mojibake.
 *
 * Each film is one `div[id^=wb_Text]`: its first line(s) (before any date
 * appears) are the shouted ALL-CAPS title (run through
 * [[ScraperParse.sentenceCase]]), its remaining `<br/>`-separated lines are
 * `D.MM <dzień tygodnia> - HH:MM[ i HH:MM]` per-day showtimes (no year — the
 * page always lists the current/upcoming run). NOT every film on the page
 * carries a schedule (a just-listed title with no dates yet) — those parse
 * to zero showtimes and are dropped, same as an empty scrape of a normal
 * client.
 */
class KinoGrunwaldClient(http: HttpFetch, override val cinema: Cinema = KinoGrunwald,
                      today: LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoGrunwaldClient.PageUrl)
  override def sourceUrl: Option[String] = Some(KinoGrunwaldClient.PageUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoGrunwaldClient.parse(new String(http.getBytes(KinoGrunwaldClient.PageUrl), KinoGrunwaldClient.PageCharset), cinema, today)
}

object KinoGrunwaldClient {

  val PageUrl: String = "https://kino.olsztynek.com.pl"

  // The page ships no charset; it is raw ISO-8859-2 (Polish Latin-2).
  val PageCharset: Charset = Charset.forName("ISO-8859-2")

  // "2.10 piątek - 16:00 i 19:00" — day.month, a weekday name (ignored beyond
  // matching), one or two HH:MM times joined by " i ".
  private val DateLine = """^(\d{1,2})\.(\d{1,2})\s+\p{L}+\s*-\s*(\d{1,2}:\d{2})(?:\s+i\s+(\d{1,2}:\d{2}))?$""".r
  private val PremierowoSuffix = """(?i)\s*-\s*premierowo\s*$""".r

  private case class RawSlot(title: String, dateTime: java.time.LocalDateTime)

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, PageUrl)
    val slots = document.select("div[id^=wb_Text]").asScala.toSeq.flatMap(parseFilmDiv(_, today))

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

  private def parseFilmDiv(div: Element, today: LocalDate): Seq[RawSlot] = {
    val lines = linesOf(div)
    val (titleLines, rest) = lines.span(l => DateLine.findFirstMatchIn(l).isEmpty)
    if (rest.isEmpty) return Seq.empty

    val rawTitle = PremierowoSuffix.replaceAllIn(titleLines.mkString(" ").trim, "")
    if (rawTitle.isEmpty) return Seq.empty
    val title = ScraperParse.sentenceCase(rawTitle)

    rest.flatMap(l => DateLine.findFirstMatchIn(l)).flatMap { m =>
      val monthDay = Try(java.time.MonthDay.of(m.group(2).toInt, m.group(1).toInt)).toOption
      val date     = monthDay.flatMap(ScraperParse.upcomingDate(_, today))
      val times    = Seq(Option(m.group(3)), Option(m.group(4))).flatten.flatMap(ScraperParse.parseHHmm)
      for { d <- date.toSeq; t <- times } yield RawSlot(title, d.atTime(t))
    }
  }

  /** The `<br/>`-separated plain-text lines of a `wb_Text` div. NOT
    * [[ScraperParse.linesOf]]/`blockText` — this page's builder nests
    * `<br/>` tags INSIDE `<strong>` spans (`<strong><br/>2.10 …</strong>`),
    * so the block-boundary sentinel trick those helpers use ends up wrapped
    * by the SAME bold-marker pass, producing one bold-fenced blob with the
    * sentinel newlines invisible to a later split. Splitting the raw HTML on
    * `<br>` first, then stripping tags per fragment, sidesteps that. */
  private def linesOf(div: Element): Seq[String] =
    div.html.split("(?i)<br\\s*/?>").toSeq
      .map(fragment => Jsoup.parseBodyFragment(fragment).body.text.trim)
      .filter(_.nonEmpty)
}
