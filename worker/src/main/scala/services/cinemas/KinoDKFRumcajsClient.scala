package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * DKF Rumcajs (Częstochowa) — a Dyskusyjny Klub Filmowy (film discussion club)
 * that screens roughly WEEKLY, on Mondays at 18:00 (the venue states its own
 * cadence as "DKF Rumcajs Częstochowa w poniedziałki o 18.00"). Because the
 * programme is sparse, the site is a WordPress blog: each month is a single
 * post titled "DKF RUMCAJS zaprasza na filmy – <month> <year>", listed
 * newest-first in the `repertuar` category. We:
 *   1. fetch `/category/repertuar`, whose first `h2.entry-title a` is the
 *      current month's post;
 *   2. fetch that post and parse its `div.entry-content`.
 *
 * Inside a post each film is introduced by a bold/italic header paragraph
 * `<p><strong><em>D.MM Title (Sala …)</em></strong></p>`:
 *   - the leading `D.MM` is the screening day+month (NO year on the line — the
 *     year comes from the post title/slug "… <year>");
 *   - there is NO time on the line — every DKF screening is the venue-wide
 *     18:00 default;
 *   - a trailing "(Sala …)" names the room and is dropped from the title;
 *   - the paragraphs that follow, up to the next header, carry "Reżyseria:",
 *     "Obsada:" and a free-text synopsis. The venue takes no online bookings.
 */
class KinoDKFRumcajsClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoDKFRumcajs,
  today: LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoDKFRumcajsClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    currentMonthPostUrl(http.get(RepertoireUrl))
      .map(url => parsePost(http.get(url), cinema, today))
      .getOrElse(Seq.empty)
}

object KinoDKFRumcajsClient {

  val BaseUrl       = "https://www.rumcajs.czest.pl"
  val RepertoireUrl = s"$BaseUrl/category/repertuar"

  /** Every screening is the venue's fixed Monday 18:00 slot — the lines carry
   *  no time of their own. */
  private val ScreeningTime = LocalTime.of(18, 0)

  // "1.06 Title (Sala …)" — day, month, then the title (room tag stripped below).
  private val HeaderPat = """^\s*(\d{1,2})\.(\d{1,2})\s+(.+)$""".r
  private val YearPat   = """\b(20\d{2})\b""".r
  private val RoomPat   = """\s*\(\s*[Ss]ala[^)]*\)\s*$""".r
  private val DirectorPat = """(?i)^Re[zż]yseria:\s*(.+)$""".r
  private val CastPat     = """(?i)^Obsada:\s*(.+)$""".r

  private case class RawFilm(
    title:    String,
    date:     LocalDate,
    director: Seq[String],
    cast:     Seq[String],
    synopsis: Option[String]
  )

  /** The newest monthly post in the repertoire category — the current month. */
  private[cinemas] def currentMonthPostUrl(indexHtml: String): Option[String] =
    Option(Jsoup.parse(indexHtml, BaseUrl).selectFirst("h2.entry-title a[href]"))
      .map(_.attr("abs:href"))
      .filter(_.nonEmpty)

  private[cinemas] def parsePost(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val year     = postYear(document).getOrElse(today.getYear)
    val content  = Option(document.selectFirst("div.entry-content")).getOrElse(document.body)

    parseFilms(content, year).map { film =>
      CinemaMovie(
        movie     = Movie(film.title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = film.synopsis,
        cast      = film.cast,
        director  = film.director,
        showtimes = Seq(Showtime(LocalDateTime.of(film.date, ScreeningTime), bookingUrl = None))
      )
    }.sortBy(c => (c.showtimes.head.dateTime, c.movie.title))
  }

  /** Year from the post title (e.g. "… czerwiec 2026"), so a `D.MM` line with no
   *  year resolves correctly. */
  private def postYear(document: Element): Option[Int] =
    Option(document.selectFirst("h1.entry-title, h1, title"))
      .map(_.text)
      .flatMap(t => YearPat.findFirstMatchIn(t).map(_.group(1).toInt))

  /** Walk the entry-content paragraphs: a bold+italic "D.MM Title (Sala …)"
   *  paragraph opens a film; the paragraphs until the next such header are its
   *  director / cast / synopsis. */
  private def parseFilms(content: Element, year: Int): Seq[RawFilm] = {
    val paragraphs = content.select("p").asScala.toSeq

    // Index of each header paragraph (a `D.MM Title` line) — films span from one
    // header up to (but not including) the next.
    val headers = paragraphs.zipWithIndex.collect {
      case (p, i) if headerOf(p, year).isDefined => i
    }

    headers.zipWithIndex.flatMap { case (start, h) =>
      val end  = headers.lift(h + 1).getOrElse(paragraphs.length)
      val (title, date) = headerOf(paragraphs(start), year).get
      val body = paragraphs.slice(start + 1, end)
      Some(RawFilm(
        title    = title,
        date     = date,
        director = peopleAfter(body, DirectorPat),
        cast     = peopleAfter(body, CastPat),
        synopsis = synopsisOf(body)
      ))
    }
  }

  /** A `(title, date)` when the paragraph is a film header — its first line is
   *  emphasised and matches "D.MM …". The room tag "(Sala …)" is stripped. */
  private def headerOf(p: Element, year: Int): Option[(String, LocalDate)] = {
    val emphasised = Option(p.selectFirst("strong em, em strong")).isDefined
    if (!emphasised) None
    else HeaderPat.findFirstMatchIn(p.text.trim).flatMap { m =>
      val rawTitle = RoomPat.replaceFirstIn(m.group(3).trim, "").trim
      Try(LocalDate.of(year, m.group(2).toInt, m.group(1).toInt)).toOption
        .filter(_ => rawTitle.nonEmpty)
        .map(date => (rawTitle, date))
    }
  }

  /** Names off a "Reżyseria: A, B" / "Obsada: A, B, C" line, split on commas. */
  private def peopleAfter(body: Seq[Element], pat: scala.util.matching.Regex): Seq[String] =
    body.iterator
      .map(_.text.trim)
      .flatMap(pat.findFirstMatchIn)
      .map(_.group(1))
      .toSeq.headOption.toSeq
      .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))

  /** The synopsis is the first substantial plain paragraph (not a Reżyseria/
   *  Obsada/Scenariusz metadata line, not the country/runtime footer). */
  private def synopsisOf(body: Seq[Element]): Option[String] =
    body.map(_.text.trim).find(t => t.length > 60 && !MetadataLine.findFirstIn(t).isDefined)

  private val MetadataLine = """(?i)^(Re[zż]yseria|Scenariusz|Obsada|DRAMAT|KOMEDIA|prod\.)""".r
}
