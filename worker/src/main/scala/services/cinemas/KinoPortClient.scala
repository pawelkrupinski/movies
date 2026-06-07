package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, TextNode}
import tools.HttpFetch

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KinoPort — the cinema run by Gdańskie Centrum Sztuki Współczesnej (CSW
 * Łaźnia, Gdańsk, Trójmiasto). It has no ticketing platform; the weekly
 * programme is published as a WordPress post on `gcsw.pl`. The stable aliases
 * `gcsw.pl/kino/` and `gcsw.pl/kinoport/` 301-redirect to the current week's
 * "Kinoport | Repertuar …" post, so we fetch the alias and let `HttpFetch`
 * follow the redirect to whatever post is live.
 *
 * The post body (`.entry-content`) is plain WordPress paragraphs:
 *   - a date paragraph holding a bare `DD.MM` token (`<p><strong>06.06</strong></p>`),
 *   - followed by one or more schedule paragraphs, each a run of
 *     `<strong>HH:MM</strong> Title | year director | format (rating)` lines
 *     separated by `<br>`.
 *
 * There is no four-digit year on the schedule rows, so we take it from the
 * post's canonical URL (`/2026/06/03/…`), which WordPress always emits.
 *
 * The title is the text before the first `|`; the rest (year, director,
 * dubbing/subtitles, age rating) is descriptive and dropped. Every row on this
 * page is a film screening — even the free exhibition-tie-in screenings carry a
 * real film title — so we keep them all.
 */
class KinoPortClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoPortClient.PageUrl)

  def fetch(): Seq[CinemaMovie] = KinoPortClient.parse(http.get(KinoPortClient.PageUrl), cinema)
}

object KinoPortClient {

  /** Stable alias that redirects to the current "Kinoport | Repertuar" post. */
  val PageUrl = "https://gcsw.pl/kino/"

  // A date paragraph is exactly a `D.MM` / `DD.MM` token (day.month).
  private val DateOnly = """^(\d{1,2})\.(\d{1,2})$""".r
  // The canonical URL is `https://gcsw.pl/<year>/<month>/<day>/<slug>/`.
  private val YearInUrl = """gcsw\.pl/(\d{4})/""".r
  private val HourMinute = """(\d{1,2}):(\d{2})""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, bookingUrl: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc  = Jsoup.parse(html)
    val year = canonicalYear(doc)
    val link = canonicalUrl(doc)

    val slots = doc.select(".entry-content p, .entry-content > div p").asScala.toSeq match {
      case Nil => doc.select("p").asScala.toSeq // fall back to the whole document
      case ps  => ps
    }

    // Walk the paragraphs in document order: a bare `DD.MM` paragraph sets the
    // current day; each subsequent schedule paragraph emits its `HH:MM Title`
    // rows against that day. Slots before any date paragraph are ignored.
    var currentMonthDay: Option[(Int, Int)] = None
    val collected = Seq.newBuilder[RawSlot]

    slots.foreach { p =>
      val text = p.text.trim
      DateOnly.findFirstMatchIn(text) match {
        case Some(m) =>
          currentMonthDay = Some((m.group(1).toInt, m.group(2).toInt))
        case None =>
          currentMonthDay.foreach { case (day, month) =>
            collected ++= parseScheduleParagraph(p, year, month, day, link)
          }
      }
    }

    val all = collected.result()

    all.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = link,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  /** Each `<strong>HH:MM</strong> film line …` run in a schedule paragraph. We
   *  walk the paragraph's child nodes: a `<strong>` whose text is a time opens
   *  a new slot, and the following text/inline nodes (until the next time
   *  `<strong>` or the end) are the film line. The title is the part before the
   *  first `|`. */
  private def parseScheduleParagraph(
    p: Element, year: Int, month: Int, day: Int, link: Option[String]
  ): Seq[RawSlot] = {
    val out = Seq.newBuilder[RawSlot]
    var pendingTime: Option[(Int, Int)] = None
    val lineBuf = new StringBuilder

    def flush(): Unit = pendingTime.foreach { case (h, mm) =>
      val title = lineBuf.toString.trim.takeWhile(_ != '|').trim
      Try(LocalDateTime.of(year, month, day, h, mm)).toOption.foreach { dt =>
        if (title.nonEmpty) out += RawSlot(title, dt, link)
      }
    }

    p.childNodes.asScala.foreach {
      case el: Element if el.tagName == "strong" && HourMinute.findFirstMatchIn(el.text).exists(_.start == 0) =>
        flush()
        val m = HourMinute.findFirstMatchIn(el.text).get
        pendingTime = Some((m.group(1).toInt, m.group(2).toInt))
        lineBuf.clear()
      case el: Element if el.tagName == "br" =>
        // A `<br>` ends a wrapped line but not the slot; keep accumulating until
        // the next time. Most rows are one line, so this rarely matters.
        lineBuf.append(' ')
      case tn: TextNode =>
        lineBuf.append(tn.text)
      case el: Element =>
        lineBuf.append(el.text)
      case _ => ()
    }
    flush()
    out.result()
  }

  private def canonicalUrl(doc: org.jsoup.nodes.Document): Option[String] =
    Option(doc.selectFirst("link[rel=canonical]")).map(_.attr("href")).filter(_.nonEmpty)
      .orElse(Option(doc.selectFirst("meta[property=og:url]")).map(_.attr("content")).filter(_.nonEmpty))

  /** Four-digit year from the canonical / og:url URL; falls back to the current
   *  Warsaw year when the page carries no datable URL. */
  private def canonicalYear(doc: org.jsoup.nodes.Document): Int =
    canonicalUrl(doc)
      .flatMap(u => YearInUrl.findFirstMatchIn(u).map(_.group(1).toInt))
      .getOrElse(java.time.LocalDate.now(java.time.ZoneId.of("Europe/Warsaw")).getYear)
}
