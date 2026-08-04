package services.cinemas.pl

import models._
import org.jsoup.nodes.{Element, Node, TextNode}
import org.jsoup.select.NodeVisitor
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * artKino — the cinema of Regionalne Centrum Kultur Pogranicza in Krosno. Its
 * repertoire at `artkino.rckp.krosno.pl/strona-375-repertuar.html` is a single
 * server-rendered article (`div.tresc`) whose body is one `<p>` per screening
 * day:
 *   - a leading `<span style="font-size: x-large;">DD month (weekday)</span>`
 *     date header ("26 czerwca (piątek)") — day + Polish genitive month, NO
 *     year, so `today` anchors year inference (roll a past month forward).
 *   - then `<br/>`-separated screening lines, each `HH:MM - <a href="/wydarzenie-…">TITLE</a>`.
 *
 * Everything is on the one page (title, dates, times, detail link) — no
 * per-film detail fetch. The page carries ONLY film screenings (a culture
 * centre's other programming lives elsewhere on the site), so no
 * [[OnlyMovieEventsFilter]]. Titles are published ALL-CAPS, recased via
 * [[ScraperParse.sentenceCase]]. Previously scraped from Filmweb, which had
 * silently gone empty for the venue.
 */
class ArtKinoKrosnoClient(http: HttpFetch, override val cinema: Cinema = KinoArtKino,
                          today: LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw")))
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ArtKinoKrosnoClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(ArtKinoKrosnoClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    ArtKinoKrosnoClient.parse(http.get(ArtKinoKrosnoClient.RepertoireUrl), cinema, today)
}

object ArtKinoKrosnoClient {

  val BaseUrl       = "https://artkino.rckp.krosno.pl"
  val RepertoireUrl = s"$BaseUrl/strona-375-repertuar.html"

  // "26 czerwca" → day + Polish genitive month (the weekday in parens is ignored).
  private val DatePat = """(\d{1,2})\s+(\p{L}+)""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, filmUrl: Option[String])

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div.tresc p").asScala.toSeq.flatMap(p => parseDay(p, today))

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.filmUrl)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
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

  /** One day's `<p>`: the leading date header anchors the date, then each
   *  `HH:MM - <a>TITLE</a>` line becomes a slot. A `<p>` with no date header
   *  (the photo banner, the empty `&nbsp;` spacers) yields nothing. */
  private def parseDay(p: Element, today: LocalDate): Seq[RawSlot] =
    dateOf(p, today) match {
      case None       => Seq.empty
      case Some(date) => slotsIn(p, date)
    }

  /** The day's screenings, read by walking the `<p>` in document order and
   *  pairing each film anchor with the most recent `HH:MM` seen before it.
   *
   *  Deliberately not "the anchor's previous sibling": the venue has published
   *  the same line in at least two shapes — the flat `HH:MM - <a>TITLE</a>`,
   *  and one where the time sits in its own coloured `<span>` with the anchor
   *  buried several spans deeper (`<span><span>13:45 -</span> <a>PUCIO</a></span>`).
   *  A time is CONSUMED by the anchor it pairs with, so an anchor with no time
   *  of its own yields nothing rather than borrowing the previous line's. */
  private def slotsIn(p: Element, date: LocalDate): Seq[RawSlot] = {
    val slots            = Seq.newBuilder[RawSlot]
    var pending: Option[LocalTime] = None

    p.traverse(new NodeVisitor {
      def head(node: Node, depth: Int): Unit = node match {
        case text: TextNode =>
          ScraperParse.parseHHmm(text.text).foreach(time => pending = Some(time))
        case link: Element if link.tagName == "a" && link.attr("href").contains("wydarzenie") =>
          val title = ScraperParse.sentenceCase(link.text.trim)
          for {
            time <- pending if title.nonEmpty
            dt   <- Try(LocalDateTime.of(date, time)).toOption
          } {
            slots += RawSlot(title, dt, Option(link.attr("abs:href")).filter(_.nonEmpty))
            pending = None
          }
        case _ => ()
      }
    })

    slots.result()
  }

  /** The screening date from the `<p>`'s `font-size: x-large` header span,
   *  inferring the year from `today` (a month earlier than today's rolls to
   *  next year). */
  private def dateOf(p: Element, today: LocalDate): Option[LocalDate] =
    for {
      header <- Option(p.selectFirst("span[style*=x-large]")).map(_.text)
      m      <- DatePat.findFirstMatchIn(header)
      month  <- monthOf(m.group(2))
      year   = if (month < today.getMonthValue) today.getYear + 1 else today.getYear
      date   <- Try(LocalDate.of(year, month, m.group(1).toInt)).toOption
    } yield date

  /** The month number for a header's month word, falling back to its first
   *  three letters when the full word isn't a month — the headers are typed by
   *  hand and do get misspelled ("4 sieprnia"), which would otherwise drop a
   *  whole day of screenings. The three-letter prefixes are unambiguous across
   *  all twelve Polish months. */
  private def monthOf(word: String): Option[Int] =
    ScraperParse.PolishMonths.get(word.toLowerCase)
      .orElse(ScraperParse.polishMonthAbbrev(word.take(3)))
}
