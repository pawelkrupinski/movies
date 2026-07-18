package services.cinemas.pl

import models._
import org.jsoup.nodes.{Element, TextNode}
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
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
      case None => Seq.empty
      case Some(date) =>
        p.select("a[href*=wydarzenie]").asScala.toSeq.flatMap { link =>
          for {
            time  <- timeBefore(link)
            title = ScraperParse.sentenceCase(link.text.trim) if title.nonEmpty
            dt    <- Try(LocalDateTime.of(date, time)).toOption
          } yield RawSlot(
            title    = title,
            dateTime = dt,
            filmUrl  = Option(link.attr("abs:href")).filter(_.nonEmpty)
          )
        }
    }

  /** The screening date from the `<p>`'s `font-size: x-large` header span,
   *  inferring the year from `today` (a month earlier than today's rolls to
   *  next year). */
  private def dateOf(p: Element, today: LocalDate): Option[LocalDate] =
    for {
      header <- Option(p.selectFirst("span[style*=x-large]")).map(_.text)
      m      <- DatePat.findFirstMatchIn(header)
      month  <- ScraperParse.PolishMonths.get(m.group(2).toLowerCase)
      year   = if (month < today.getMonthValue) today.getYear + 1 else today.getYear
      date   <- Try(LocalDate.of(year, month, m.group(1).toInt)).toOption
    } yield date

  /** The `HH:MM` in the text node immediately preceding the film anchor — the
   *  page renders each line as `<br/>14:15 - <a>TITLE</a>`, so the time lives in
   *  the anchor's previous sibling text node. */
  private def timeBefore(link: Element): Option[java.time.LocalTime] =
    Option(link.previousSibling()).collect { case t: TextNode => t.text }
      .flatMap(ScraperParse.parseHHmm)
}
