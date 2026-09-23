package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import services.movies.FormatTags
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDate
import java.time.LocalDateTime
import scala.jdk.CollectionConverters._

/**
 * The cinema run by Regionalne Centrum Kultury w Drzewicy (RCK). Its news
 * post lists an incomplete, unstructured-text summary of the schedule; the
 * structured, complete source is its ticketing backend — "iKsoris" by
 * SoftCOM Wrocław (`bilety.rck.drzewica.pl`, confirmed by the page's own
 * `<meta name="author">` and footer credit), a white-label platform this
 * codebase has no client for yet. One page holds the whole schedule:
 * `rezerwacja/termin.html?idg=1` (`idg=6` is a different, non-cinema event
 * group on the same instance).
 *
 * `li.program__item` groups screenings by day (`h2.program__item-header`,
 * "26.09.2026 / sobota" — date + weekday name, year present). Each
 * `li.program__show-item` inside it is one showing:
 *   - `.program__show-date`               → `HH:MM`
 *   - `h3.program__show-header a`         → title (the anchor is actually an
 *     outbound Filmweb link, not a venue page, so it's not surfaced as `filmUrl`)
 *   - `.program__show-header-label` spans → a mix of language ("DUBBING"/
 *     "NAPISY"), age rating ("OGRANICZENIE WIEKOWE N+") and genre words, with
 *     no class telling them apart — only the language ones are recognised
 *     (via [[FormatTags.formatTokensIn]]), the rest left unparsed
 *   - `.program__show-description`        → "<countries, comma-separated>
 *     NNNmin" free text
 *   - `.program__show-buttons a[href]`    → the iKsoris booking deep link
 *
 * No `today` parameter: `program__item-header` always carries the year.
 *
 * Verified screening real, dated films 2026-09-23 through 2026-10-07 (the
 * booking system only opens a film's page a week ahead, so nothing for
 * October existed yet at capture time): Psi Patrol i Dinozaury, Niebo nad
 * Normandią (26/27 Sep), Mistyczka (29 Sep), Vaiana (30 Sep).
 */
class KinoRCKDrzewicaClient(http: HttpFetch, override val cinema: Cinema = KinoRCKDrzewica) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoRCKDrzewicaClient.TerminUrl)
  override def sourceUrl: Option[String] = Some(KinoRCKDrzewicaClient.TerminUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoRCKDrzewicaClient.parse(http.get(KinoRCKDrzewicaClient.TerminUrl), cinema)
}

object KinoRCKDrzewicaClient {

  val BaseUrl   = "https://bilety.rck.drzewica.pl"
  val TerminUrl = s"$BaseUrl/rezerwacja/termin.html?idg=1"

  // "Kanada, USA 89min" — comma-list countries, then the runtime glued to "min".
  private val DescPat = """^(.*?)\s*(\d+)\s*min\s*$""".r

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    runtime:   Option[Int],
    countries: Seq[String],
    format:    List[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("li.program__item").asScala.toSeq.flatMap(parseDay)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = head.runtime, countries = head.countries),
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

  private def parseDay(item: Element): Seq[RawSlot] =
    Option(item.selectFirst("h2.program__item-header")).map(_.text).flatMap(ScraperParse.parseDate) match {
      case None       => Seq.empty
      case Some(date) => item.select("li.program__show-item").asScala.toSeq.flatMap(parseShow(_, date))
    }

  private def parseShow(show: Element, date: LocalDate): Option[RawSlot] =
    for {
      time  <- Option(show.selectFirst(".program__show-date")).map(_.text.trim).flatMap(ScraperParse.parseHHmm)
      title <- Option(show.selectFirst("h3.program__show-header a")).map(_.text.trim).filter(_.nonEmpty)
    } yield {
      val labels = show.select(".program__show-header-label").asScala.toSeq.map(_.text.trim)
      val (countries, runtime) = Option(show.selectFirst(".program__show-description")).map(_.text.trim) match {
        case Some(DescPat(countriesPart, minutes)) =>
          (countriesPart.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq, minutes.toIntOption)
        case _ => (Seq.empty, None)
      }
      RawSlot(
        title     = title,
        dateTime  = LocalDateTime.of(date, time),
        booking   = Option(show.selectFirst(".program__show-buttons a[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
        runtime   = runtime,
        countries = countries,
        format    = FormatTags.formatTokensIn(labels.mkString(" "))
      )
    }
}
