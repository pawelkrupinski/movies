package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{CinemaScraper, ListingPages, ScraperParse, SlotsToMovies}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The day-by-day repertoire of an iKsoris ticketing site (SoftCOM Wrocław's
 * white-label platform — "System rezerwacji i sprzedaży biletów iKsoris" in
 * every footer) that a small venue runs as its whole website: Kino Plon
 * (Hrubieszów) and Kino Sokolnia (Kępno) both serve it. Filmweb carries little
 * of either, so this reads the venue's own ticketing pages.
 *
 * `repertuar.html?data=YYYY-MM-DD` renders one day. Its `#dni-daty a.dzien`
 * day-picker links EVERY date the venue has anything on (not a fixed window —
 * Plon's ran to December), so the client fetches `today`'s page, reads the
 * picker, then fetches each other listed day.
 *
 * One `.terminy` block holds the day's screenings, in one of two skins:
 *   - `div.termin` (Plon): `span.nazwa` title, then `/ 88'` runtime text, then
 *     one `a[data-content]` per showing whose text is `HH:MM` and whose href is
 *     the showing's `rezerwacja/numerowane.html?id=…` seat-picker.
 *   - `div.termin-box` (Sokolnia): `h3.kalendarium-nazwa-wydarzenia` "TITLE /
 *     <span.czas-trwania>90'</span>", one `.termin-start` `HH:MM`, a `Kup`
 *     (`a.btn-kup`) seat-picker link — or only a `Rezerwuj` one when online sale
 *     is closed — and an `img.termin-img` poster.
 * The listing exposes nothing more — the `wydarzenie.html` event pages aren't
 * linked from a showing and carry at most a prose blurb — so title, runtime,
 * poster and booking link are the whole signal. Live events sold on the same
 * site (concerts) are left to [[NonMovieEventClassifier]] at the scrape seam.
 */
class IksorisRepertoireClient(
  http:   HttpFetch,
  origin: IksorisOrigin,
  override val cinema: Cinema,
  today:  LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import IksorisRepertoireClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(origin.value)
  override def sourceUrl: Option[String] = Some(dayUrl(origin, today))

  // Today's page (the day-picker) failing propagates — a red scrape, not a
  // white "0 films". A later day failing drops only that day, unless every one did.
  def fetch(): Seq[CinemaMovie] = {
    val first     = Jsoup.parse(http.get(dayUrl(origin, today)), origin.value)
    val otherDays = pickerDays(first).filterNot(_ == today)
    val attempts  = otherDays.map(day => Try(day -> Jsoup.parse(http.get(dayUrl(origin, day)), origin.value)))
    ListingPages.requireAnyReached(attempts)
    parse((today -> first) +: attempts.flatMap(_.toOption), cinema)
  }
}

object IksorisRepertoireClient {

  def dayUrl(origin: IksorisOrigin, day: LocalDate): String = s"${origin.value}/repertuar.html?data=$day"

  private val DataParam = """[?&]data=(\d{4}-\d{2}-\d{2})""".r
  private val Runtime   = """(\d+)\s*'""".r

  private case class RawSlot(
    title:    String,
    rawTitle: String,
    dateTime: LocalDateTime,
    runtime:  Option[Int],
    poster:   Option[String],
    booking:  Option[String]
  )

  /** The days the picker links, in page order. */
  private[pl] def pickerDays(document: Document): Seq[LocalDate] =
    document.select("#dni-daty a[href]").asScala.toSeq
      .flatMap(a => DataParam.findFirstMatchIn(a.attr("href")))
      .flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)
      .distinct

  def parse(days: Seq[(LocalDate, Document)], cinema: Cinema): Seq[CinemaMovie] = {
    val slots = days.flatMap { case (day, document) =>
      document.select(".terminy .termin, .terminy .termin-box").asScala.toSeq.flatMap(parseShowing(_, day))
    }
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(
          title          = title,
          runtimeMinutes = group.flatMap(_.runtime).headOption,
          rawTitle       = Some(group.head.rawTitle).filter(_ != title)
        ),
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

  private def parseShowing(box: Element, day: LocalDate): Seq[RawSlot] =
    Option(box.selectFirst(".nazwa, .kalendarium-nazwa-wydarzenia")).map(_.ownText.trim.stripSuffix("/").trim).filter(_.nonEmpty).toSeq.flatMap { rawTitle =>
      val title   = ScraperParse.stripFormatTags(rawTitle)
      val runtime = Runtime.findFirstMatchIn(box.text).flatMap(_.group(1).toIntOption)
      val poster  = Option(box.selectFirst("img.termin-img")).map(_.attr("abs:src")).filter(_.nonEmpty)
      // "Kup" (buy) when online sale is open; a showing sold as reservation-only
      // carries just "Rezerwuj" — the same seat-picker, so it stands in.
      val buyLink = Seq("a.btn-kup[href]", "a[href*=rezerwacja/]").iterator
        .flatMap(selector => Option(box.selectFirst(selector))).map(_.attr("abs:href")).find(_.nonEmpty)
      box.select(".termin-start, a[data-content]").asScala.toSeq.flatMap { timeNode =>
        ScraperParse.parseHHmm(timeNode.text).map { time =>
          val booking = if (timeNode.tagName == "a") Some(timeNode.attr("abs:href")).filter(_.nonEmpty) else buyLink
          RawSlot(title, rawTitle, LocalDateTime.of(day, time), runtime, poster, booking)
        }
      }
    }
}
