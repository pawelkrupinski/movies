package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import services.movies.FormatTags
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDate
import java.time.LocalDateTime
import scala.jdk.CollectionConverters._

/** One venue's install of the iKsoris ticketing platform: the host it runs on
 *  and the event group (`idg`) that holds its film screenings — other groups on
 *  the same install are concerts and theatre. */
final case class IksorisSite(baseUrl: String, eventGroup: Int = 1) {
  def terminUrl: String = s"$baseUrl/rezerwacja/termin.html?idg=$eventGroup"
}

/**
 * The iKsoris ticketing platform by SoftCOM Wrocław — a white-label booking
 * backend small municipal cinemas run on their own `bilety.<venue>` host. One
 * page holds a venue's whole bookable schedule: `rezerwacja/termin.html?idg=N`.
 * Every showing on it links to a `rezerwacja/numerowane.html?ter_id=…` booking
 * deep link, whichever of the platform's two page themes the venue uses:
 *
 * PROGRAMME theme (RCK Drzewica, which credits "iKsoris - SoftCOM Wrocław" in
 * its `<meta name="author">`). `li.program__item` groups screenings by day
 * (`h2.program__item-header`, "26.09.2026 / sobota" — the year is present). Each
 * `li.program__show-item` inside it is one showing:
 *   - `.program__show-date`               → `HH:MM`
 *   - `h3.program__show-header a`         → title (the anchor is an outbound
 *     Filmweb link, not a venue page, so it's not surfaced as `filmUrl`)
 *   - `.program__show-header-label` spans → a mix of language ("DUBBING"/
 *     "NAPISY"), age rating ("OGRANICZENIE WIEKOWE N+") and genre words, with
 *     no class telling them apart — only the language ones are recognised
 *     (via [[FormatTags.formatTokensIn]]), the rest left unparsed
 *   - `.program__show-description`        → "<countries, comma-separated>
 *     NNNmin" free text
 *
 * TABLE theme (MCK Bełchatów's Kino Kultura, an older Bootstrap skin). A flat
 * run of `#termin div.row`s, one per showing, each a fixed set of cells: a
 * poster thumbnail `img`, the "27-09-2026 13:15" date-time, the title in `<b>`
 * (followed by an outbound "więcej informacji" distributor link — not a venue
 * page, so not a `filmUrl`), a seat count and the booking button. No runtime,
 * countries or language badge is published in this theme.
 *
 * No `today` parameter: both themes always carry the year. The booking system
 * only opens a film's page about a week ahead, so the listing is short but
 * complete for what can be booked.
 */
class IksorisClient(http: HttpFetch, site: IksorisSite, override val cinema: Cinema) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(site.baseUrl)
  override def sourceUrl: Option[String] = Some(site.terminUrl)

  def fetch(): Seq[CinemaMovie] = IksorisClient.parse(http.get(site.terminUrl), site, cinema)
}

object IksorisClient {

  // "Kanada, USA 89min" — comma-list countries, then the runtime glued to "min".
  private val DescPat = """^(.*?)\s*(\d+)\s*min\s*$""".r

  private val BookingLink = "a[href*=rezerwacja/numerowane.html]"

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    runtime:   Option[Int],
    countries: Seq[String],
    format:    List[String],
    poster:    Option[String]
  )

  def parse(html: String, site: IksorisSite, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, site.baseUrl)
    val slots    = programmeSlots(document) ++ tableSlots(document)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = head.runtime, countries = head.countries),
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

  private def bookingOf(show: Element): Option[String] =
    Option(show.selectFirst(BookingLink)).map(_.attr("abs:href")).filter(_.nonEmpty)

  // ── Programme theme ────────────────────────────────────────────────────────

  private def programmeSlots(document: Document): Seq[RawSlot] =
    document.select("li.program__item").asScala.toSeq.flatMap { item =>
      Option(item.selectFirst("h2.program__item-header")).map(_.text).flatMap(ScraperParse.parseDate).toSeq
        .flatMap(date => item.select("li.program__show-item").asScala.toSeq.flatMap(programmeShow(_, date)))
    }

  private def programmeShow(show: Element, date: LocalDate): Option[RawSlot] =
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
        booking   = bookingOf(show),
        runtime   = runtime,
        countries = countries,
        format    = FormatTags.formatTokensIn(labels.mkString(" ")),
        poster    = None
      )
    }

  // ── Table theme ────────────────────────────────────────────────────────────

  /** A showing row is one carrying a booking link — the header row ("Data",
   *  "Wydarzenie") and the legend carry none. Its date-time is the first cell
   *  text that parses as one; the title is the row's `<b>`. */
  private def tableSlots(document: Document): Seq[RawSlot] =
    document.select("#termin div.row").asScala.toSeq.filter(_.selectFirst(BookingLink) != null).flatMap { row =>
      for {
        dateTime <- row.children.asScala.iterator.map(_.ownText).flatMap(ScraperParse.parseDateTime).nextOption()
        (title, format) <- Option(row.selectFirst("b")).map(b => FormatTags.extractFormatTags(b.text)).filter(_._1.nonEmpty)
      } yield RawSlot(
        title     = title,
        dateTime  = dateTime,
        booking   = bookingOf(row),
        runtime   = None,
        countries = Seq.empty,
        format    = format,
        poster    = Option(row.selectFirst("img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)
      )
    }
}
