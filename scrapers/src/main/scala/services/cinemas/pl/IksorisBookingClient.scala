package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import services.movies.FormatTags
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDate
import java.time.LocalDateTime
import scala.jdk.CollectionConverters._

/** One venue's install of the iKsoris ticketing platform: the host it runs on
 *  and the event group (`idg`) that holds its film screenings — other groups on
 *  the same install are concerts and theatre. */
final case class IksorisBookingPage(origin: IksorisOrigin, eventGroup: Int = 1) {
  def url: String = s"${origin.value}/rezerwacja/termin.html?idg=$eventGroup"
}

/**
 * The iKsoris ticketing platform by SoftCOM Wrocław — a white-label booking
 * backend small municipal cinemas run on their own `bilety.<venue>` host. One
 * page holds a venue's whole bookable schedule: `rezerwacja/termin.html?idg=N`,
 * served server-side even where the install's front page is a script-driven
 * shell. Every showing on it carries its own booking deep link, whichever of
 * the platform's three page themes the venue uses:
 *
 * PROGRAMME theme (RCK Drzewica, which credits "iKsoris - SoftCOM Wrocław" in
 * its `<meta name="author">`). `li.program__item` groups screenings by day
 * (`h2.program__item-header`, "26.09.2026 / sobota" — the year is present). Each
 * `li.program__show-item` inside it is one showing:
 *   - `.program__show-date`               → `HH:MM`
 *   - `.program__show-item a[href*=rezerwacja/numerowane.html]` → booking
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
 * page, so not a `filmUrl`), a seat count and the `rezerwacja/numerowane.html`
 * booking button. No runtime, countries or language badge is published in this
 * theme.
 *
 * TERMS-LIST theme (Biłgorajskie Centrum Kultury's Kino BCK). `div.terms-in-date`
 * groups showings by day (`.term-in-date-header`, "2026-09-27 niedziela"); each
 * `.term-in-date-term` inside it is one showing:
 *   - `.hour-wrapper`       → `HH:MM`
 *   - `.title-block`        → title, usually wrapped in an outbound link — a
 *     Filmweb one (`filmweb.pl/film/Lalka-2026-10057628`) whose slug carries the
 *     production year, or a distributor's page; neither is a venue page, so not
 *     a `filmUrl`
 *   - `.description-block`  → "<countries>, NNN min, od lat N, <genres>" — the
 *     genres split on "/" or ","; an event without an age rating ("170 min,
 *     Retransmisja letniego koncertu…") has prose, not genres, after the runtime
 *   - `.event-tag` spans    → "2D", "NAPISY", "HIT", "NOWOŚĆ"… — only the
 *     format ones are recognised
 *   - `a.select-term-btn`   → booking (`miejsca.html?id=…`)
 *
 * No `today` parameter: both themes always carry the year. The booking system
 * only opens a film's page about a week ahead, so the listing is short but
 * complete for what can be booked.
 */
class IksorisBookingClient(http: HttpFetch, page: IksorisBookingPage, override val cinema: Cinema) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(page.origin.value)
  override def sourceUrl: Option[String] = Some(page.url)

  def fetch(): Seq[CinemaMovie] = IksorisBookingClient.parse(HttpRead.page(http, page.url), page, cinema)
}

object IksorisBookingClient {

  // "Kanada, USA 89min" — comma-list countries, then the runtime glued to "min".
  private val DescPat = """^(.*?)\s*(\d+)\s*min\s*$""".r

  private val BookingLink = "a[href*=rezerwacja/numerowane.html]"

  // "Polska, 90 min, od lat 13, Dramat / Biograficzny" — countries before the
  // runtime; after it, an optional age rating and then the genres.
  private val TermsDescPat = """^(.*?)\s*(\d+)\s*min\b\s*,?\s*(.*)$""".r
  private val AgeRatingPat = """^od\s+lat\s+\d+\s*,?\s*(.*)$""".r

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    runtime:   Option[Int],
    countries: Seq[String],
    format:    List[String],
    poster:    Option[String],
    year:      Option[Int]    = None,
    genres:    Seq[String]    = Seq.empty
  )

  def parse(html: String, page: IksorisBookingPage, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, page.origin.value)
    val slots    = programmeSlots(document) ++ tableSlots(document) ++ termsListSlots(document)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = head.runtime, releaseYear = group.flatMap(_.year).headOption,
                          countries = head.countries, genres = head.genres),
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

  private def bookingOf(show: Element, link: String = BookingLink): Option[String] =
    Option(show.selectFirst(link)).map(_.attr("abs:href")).filter(_.nonEmpty)

  private def commaList(s: String): Seq[String] =
    s.split("[,/]").iterator.map(_.trim).filter(_.nonEmpty).toSeq

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
          (commaList(countriesPart), minutes.toIntOption)
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

  // ── Terms-list theme ───────────────────────────────────────────────────────

  private def termsListSlots(document: Document): Seq[RawSlot] =
    document.select("div.terms-in-date").asScala.toSeq.flatMap { day =>
      Option(day.selectFirst(".term-in-date-header")).map(_.text).flatMap(ScraperParse.parseDate).toSeq
        .flatMap(date => day.select(".term-in-date-term").asScala.toSeq.flatMap(termsListShow(_, date)))
    }

  private def termsListShow(show: Element, date: LocalDate): Option[RawSlot] =
    for {
      time  <- Option(show.selectFirst(".hour-wrapper")).map(_.text.trim).flatMap(ScraperParse.parseHHmm)
      block <- Option(show.selectFirst(".title-block"))
      title <- Some(block.text.trim).filter(_.nonEmpty)
    } yield {
      val (countries, runtime, genres) = Option(show.selectFirst(".description-block")).map(_.text.trim) match {
        case Some(TermsDescPat(countriesPart, minutes, rest)) =>
          val genres = rest match {
            case AgeRatingPat(genrePart) => commaList(genrePart)
            case _                       => Seq.empty
          }
          (commaList(countriesPart), minutes.toIntOption, genres)
        case _ => (Seq.empty, None, Seq.empty)
      }
      val tags = show.select(".event-tag").asScala.toSeq.map(_.text.trim)
      RawSlot(
        title     = title,
        dateTime  = LocalDateTime.of(date, time),
        booking   = bookingOf(show, "a.select-term-btn"),
        runtime   = runtime,
        countries = countries,
        format    = FormatTags.formatTokensIn(tags.mkString(" ")),
        poster    = None,
        year      = Option(block.selectFirst("a[href]")).flatMap(a => ScraperParse.filmwebSlugYear(a.attr("href"))),
        genres    = genres
      )
    }
}
