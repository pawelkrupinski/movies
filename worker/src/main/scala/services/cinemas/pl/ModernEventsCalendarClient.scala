package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import play.api.libs.json.{JsObject, JsString, Json}
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScraperParse, SlotsToMovies}
import services.movies.FormatTags
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime, YearMonth, ZoneId}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/** The WordPress page on which a venue embeds its Modern Events Calendar
 *  shortcode. The shortcode's own attributes (skin, category filter) travel
 *  with the page, so the page alone says which events are the cinema's. */
final case class ModernEventsCalendarPage(url: String)

/**
 * Venues whose WordPress site runs the Modern Events Calendar plugin (MEC,
 * webnus) and shows its screenings on one of MEC's calendar skins — MOK
 * Zambrów (`daily_view`) and NCK Sokół Nisko (`monthly_view`, filtered to the
 * cinema's category).
 *
 * MEC has no feed both venues expose: the plugin's REST route is off at
 * Zambrów and empty at Nisko, and its iCal export is per event and demands a
 * nonce. The calendar HTML is the common ground, in two parts:
 *
 *  1. The page renders the CURRENT month server-side, and an init script
 *     (`jQuery(…).mecDailyView({ … })` / `.mecMonthlyView({ … })`) carries the
 *     shortcode's serialised `atts`, the `admin-ajax.php` URL and the month
 *     shown. Every later month is the same skin re-rendered by
 *     `POST admin-ajax.php action=mec_<skin>_load_month&mec_year=…&mec_month=…&<atts>`
 *     — exactly what the calendar's own "next month" arrow sends — answering
 *     JSON whose string fields are HTML fragments (`month`, and for the monthly
 *     skin `events_side`). Months are loaded forward until one has no
 *     screenings of its own (the monthly grid also shows the neighbouring
 *     months' edge days, which don't count), so the scrape reaches as far as
 *     the venue has scheduled.
 *  2. In both skins each occurrence is an `article.mec-event-article` inside a
 *     day container naming its date as `yyyyMMdd` — the monthly skin's
 *     `data-mec-cell`, the daily skin's `id="…_20260925"`. A recurring event is
 *     already expanded into one article per day. The article's
 *     `.mec-event-time` is "3:30 pm" (daily) or "16:00 - 17:00" (monthly), and
 *     `.mec-event-title a` the event's title and page.
 *
 * Title quirks: Zambrów files each (film, time) pair as its own event titled
 * "15:30 – Mistyczka" — the leading time is peeled off; Nisko suffixes
 * "[PREMIERA] [DUBBING]" tags — peeled by [[FormatTags.extractFormatTags]], the
 * language ones becoming the showing's badge. No booking link is published.
 *
 * Per-film detail (deferred) is the event's page: Nisko's is MEC's single-event
 * view, whose description ends in "gatunek: …", "czas trwania: 2 godz. 50 min",
 * "produkcja: Polska" lines; Zambrów's links to a hand-built post whose excerpt
 * (`og:description`) has "Czas trwania - 85 min", "Kraj i rok produkcji -
 * Polska, 2026". Both are read as labelled lines, plus synopsis paragraphs,
 * `og:image` poster and an embedded YouTube trailer.
 */
class ModernEventsCalendarClient(
  http:  HttpFetch,
  page:  ModernEventsCalendarPage,
  override val cinema: Cinema,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with DetailEnricher with OnlyMovieEventsFilter {

  import ModernEventsCalendarClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(page.url)
  override def sourceUrl: Option[String] = Some(page.url)

  override val detailGroup: String = s"modern-events-calendar-${cinema.slug}"

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val html     = http.get(page.url)
    val calendar = calendarOf(html).getOrElse(
      throw new IllegalStateException(s"no Modern Events Calendar skin initialised on ${page.url}"))

    val later = Iterator.iterate(calendar.month.plusMonths(1))(_.plusMonths(1))
      .take(MaxMonthsAhead)
      .map(month => month -> slotsIn(monthFragments(http.post(calendar.ajaxUrl, calendar.loadMonthBody(month),
        "application/x-www-form-urlencoded"))))
      .takeWhile { case (month, slots) => slots.exists(slot => YearMonth.from(slot.dateTime) == month) }
      .flatMap(_._2)
      .toSeq

    toMovies((slotsIn(html) ++ later).filterNot(_.dateTime.toLocalDate.isBefore(today)), cinema)
  }

  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map(parseDetail)
}

object ModernEventsCalendarClient {

  /** A loop guard, not a horizon: loading stops at the first month with no
   *  screenings, which a small cinema reaches within two or three. */
  private val MaxMonthsAhead = 12

  /** The skin's init call: `.mecDailyView({ … })` — its options object. */
  private val InitScript = """(?s)\.mec\w+View\(\s*\{(.*?)\}\s*\);""".r
  private def option(name: String) = s"""$name:\\s*"([^"]*)"""".r

  private val DayId       = """(?:^|_)(\d{8})$""".r
  private val DayFormat   = DateTimeFormatter.BASIC_ISO_DATE
  private val ClockTime   = """(?i)(\d{1,2}):(\d{2})\s*([ap]\.?m\.?)?""".r
  private val LeadingTime = """^\d{1,2}[:.]\d{2}\s*[–—-]\s*""".r

  /** The calendar's AJAX handle: which skin, its serialised shortcode `atts`,
   *  the endpoint, and the month the page itself rendered. */
  private final case class Calendar(skin: String, atts: String, ajaxUrl: String, month: YearMonth) {
    def loadMonthBody(target: YearMonth): String =
      s"action=mec_${skin}_load_month&mec_year=${target.getYear}&mec_month=${f"${target.getMonthValue}%02d"}" +
        s"&$atts&apply_sf_date=0&sed=0"
  }

  private final case class Slot(title: String, dateTime: LocalDateTime, format: List[String], eventUrl: String)

  private def calendarOf(html: String): Option[Calendar] =
    InitScript.findFirstMatchIn(html).map(_.group(1)).flatMap { options =>
      def value(name: String) = option(name).findFirstMatchIn(options).map(_.group(1))
      for {
        atts  <- value("atts")
        skin  <- """atts%5Bskin%5D=([a-z_]+)""".r.findFirstMatchIn(atts).map(_.group(1))
        ajax  <- value("ajax_url")
        year  <- value("year").flatMap(_.toIntOption)
        month <- value("month").flatMap(_.toIntOption)
      } yield Calendar(skin, atts, ajax, YearMonth.of(year, month))
    }

  /** The HTML fragments of a `mec_<skin>_load_month` answer, joined — which
   *  field carries the day lists differs by skin, and the others hold none. */
  private def monthFragments(json: String): String =
    Json.parse(json).asOpt[JsObject].toSeq.flatMap(_.values).collect { case JsString(fragment) => fragment }.mkString("\n")

  private def slotsIn(html: String): Seq[Slot] =
    Jsoup.parse(html).select("article.mec-event-article").asScala.toSeq.flatMap(slotOf)

  private def slotOf(article: Element): Option[Slot] =
    for {
      link     <- Option(article.selectFirst(".mec-event-title a[href]"))
      date     <- dayOf(article)
      rawTitle  = link.text.trim
      time     <- Option(article.selectFirst(".mec-event-time")).map(_.text).flatMap(clockTime)
                    .orElse(clockTime(rawTitle))
      (title, format) = FormatTags.extractFormatTags(LeadingTime.replaceFirstIn(rawTitle, ""))
      if title.nonEmpty
    } yield Slot(title, date.atTime(time), format, link.attr("abs:href"))

  /** The date of the day container an occurrence sits in. */
  private def dayOf(article: Element): Option[LocalDate] =
    article.parents.asScala.iterator.flatMap { parent =>
      Seq(parent.attr("data-mec-cell"), parent.id).flatMap(DayId.findFirstMatchIn(_).map(_.group(1)))
    }.nextOption().flatMap(day => Try(LocalDate.parse(day, DayFormat)).toOption)

  /** The first clock time in `text`, 24-hour or with an am/pm marker. */
  private def clockTime(text: String): Option[LocalTime] =
    ClockTime.findFirstMatchIn(text).flatMap { m =>
      val hour = m.group(1).toInt
      val pm   = Option(m.group(3)).exists(_.toLowerCase.startsWith("p"))
      val am   = Option(m.group(3)).exists(_.toLowerCase.startsWith("a"))
      val hour24 = if (pm && hour < 12) hour + 12 else if (am && hour == 12) 0 else hour
      Try(LocalTime.of(hour24, m.group(2).toInt)).toOption
    }

  private def toMovies(slots: Seq[Slot], cinema: Cinema): Seq[CinemaMovie] =
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None, None, s.format)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = Some(group.head.eventUrl),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }

  // ── Detail page ────────────────────────────────────────────────────────────

  /** "czas trwania: 2 godz. 50 min", "Czas trwania - 85 min": a short label,
   *  then a colon or a spaced dash, then the value. A synopsis sentence of the
   *  same shape ("Dzień dziecka – diagnoza choroby…") parses too, harmlessly:
   *  only the known labels are ever looked up. The field lines are all shorter
   *  than [[SynopsisMinLength]], which is what keeps them out of the synopsis. */
  private val LabelledLine = """^\s*([\p{L} ]{2,25}?)\s*(?::|\s[-–]\s)\s*(.+?)\s*$""".r
  private val HoursMinutes = """(?i)(?:(\d+)\s*godz\.?)?\s*(?:(\d+)\s*min)?""".r
  private val SynopsisMinLength = 60

  private def labelled(lines: Seq[String]): Map[String, String] =
    lines.flatMap {
      case LabelledLine(label, value) => Some(label.trim.toLowerCase(java.util.Locale.ROOT) -> value)
      case _                          => None
    }.reverse.toMap

  private def runtimeOf(value: String): Option[Int] =
    HoursMinutes.findFirstMatchIn(value.trim).flatMap { m =>
      val hours   = Option(m.group(1)).map(_.toInt)
      val minutes = Option(m.group(2)).map(_.toInt)
      if (hours.isEmpty && minutes.isEmpty) None else Some(hours.getOrElse(0) * 60 + minutes.getOrElse(0))
    }.filter(_ > 0)

  private def listOf(value: String): Seq[String] =
    value.split("[,/]").iterator.map(_.trim).filter(_.nonEmpty).toSeq

  private def parseDetail(html: String): FilmDetail = {
    val document  = Jsoup.parse(html)
    val body      = Option(document.selectFirst(".mec-single-event-description"))
                      .orElse(Option(document.selectFirst(".entry-content")))
    val paragraphs = body.toSeq.flatMap(_.select("p").asScala)
                      .filterNot(p => p.closest(".widget, form") != null)
                      .map(_.text.trim).filter(_.nonEmpty)
    val excerpt   = meta(document, "og:description").toSeq.flatMap(_.split("\n")).map(_.trim).filter(_.nonEmpty)
    val fields    = labelled(excerpt ++ paragraphs)
    def field(labels: String*): Option[String] = labels.iterator.flatMap(fields.get).nextOption()

    // "Polska, 2026" and "Belgia, Francja (2026)" — the year's parentheses dropped
    // so they don't cling to the last country.
    val production = field("produkcja", "kraj produkcji", "kraj i rok produkcji")
                       .map(value => ScraperParse.productionMeta(value.replaceAll("[()]", " ")))
    FilmDetail(
      synopsis       = Some(paragraphs.filter(_.length >= SynopsisMinLength).mkString("\n\n")).filter(_.nonEmpty),
      director       = field("reżyseria", "reżyser").toSeq.flatMap(listOf),
      runtimeMinutes = field("czas trwania").flatMap(runtimeOf),
      releaseYear    = production.flatMap(_._2),
      countries      = production.toSeq.flatMap(_._1),
      genres         = field("gatunek").toSeq.flatMap(listOf),
      posterUrl      = meta(document, "og:image"),
      trailerUrl     = body.flatMap(trailerIn)
    )
  }

  private def meta(document: Document, property: String): Option[String] =
    Option(document.selectFirst(s"meta[property=$property]")).map(_.attr("content").trim).filter(_.nonEmpty)

  private def trailerIn(body: Element): Option[String] =
    body.select("iframe[src], [data-video_id]").asScala.iterator.flatMap { embed =>
      Option(embed.attr("data-video_id")).filter(_.nonEmpty).map(id => s"https://www.youtube.com/embed/$id")
        .orElse(Some(embed.attr("src")))
    }.flatMap(ScraperParse.canonicalTrailer).nextOption()
}
