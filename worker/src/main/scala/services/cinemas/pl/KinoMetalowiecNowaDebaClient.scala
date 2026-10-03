package services.cinemas.pl

import java.util.Locale

import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import models._
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}

import java.text.Normalizer
import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Kino Metalowiec, run by Samorządowy Ośrodek Kultury w Nowej Dębie. Its
 * programme is one WordPress/Elementor post, `/repertuar-kina-metalowiec/`, in two
 * parts that must be joined:
 *
 *   - THE SCHEDULE — a sidebar text widget (`aside p`), one `<br>`-separated run
 *     per day: a `<strong>26 września</strong>` header (no year), then
 *     `15:30 Asterix i Obelix. Misja Kleopatra` lines. This is the only place a
 *     screening has a TIME, so it alone decides which films and showtimes are
 *     emitted. Some titles link to their Filmweb page
 *     (`filmweb.pl/film/Mistyczka-2026-10125135`), whose slug carries the
 *     production year; the widget's copy-pasted markup sometimes wraps one film's
 *     link around another's (`<a Historie…><a Mistyczka>Mistyczka</a></a>`), so a
 *     year is only taken from a link whose own text is the film.
 *   - THE FILM BLOCKS — the post body, films separated by Elementor divider
 *     widgets. A block is a shouted title (`<p><strong>MISTYCZKA</strong></p>`),
 *     a poster image, a details widget (`DATA WYŚWIETLANIA W KINIE METALOWIEC:`
 *     day list, `OPIS FILMU:`, `GATUNEK:`, `CZAS TRWANIA: 90 min.`,
 *     `OGRANICZENIE WIEKOWE: 12+`) and a YouTube video widget. The day lists have
 *     no times, so the blocks only enrich the schedule's films (joined on the
 *     title's letters and digits: the schedule's `Asterix i Obelix. Misja
 *     Kleopatra` is the block's `ASTERIX I OBELIX: MISJA KLEOPATRA`). A block
 *     without a title (a poster-only heading) is skipped rather than guessed.
 *
 * The schedule is the week in progress; films announced further ahead appear
 * only as blocks with dates but no times until the schedule reaches their week. Tickets are
 * sold at the box office only, so no booking URL is surfaced.
 */
class KinoMetalowiecNowaDebaClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoMetalowiecNowaDeba,
  today: => LocalDate
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoMetalowiecNowaDebaClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoMetalowiecNowaDebaClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoMetalowiecNowaDebaClient.parse(HttpRead.page(http, KinoMetalowiecNowaDebaClient.RepertoireUrl), today, cinema)
}

object KinoMetalowiecNowaDebaClient {

  val RepertoireUrl = "https://www.soknowadeba.pl/repertuar-kina-metalowiec/"

  private val DayHeader    = """^(\d{1,2})\s+(\p{L}+)$""".r
  private val Screening    = """^(\d{1,2}:\d{2})\s+(.+)$""".r
  private val Minutes      = """(\d+)\s*min""".r
  private val AgeYears     = """(\d+)\s*\+""".r
  private val YouTubeUrl   = """"youtube_url":"([^"]+)"""".r
  private val DetailsMark  = "DATA WYŚWIETLANIA"
  // The film block's `<strong>` field labels, upper-cased as parsed.
  private val SynopsisLabel = "OPIS FILMU"
  private val GenreLabel    = "GATUNEK"
  private val RuntimeLabel  = "CZAS TRWANIA"
  private val AgeLabel      = "OGRANICZENIE WIEKOWE"

  private case class RawSlot(title: String, dateTime: LocalDateTime)

  private case class FilmBlock(
    title:     String,
    posterUrl: Option[String],
    synopsis:  Option[String],
    genres:    Seq[String],
    runtime:   Option[Int],
    age:       Option[String],
    trailer:   Option[String]
  )

  def parse(html: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, RepertoireUrl)
    val blocks   = filmBlocks(document).map(b => keyOf(b.title) -> b).toMap
    val years    = filmwebYears(document)

    SlotsToMovies.fold(scheduleSlots(document, today), _.title, s => Showtime(s.dateTime, None)) { (title, _, showtimes) =>
      val block = blocks.get(keyOf(title))
      CinemaMovie(
        movie      = Movie(
          title          = title,
          runtimeMinutes = block.flatMap(_.runtime),
          releaseYear    = years.get(keyOf(title)),
          genres         = block.map(_.genres).getOrElse(Seq.empty)
        ),
        cinema     = cinema,
        posterUrl  = block.flatMap(_.posterUrl),
        filmUrl    = None,
        synopsis   = block.flatMap(_.synopsis),
        cast       = Seq.empty,
        director   = Seq.empty,
        showtimes  = showtimes,
        trailerUrl = block.flatMap(_.trailer),
        ageRating  = block.flatMap(_.age)
      )
    }
  }

  /** The sidebar schedule: day headers and `HH:MM Title` lines, in order. */
  private def scheduleSlots(document: Document, today: LocalDate): Seq[RawSlot] =
    document.select("aside p").asScala.toSeq.flatMap { p =>
      var currentDate: Option[LocalDate] = None
      ScraperParse.linesOf(p).flatMap {
        case line @ DayHeader(_, _) =>
          currentDate = ScraperParse.parseDayMonth(line).flatMap(ScraperParse.upcomingDate(_, today))
          None
        case Screening(time, title) =>
          for {
            date <- currentDate
            tm   <- ScraperParse.parseHHmm(time)
          } yield RawSlot(title.trim, LocalDateTime.of(date, tm))
        case _ => None
      }
    }

  /** Production years from the schedule's Filmweb links, keyed by the film the
   *  link's OWN text names (an empty wrapping link names nothing). */
  private def filmwebYears(document: Document): Map[String, Int] =
    document.select("aside a[href*=filmweb.pl/film/]").asScala.toSeq.flatMap { a =>
      ScraperParse.filmwebSlugYear(a.attr("href")).map(keyOf(a.ownText) -> _)
    }.filter(_._1.nonEmpty).toMap

  /** The post body's film blocks: its Elementor widgets, split at the dividers. */
  private def filmBlocks(document: Document): Seq[FilmBlock] =
    document.select("article .elementor-widget").asScala.toSeq
      .foldLeft(Vector(Vector.empty[Element])) { (groups, widget) =>
        if (widget.hasClass("elementor-widget-divider")) groups :+ Vector.empty
        else groups.init :+ (groups.last :+ widget)
      }
      .flatMap(blockOf)

  /** One block, or None when it has no title (or no details widget). */
  private def blockOf(widgets: Seq[Element]): Option[FilmBlock] = {
    val (details, titles) = widgets.filter(_.hasClass("elementor-widget-text-editor")).partition(_.text.contains(DetailsMark))
    for {
      title   <- titles.map(_.text.trim).find(_.nonEmpty)
      details <- details.headOption
    } yield {
      // `<p><strong>LABEL:</strong> value</p>`; the label may be `<b>` and end in a `<br>`.
      val fields = details.select("p").asScala.toSeq.flatMap { p =>
        Option(p.selectFirst("strong, b")).map(_.text.trim.stripSuffix(":").trim.toUpperCase(Locale.ROOT) -> p.ownText.trim)
      }.toMap
      FilmBlock(
        title     = title,
        posterUrl = widgets.filter(_.hasClass("elementor-widget-image"))
                      .flatMap(w => Option(w.selectFirst("img[src]"))).map(_.attr("abs:src")).find(_.nonEmpty),
        synopsis  = fields.get(SynopsisLabel).filter(_.nonEmpty),
        genres    = fields.get(GenreLabel).toSeq.flatMap(_.split("/")).map(_.trim).filter(_.nonEmpty),
        runtime   = fields.get(RuntimeLabel).flatMap(Minutes.findFirstMatchIn).map(_.group(1).toInt),
        // "b/o" (no restriction) is a marker, not a rating, so only "N+" counts.
        age       = fields.get(AgeLabel).flatMap(AgeYears.findFirstMatchIn).map(m => s"${m.group(1)}+"),
        trailer   = widgets.filter(_.hasClass("elementor-widget-video"))
                      .flatMap(w => YouTubeUrl.findFirstMatchIn(w.attr("data-settings")))
                      .flatMap(m => ScraperParse.canonicalTrailer(m.group(1).replace("\\/", "/")))
                      .headOption
      )
    }
  }

  /** The join key: lower-cased letters and digits only, diacritics kept. */
  private def keyOf(title: String): String =
    Normalizer.normalize(title, Normalizer.Form.NFC).toLowerCase(Locale.ROOT).filter(_.isLetterOrDigit)
}
