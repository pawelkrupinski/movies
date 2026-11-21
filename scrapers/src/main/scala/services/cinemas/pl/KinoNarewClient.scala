package services.cinemas.pl

import java.time.LocalDate
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.ScraperParse
import services.cinemas.common.{ListingPageScraper, SlotsToMovies}
import services.titles.FormatTags
import tools.HttpFetch

/**
 * Kino Narew, the cinema hall of MCKiS Pułtusk. Its `mckispultusk.pl/kino-narew/`
 * page is a WordPress (Neve theme) page whose "REPERTUAR" section lists one
 * `div.wp-block-group.is-content-justification-space-between` per film, each
 * wrapping a first `div.wp-block-group…is-vertical` whose direct-child
 * `p.wp-block-paragraph`s run, in order:
 *   1. the title (large-font `<strong>`, shouted ALL-CAPS — run through
 *      [[ScraperParse.sentenceCase]])
 *   2. ONE OR MORE date-range lines, "D – D miesiąca YYYYr. (dzień – dzień)"
 *      — a film that screens across two separate windows at the same time
 *      (e.g. a bank-holiday gap) lists each range on its own line before the
 *      showtime
 *   3. the showtime, "godz. HH.MM/format[/dubbing|napisy]" (a literal dot as
 *      the time separator, not a colon)
 *   4. an em-dash separator, then `Gatunek:`/`Kraj:`/`Czas trwania:`/`Od lat:`
 *      fields packed into one `<br>`-joined paragraph
 *   5. ticket prices (skipped) and, after an "O filmie:" label, the synopsis
 *
 * Each date range expands to one showtime per day at the shared time
 * ([[ScraperParse.dailyRange]]) — the page has no per-day variation within a
 * range.
 */
class KinoNarewClient(http: HttpFetch, override val cinema: Cinema = KinoNarew)
    extends ListingPageScraper(http, KinoNarewClient.RepertoireUrl, KinoNarewClient.RepertoireUrl) {
  protected def parsePage(html: String): Seq[CinemaMovie] =
    KinoNarewClient.parse(html, cinema)
}

object KinoNarewClient {

  val BaseUrl       = "https://mckispultusk.pl"
  val RepertoireUrl = s"$BaseUrl/kino-narew/"

  // "18 – 23 września 2026r." — the day-range separator is a hyphen or an
  // en/em-dash; the second day is optional (a single-day listing).
  private val DateRange = """(\d{1,2})(?:\s*[–—-]\s*(\d{1,2}))?\s+(\p{L}+)\s+(\d{4})r\.""".r
  // "godz. 17.00/2D/dubbing" — the hour/minute separator is a literal dot.
  private val GodzTime  = raw"""godz\.?\s*${ScraperParse.ClockPartsDotted}""".r

  private case class RawSlot(movie: Movie, dateTime: java.time.LocalDateTime, format: List[String], synopsis: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val blocks = document.select("div.wp-block-group.is-content-justification-space-between").asScala.toSeq
    val slots = blocks.flatMap(parseFilm)

    SlotsToMovies.fold(slots, _.movie.title, s => Showtime(s.dateTime, None, format = s.format)) { (_, group, showtimes) =>
      CinemaMovie(movie = group.head.movie, cinema = cinema, synopsis = group.head.synopsis, showtimes = showtimes)
    }
  }

  private def parseFilm(block: Element): Seq[RawSlot] = {
    // `block` ITSELF carries the `wp-block-group` class (among others), so a
    // plain `selectFirst("div.wp-block-group")` matches `block` before its
    // nested "is-vertical" info column — the child combinator excludes self.
    val info = Option(block.selectFirst("> div.wp-block-group")).getOrElse(block)
    val paragraphs = info.select("> p.wp-block-paragraph").asScala.toSeq

    val ranges = paragraphs.drop(1).map(p => DateRange.findFirstMatchIn(p.text))
      .takeWhile(_.isDefined).flatten
      .flatMap { m =>
        for {
          month <- ScraperParse.polishMonth(m.group(3))
          year  <- Try(m.group(4).toInt).toOption
          from  <- Try(LocalDate.of(year, month, m.group(1).toInt)).toOption
          to     = Option(m.group(2)).flatMap(d => Try(LocalDate.of(year, month, d.toInt)).toOption).getOrElse(from)
        } yield (from, to)
      }

    val godzParagraph = paragraphs.find(p => p.text.trim.toLowerCase(Locale.ROOT).startsWith("godz"))
    val time = godzParagraph.flatMap(p => GodzTime.findFirstMatchIn(p.text))
      .flatMap(m => ScraperParse.clockAt(m, 1))

    (paragraphs.headOption.map(_.text.trim).filter(_.nonEmpty), time) match {
      case (Some(rawTitle), Some(t)) if ranges.nonEmpty =>
        val title  = ScraperParse.sentenceCase(rawTitle)
        val format = godzParagraph.map(p => FormatTags.formatTokensIn(p.text)).getOrElse(Nil)

        val fields = paragraphs.find(p => p.text.contains("Gatunek")).map(fieldsOf).getOrElse(Map.empty)
        val genres = fields.get("gatunek").map(ScraperParse.commaItems).getOrElse(Seq.empty)
        val (countries, year) = fields.get("kraj").map(ScraperParse.productionMeta).getOrElse((Nil, None))
        val runtime = fields.get("czas trwania").flatMap(v => """\d+""".r.findFirstIn(v)).flatMap(n => Try(n.toInt).toOption)

        val synopsisIndex = paragraphs.indexWhere(_.text.trim.equalsIgnoreCase("O filmie:"))
        val synopsis = if (synopsisIndex >= 0) paragraphs.lift(synopsisIndex + 1).map(_.text.trim).filter(_.nonEmpty) else None

        ranges.flatMap { case (from, to) => ScraperParse.dailyRange(from, to, t) }
          .map(dt => RawSlot(Movie(title, runtime, year, countries = countries, genres = genres), dt, format, synopsis))
      case _ => Seq.empty
    }
  }

  /** "Gatunek: Komedia, Akcja<br>Kraj: Polska/2026<br>Czas trwania: 110 min.<br>Od lat: 7"
   *  packed into one `<br>`-separated paragraph, split via [[ScraperParse.linesOf]]
   *  into a lower-cased label → value map. */
  private def fieldsOf(p: Element): Map[String, String] =
    ScraperParse.linesOf(p).flatMap { line =>
      val idx = line.indexOf(':')
      if (idx < 0) None else Some(line.take(idx).trim.toLowerCase(Locale.ROOT) -> line.drop(idx + 1).trim)
    }.toMap
}
