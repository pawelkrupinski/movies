package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDate
import scala.jdk.CollectionConverters._
import scala.util.Try

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
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoNarewClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoNarewClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoNarewClient.parse(http.get(KinoNarewClient.RepertoireUrl), cinema)
}

object KinoNarewClient {

  val BaseUrl       = "https://mckispultusk.pl"
  val RepertoireUrl = s"$BaseUrl/kino-narew/"

  // "18 – 23 września 2026r." — the day-range separator is a hyphen or an
  // en/em-dash; the second day is optional (a single-day listing).
  private val DateRange = """(\d{1,2})(?:\s*[–—-]\s*(\d{1,2}))?\s+(\p{L}+)\s+(\d{4})r\.""".r
  // "godz. 17.00/2D/dubbing" — the hour/minute separator is a literal dot.
  private val GodzTime  = """godz\.?\s*(\d{1,2})[.:](\d{2})""".r

  private case class RawSlot(
    title:     String,
    dateTime:  java.time.LocalDateTime,
    format:    List[String],
    runtime:   Option[Int],
    countries: Seq[String],
    year:      Option[Int],
    genres:    Seq[String],
    synopsis:  Option[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val blocks = document.select("div.wp-block-group.is-content-justification-space-between").asScala.toSeq
    val slots = blocks.flatMap(parseFilm)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None, format = s.format)) { (title, group, showtimes) =>
      val first = group.head
      CinemaMovie(
        movie     = Movie(
          title          = title,
          runtimeMinutes = first.runtime,
          releaseYear    = first.year,
          countries      = first.countries,
          genres         = first.genres
        ),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = first.synopsis,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
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

    val godzParagraph = paragraphs.find(p => p.text.trim.toLowerCase.startsWith("godz"))
    val time = godzParagraph.flatMap(p => GodzTime.findFirstMatchIn(p.text))
      .flatMap(m => Try(java.time.LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)

    (paragraphs.headOption.map(_.text.trim).filter(_.nonEmpty), time) match {
      case (Some(rawTitle), Some(t)) if ranges.nonEmpty =>
        val title  = ScraperParse.sentenceCase(rawTitle)
        val format = godzParagraph.map(p => ScraperParse.formatTokensIn(p.text)).getOrElse(Nil)

        val fields = paragraphs.find(p => p.text.contains("Gatunek")).map(fieldsOf).getOrElse(Map.empty)
        val genres = fields.get("gatunek").map(_.split(",").toSeq.map(_.trim).filter(_.nonEmpty)).getOrElse(Seq.empty)
        val (countries, year) = fields.get("kraj").map(ScraperParse.productionMeta).getOrElse((Nil, None))
        val runtime = fields.get("czas trwania").flatMap(v => """\d+""".r.findFirstIn(v)).flatMap(n => Try(n.toInt).toOption)

        val synopsisIndex = paragraphs.indexWhere(_.text.trim.equalsIgnoreCase("O filmie:"))
        val synopsis = if (synopsisIndex >= 0) paragraphs.lift(synopsisIndex + 1).map(_.text.trim).filter(_.nonEmpty) else None

        ranges.flatMap { case (from, to) => ScraperParse.dailyRange(from, to, t) }
          .map(dt => RawSlot(title, dt, format, runtime, countries, year, genres, synopsis))
      case _ => Seq.empty
    }
  }

  /** "Gatunek: Komedia, Akcja<br>Kraj: Polska/2026<br>Czas trwania: 110 min.<br>Od lat: 7"
   *  packed into one `<br>`-separated paragraph, split via [[ScraperParse.linesOf]]
   *  into a lower-cased label → value map. */
  private def fieldsOf(p: Element): Map[String, String] =
    ScraperParse.linesOf(p).flatMap { line =>
      val idx = line.indexOf(':')
      if (idx < 0) None else Some(line.take(idx).trim.toLowerCase -> line.drop(idx + 1).trim)
    }.toMap
}
