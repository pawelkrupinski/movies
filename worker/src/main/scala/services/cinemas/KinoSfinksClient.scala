package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Sfinks (Kraków, Nowa Huta) — the film screen run by Ośrodek Kultury im.
 * C.K. Norwida. The venue publishes a server-rendered "harmonogram" (schedule)
 * table at `/wydarzenia-harmonogram.html`, paginated (`-strona-N.html`) and
 * mixing film screenings with other cultural events.
 *
 * Each schedule row is a `<tr onclick="location.href=…">` carrying:
 *   - `td.info` → screening date (`DD-MM-YYYY`). The date cell is rendered ONCE
 *     per day and left EMPTY on the day's following rows, so the parser carries
 *     the last seen date forward.
 *   - `td.info` → start time (`HH:MM`).
 *   - `td.title strong` → the film title (kept verbatim — programme prefixes
 *     like "Tani wtorek:" / "Filmowy Klub Seniora i Seniorki:" are handled
 *     downstream by `TitleNormalizer.ProgrammePrefix`, not stripped here).
 *   - `td.kategorie` → category labels. Film screenings carry the `Seanse`
 *     category; this is the discriminator that drops non-film events.
 *   - `a.bilety` → the booking URL on the ticketing host (systembiletowy.pl).
 *
 * The listing alone has everything we surface; TMDB enriches the rest
 * downstream, so there's no per-film detail fetch. Pagination is followed by
 * the "next page" link, each extra page fetched tolerantly — a missing page
 * just contributes nothing (the recorded fixture is page 1 only).
 */
class KinoSfinksClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoSfinksClient._

  def fetch(): Seq[CinemaMovie] = {
    val firstHtml = http.get(PageUrl)
    val firstDoc  = Jsoup.parse(firstHtml)

    // Follow the "next page" chain off page 1. Extra pages are fetched in
    // parallel and tolerantly — a fetch failure (or a fixture that only
    // recorded page 1) drops that page rather than failing the scrape.
    val extraPaths = nextPagePaths(firstDoc)
    val extraDocs  = ParallelDetailFetch.keyed("kino-sfinks-pages", extraPaths, 1.minute)(p => BaseUrl + p) { url =>
      Try(Jsoup.parse(http.get(url))).toOption
    }
    val docs = firstDoc +: extraPaths.flatMap(p => extraDocs.getOrElse(p, None))

    val slots = docs.flatMap(parseDoc)
    val byTitle = slots.groupBy(_.title)

    byTitle.toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object KinoSfinksClient {

  val BaseUrl = "https://kinosfinks.okn.edu.pl"
  val PageUrl = s"$BaseUrl/wydarzenia-harmonogram.html"

  // The schedule lists film screenings under the "Seanse" category; other
  // cultural events on the same harmonogram lack it. This is the film filter.
  private val FilmCategory = "Seanse"

  private val DateFmt = DateTimeFormatter.ofPattern("dd-MM-yyyy")
  private val DatePat = """(\d{2}-\d{2}-\d{4})""".r
  private val NextPagePat = """/wydarzenia-harmonogram-strona-\d+\.html""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String])

  /** Distinct "next page" listing paths reachable from a page — the paginator
    * links every page (`-strona-2…6.html`), so collecting them off page 1
    * covers the whole schedule without walking the chain page by page. */
  private def nextPagePaths(doc: org.jsoup.nodes.Document): Seq[String] =
    doc.select("a[href]").asScala.toSeq
      .map(_.attr("href"))
      .flatMap(h => NextPagePat.findFirstIn(h))
      .distinct

  /** Parse one harmonogram page into its film screenings. */
  private def parseDoc(doc: org.jsoup.nodes.Document): Seq[RawSlot] = {
    var carriedDate: Option[LocalDate] = None
    doc.select("table.widok_listy tbody tr[onclick]").asScala.toSeq.flatMap { row =>
      // The date cell is only populated on the first row of each day; carry the
      // last seen date forward to the day's remaining rows.
      cellDate(row).foreach(d => carriedDate = Some(d))
      val time  = cellTime(row)
      val title = Option(row.selectFirst("td.title")).map(_.text.trim).filter(_.nonEmpty)
      val isFilm = row.select("td.kategorie span.label").asScala.exists(_.attr("title") == FilmCategory)

      (if (isFilm) title else None, carriedDate, time) match {
        case (Some(t), Some(date), Some(lt)) =>
          val booking = Option(row.selectFirst("a.bilety")).map(_.attr("href")).filter(_.nonEmpty)
          Some(RawSlot(t, LocalDateTime.of(date, lt), booking))
        case _ => None
      }
    }
  }

  /** First `td.info` carrying a `DD-MM-YYYY` date-caption, parsed. */
  private def cellDate(row: Element): Option[LocalDate] =
    row.select("td.info span.date-caption").asScala.iterator
      .map(_.text.trim)
      .flatMap(t => DatePat.findFirstIn(t))
      .flatMap(s => Try(LocalDate.parse(s, DateFmt)).toOption)
      .nextOption()

  /** First `td.info` carrying an `HH:MM` start time, parsed. */
  private def cellTime(row: Element): Option[java.time.LocalTime] =
    row.select("td.info span.date-caption").asScala.iterator
      .map(_.text.trim)
      .flatMap(ScraperParse.parseHHmm)
      .nextOption()
}
