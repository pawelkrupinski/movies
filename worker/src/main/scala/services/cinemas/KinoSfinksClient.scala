package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.{CachingDetailFetch, HttpFetch, ParallelDetailFetch}

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
 *   - `tr[onclick="location.href=('…szczegoly….html')"]` → the per-screening
 *     detail page. The listing carries title + showtime only; the detail page
 *     adds runtime, genres, production country/year, director and cast (in a
 *     `div.box-iobiekt.<field>` block). Those are deferred to an `EnrichDetails`
 *     task via [[fetchFilmDetail]].
 *
 * Pagination is followed by the "next page" link, each extra page fetched
 * tolerantly — a missing page just contributes nothing (the recorded fixture
 * is page 1 only).
 *
 * `defersTmdbResolution = false`: the row resolves straight from its clean
 * title (the listing carries no identity hints), and the detail metadata —
 * including the production year — merges in asynchronously when the detail
 * fetch lands. We don't gate the read model on a per-film fetch here: the
 * detail pages are per-screening and short-lived (past screenings 404), so a
 * blocking dependency would strand rows whenever a page expired.
 */
class KinoSfinksClient(http: HttpFetch, override val cinema: Cinema)
    extends CinemaScraper with DetailEnricher {

  import KinoSfinksClient._

  // Detail pages are static across passes for a live screening, so cache them.
  private val detailHttp = new CachingDetailFetch(http)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  override val detailGroup: String = "kino-sfinks"
  // The listing has no identity hints; resolve from the title and merge the
  // detail (year/director/…) in asynchronously rather than blocking on it.
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail — runtime, genres, country, production year,
   *  director and cast off the `box-iobiekt` block. None on a fetch failure so
   *  the task stays stale and retries rather than recording an empty result. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map(html => parseDetail(Jsoup.parse(html)))

  def fetch(): Seq[CinemaMovie] = {
    val firstHtml = http.get(PageUrl)
    val firstDocument  = Jsoup.parse(firstHtml)

    // Follow the "next page" chain off page 1. Extra pages are fetched in
    // parallel and tolerantly — a fetch failure (or a fixture that only
    // recorded page 1) drops that page rather than failing the scrape.
    val extraPaths = nextPagePaths(firstDocument)
    val extraDocuments  = ParallelDetailFetch.keyed("kino-sfinks-pages", extraPaths, 1.minute)(p => BaseUrl + p) { url =>
      Try(Jsoup.parse(http.get(url))).toOption
    }
    val documents = firstDocument +: extraPaths.flatMap(p => extraDocuments.getOrElse(p, None))

    val slots = documents.flatMap(parseDocument)
    val byTitle = slots.groupBy(_.title)

    byTitle.toSeq.flatMap { case (title, group) =>
      val ordered = group.sortBy(_.dateTime)
      val showtimes = ordered
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        // The earliest screening's detail page — the metadata is film-level, so
        // any one screening's page serves; pick deterministically.
        filmUrl   = ordered.flatMap(_.detailUrl).headOption,
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
  // The row's `onclick="location.href = ('/wydarzenie-…-szczegoly-….html')"`.
  private val DetailHrefPat = """location\.href\s*=\s*\(?\s*'([^']+szczegoly[^']+)'""".r
  // Any in-range 4-digit production year (trailing token of "USA 1999").
  private val YearTokenPat  = """\b(?:19|20)\d{2}\b""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String],
                             detailUrl: Option[String])

  /** Distinct "next page" listing paths reachable from a page — the paginator
    * links every page (`-strona-2…6.html`), so collecting them off page 1
    * covers the whole schedule without walking the chain page by page. */
  private def nextPagePaths(document: org.jsoup.nodes.Document): Seq[String] =
    document.select("a[href]").asScala.toSeq
      .map(_.attr("href"))
      .flatMap(h => NextPagePat.findFirstIn(h))
      .distinct

  /** Parse one harmonogram page into its film screenings. */
  private def parseDocument(document: org.jsoup.nodes.Document): Seq[RawSlot] = {
    var carriedDate: Option[LocalDate] = None
    document.select("table.widok_listy tbody tr[onclick]").asScala.toSeq.flatMap { row =>
      // The date cell is only populated on the first row of each day; carry the
      // last seen date forward to the day's remaining rows.
      cellDate(row).foreach(d => carriedDate = Some(d))
      val time  = cellTime(row)
      val title = Option(row.selectFirst("td.title")).map(_.text.trim).filter(_.nonEmpty)
      val isFilm = row.select("td.kategorie span.label").asScala.exists(_.attr("title") == FilmCategory)

      (if (isFilm) title else None, carriedDate, time) match {
        case (Some(t), Some(date), Some(lt)) =>
          val booking = Option(row.selectFirst("a.bilety")).map(_.attr("href")).filter(_.nonEmpty)
          val detail  = DetailHrefPat.findFirstMatchIn(row.attr("onclick")).map { m =>
            val href = m.group(1)
            if (href.startsWith("http")) href else BaseUrl + href
          }
          Some(RawSlot(t, LocalDateTime.of(date, lt), booking, detail))
        case _ => None
      }
    }
  }

  // The detail page renders each metadata field as
  // `<div class="box-iobiekt <field>"><div class="obiekt_typ">Label:</div>
  //  <div class="obiekt_dane">VALUE</div></div>`. Read the value cell by the
  // field's class so the surrounding "related screenings" listing (which has no
  // such block) can't bleed in.
  private def fieldValue(document: Document, field: String): Option[String] =
    Option(document.selectFirst(s"div.box-iobiekt.$field div.obiekt_dane"))
      .map(_.text.trim).filter(_.nonEmpty)

  private def splitList(s: Option[String]): Seq[String] =
    s.toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)

  /** Parse a screening detail page into its film-level metadata. `Produkcja /
   *  rok` folds country + year ("USA 1999") — the trailing 4-digit run is the
   *  year, the rest the production countries. */
  private[cinemas] def parseDetail(document: Document): FilmDetail = {
    val prod      = fieldValue(document, "produkcja_rok")
    val year      = prod.flatMap(p => YearTokenPat.findFirstMatchIn(p).map(_.matched.toInt))
    val countries = splitList(prod.map(p => YearTokenPat.replaceAllIn(p, "").trim))
    val runtime   = fieldValue(document, "czas_trwania")
      .flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)).filter(n => n >= 30 && n <= 300)
    val poster    = Option(document.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty)
    FilmDetail(
      cast           = splitList(fieldValue(document, "obsada_wykonawcy")),
      director       = splitList(fieldValue(document, "rezyseria")),
      runtimeMinutes = runtime,
      releaseYear    = year,
      countries      = countries,
      genres         = splitList(fieldValue(document, "gatunek")),
      posterUrl      = poster
    )
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
