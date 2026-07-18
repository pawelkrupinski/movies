package services.cinemas.pl

import org.jsoup.nodes.{Document, Element}
import models._
import tools.{CachingDetailFetch, HttpFetch, ParallelDetailFetch}
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail}

import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KINOkawiarnia Stacja Falenica (Warszawa). The `/repertuar/` page lists each
 * film (title, runtime + director, poster) linking to `/filmy/<slug>/`, whose
 * "Dostępne terminy" table holds the screenings (absolute DD.MM.YYYY date +
 * time + a systembiletowy booking link) and the full synopsis.
 *
 * Two-phase fetch: the repertoire listing yields one entry per film with title,
 * runtime, director, poster, and the per-film detail-page URL stored in
 * `filmUrl`. The detail page is fetched per film for showtimes (always) and
 * for synopsis + trailerUrl (via `fetchFilmDetail`).
 */
class FalenicaClient(http: HttpFetch) extends CinemaScraper with DetailEnricher {

  // Static film detail pages cached across passes; the repertoire listing keeps
  // the live `http` since its showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = StacjaFalenica

  private val BaseUrl    = "https://stacjafalenica.pl"
  private val ListingUrl = s"$BaseUrl/repertuar/"
  private val SlugPat    = """/filmy/([^/"]+)/""".r
  private val DateFmt    = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  private case class Film(slug: String, title: String, format: List[String], runtime: Option[Int], director: Seq[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    // A WordPress `__trashed-N` slug isn't necessarily dead: the venue trashes
    // the editorial post but the film keeps live "Dostępne terminy" (Romeria,
    // Znaki Pana Śliwki did, with future showtimes). Don't exclude by slug —
    // the `showtimes.isEmpty` drop below already removes genuinely-dead pages.
    val films = Jsoup.parse(http.get(ListingUrl)).select("article.filmy").asScala.toSeq.flatMap(parseListItem)
      .distinctBy(_.slug)

    val pages = ParallelDetailFetch.keyed("falenica-details", films.map(_.slug), 1.minute)(s => s"$BaseUrl/filmy/$s/") { url =>
      Try(http.get(url)).toOption.map(Jsoup.parse)
    }

    films.flatMap { f =>
      val detail    = pages.getOrElse(f.slug, None)
      val showtimes = detail.toSeq.flatMap(parseShowtimes).map(_.copy(format = f.format))
        .distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = f.title, runtimeMinutes = f.runtime, releaseYear = None),
        cinema    = cinema,
        posterUrl = f.poster,
        filmUrl   = Some(s"$BaseUrl/filmy/${f.slug}/"),
        synopsis  = None,
        cast      = Seq.empty,
        director  = f.director,
        showtimes = showtimes,
        trailerUrl = None
      ))
    }
  }

  override val detailGroup: String = "falenica"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl. Provides synopsis and trailerUrl from the detail page.
   *  None on a fetch failure so the task stays stale and is retried by the next
   *  scrape rather than recording an empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val document = Jsoup.parse(html)
      FilmDetail(
        // `div.section.tresc` wraps the synopsis prose alongside the
        // "Dostępne terminy" showtime table and the trailer's <video><a> (a
        // YouTube URL); drop both so only the prose lands in the synopsis.
        synopsis   = Option(document.selectFirst("div.section.tresc"))
                       .map(ScraperParse.cleanSynopsis(_, "div.terminy_row", "div.trailer")).filter(_.length > 20),
        // The detail page's WordPress `[video]` block holds the YouTube
        // trailer as `<source type="video/youtube" src="…watch?v=…">`.
        trailerUrl = document.select("video source[src], iframe[src]").asScala
                       .map(_.attr("src")).filter(_.nonEmpty).flatMap(ScraperParse.canonicalTrailer).headOption
      )
    }

  private def parseListItem(art: Element): Option[Film] =
    Option(art.selectFirst("h2.repe_title a")).flatMap { a =>
      // The listing bakes the version into the title ("Ścieżki życia – LEKTOR");
      // peel it off and carry it as a per-showtime badge so the LEKTOR/napisy
      // variants collapse onto one film row without losing the format.
      val (title, format) = ScraperParse.extractFormatTags(a.text.trim)
      SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1)).filter(_ => title.nonEmpty).map { slug =>
        val czas     = Option(art.selectFirst("div.repe_czas")).map(_.text.trim).getOrElse("")
        val runtime  = """(\d+)\s*min""".r.findFirstMatchIn(czas).map(_.group(1).toInt)
        val director = """(?i)reż\.\s*(.+)$""".r.findFirstMatchIn(czas).map(_.group(1).trim)
                         .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
        val poster   = Option(art.selectFirst("div.repe_outer")).map(_.attr("style"))
                         .flatMap(ScraperParse.cssUrl)
                         .map(u => if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}")
        Film(slug, title, format, runtime, director, poster)
      }
    }

  private def parseShowtimes(document: org.jsoup.nodes.Document): Seq[Showtime] =
    document.select("div.terminy_list > div.row").asScala.toSeq.flatMap { row =>
      val dets = row.select("div.term_det").asScala.toSeq.map(_.text.trim)
      val date = dets.flatMap(t => Try(java.time.LocalDate.parse(t, DateFmt)).toOption).headOption
      val time = dets.flatMap(ScraperParse.parseHHmm).headOption
      val booking = Option(row.selectFirst("a.green_but[href]")).map(_.attr("href")).filter(_.nonEmpty)
      for { d <- date; t <- time } yield Showtime(d.atTime(t), booking, None, Nil)
    }
}
