package services.cinemas.pl

import services.cinemas.common.ScraperParse
import org.jsoup.nodes.{Document, Element}
import models._
import tools.{HttpFetch, ParallelDetailFetch}
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail}

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KINOkawiarnia Stacja Falenica (Warszawa). The `/repertuar/` page lists each
 * film (title, runtime + director, poster) linking to `/filmy/<slug>/`, whose
 * "Dostępne terminy" list holds the screenings (absolute DD.MM.YYYY date +
 * time + a systembiletowy booking link) and the full synopsis.
 *
 * Two-phase fetch: the repertoire listing yields one entry per film with title,
 * runtime, director, poster, and the per-film detail-page URL stored in
 * `filmUrl`. The detail page is fetched per film for showtimes (always) and
 * for synopsis + trailerUrl (via `fetchFilmDetail`).
 *
 * The site moved to the `falenica3` WordPress theme in 2026-09: the listing's
 * `<article class="filmy">` wrapper (a WP custom-post-type class) is gone —
 * `div.repe-box` is now the outermost per-film element, though the inner
 * `repe_title`/`repe_czas`/`repe_opis`/`repe_outer` classes carried over
 * unchanged. The detail page was redesigned more thoroughly: showtimes moved
 * from `div.terminy_list > div.row` to `div.entry-terms__row` (date/time/CTA
 * each their own class), synopsis moved from `div.section.tresc` to
 * `article.entry-description`, and the trailer is no longer a `<video>`/
 * `<iframe>` embed but a `<button class="entry-trailer" data-youtube-id="…">`.
 */
class FalenicaClient(http: HttpFetch
) extends CinemaScraper with DetailEnricher {


  val cinema: Cinema = StacjaFalenica

  private val BaseUrl    = "https://stacjafalenica.pl"
  private val ListingUrl = s"$BaseUrl/repertuar/"
  private val SlugPat    = """/filmy/([^/"]+)/""".r

  private case class Film(slug: String, title: String, format: List[String], runtime: Option[Int], director: Seq[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    // A WordPress `__trashed-N` slug isn't necessarily dead: the venue trashes
    // the editorial post but the film keeps live "Dostępne terminy" (Romeria,
    // Znaki Pana Śliwki did, with future showtimes). Don't exclude by slug —
    // the `showtimes.isEmpty` drop below already removes genuinely-dead pages.
    val films = Jsoup.parse(http.get(ListingUrl)).select("div.repe-box").asScala.toSeq.flatMap(parseListItem)
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
   *  scrape rather than recording an empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map { html =>
      val document = Jsoup.parse(html)
      FilmDetail(
        // `article.entry-description` holds only the synopsis prose (the
        // showtimes list and trailer button live in sibling elements under the
        // redesigned theme, not nested inside it) plus a leading "O filmie"
        // heading, dropped so it doesn't leak into the first line.
        synopsis   = Option(document.selectFirst("article.entry-description"))
                       .map(ScraperParse.cleanSynopsis(_, "h2")).filter(_.length > 20),
        // The trailer is a `<button class="entry-trailer" data-youtube-id="…">`
        // (no more `<video>`/`<iframe>` embed); route the id through the same
        // canonicaliser every scraper uses by rebuilding a watch URL from it.
        trailerUrl = Option(document.selectFirst("button.entry-trailer[data-youtube-id]"))
                       .map(_.attr("data-youtube-id")).filter(_.nonEmpty)
                       .flatMap(id => ScraperParse.canonicalTrailer(s"https://www.youtube.com/watch?v=$id"))
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
    document.select("div.entry-terms__row").asScala.toSeq.flatMap { row =>
      val date = Option(row.selectFirst("div.entry-terms__date")).map(_.text.trim).flatMap(ScraperParse.parseDate)
      val time = Option(row.selectFirst("div.entry-terms__time")).map(_.text.trim).flatMap(ScraperParse.parseHHmm)
      val booking = Option(row.selectFirst("div.entry-terms__cta a[href]")).map(_.attr("href")).filter(_.nonEmpty)
      for { d <- date; t <- time } yield Showtime(d.atTime(t), booking, None, Nil)
    }
}
