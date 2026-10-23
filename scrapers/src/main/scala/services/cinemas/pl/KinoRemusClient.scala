package services.cinemas.pl

import java.util.Locale
import services.movies.FormatTags

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{AgeRating, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ListingPages, ScraperParse, SlotsToMovies}
import tools.Whitespace.collapsedSpaces
import tools.{HttpFetch, HttpRead}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._

/**
 * Kino Remus (Kościerzyna), the Kościerski Dom Kultury's cinema. Its programme
 * lives on two sites, and each holds what the other lacks:
 *
 *   - The "Ticket Manager" booking site (`remus.hostingasp.pl`, [[TicketManagerFeed]])
 *     carries every screening, weeks ahead, billed with quotes and a version
 *     (`„Lalka” 2D`, `„Zapomniana wyspa” 2D dubbing`). Its poster is one generic
 *     "zapraszamy" placeholder for every event, so none is taken from it.
 *   - The KDK WordPress page `/kino-remus/` lists the current films, undated,
 *     each linking to its own page (`/lalka/`) — and those pages carry the
 *     poster, genres, age rating, runtime and synopsis. The Ticket Manager
 *     names no page, so a screening is matched to one by its title with the
 *     quotes stripped; a title the list doesn't carry is still screened, just
 *     without a page.
 *
 * The film page states no director, year or original title, so there is no
 * identity hint to wait for: TMDB resolves from the listing title and the page
 * merges in as display enrichment ([[defersTmdbResolution]] = false).
 *
 * [[OnlyMovieEventsFilter]] is mixed in defensively: the Ticket Manager is a
 * generic box office with no field telling a screening from a stage event, and
 * the culture house also stages concerts and plays.
 */
class KinoRemusClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper with OnlyMovieEventsFilter with DetailEnricher {

  import KinoRemusClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(TicketManagerUrl, FilmListUrl)
  override def sourceUrl: Option[String] = Some(TicketManagerUrl)

  // The feed is the programme and propagates its failure. The KDK film list
  // only links screenings to their pages, so it failing costs the links, not
  // the screenings — an enrichment read, not a listing page.
  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val feed     = HttpRead.page(http, FeedUrl)
    val filmList = ListingPages.readEnrichment("kino-remus-films", Seq(FilmListUrl), identity[String])(HttpRead.page(http, _))
      .flatMap(_._2.toOption).headOption.getOrElse("")
    parse(feed, filmList, cinema)
  }

  override val detailGroup: String = "kino-remus"
  override def defersTmdbResolution: Boolean = false

  /** See [[DetailFetchOutcome.page]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(parseDetail)
}

object KinoRemusClient {

  val TicketManagerUrl = "https://www.remus.hostingasp.pl/"
  val FeedUrl          = TicketManagerFeed.feedUrl(TicketManagerUrl)
  val FilmListUrl      = "https://kdkkoscierzyna.pl/kino-remus/"

  /** Every quotation mark either site wraps a title in — both mix Polish
   *  „…” with straight and English quotes, sometimes within one title. */
  private val Quotes = "[„”“\"]".r

  /** The film page's header line, `„Lalka”/dramat, romans/13+/2D`: genres and
   *  age rating are the two slash-separated fields before the version. */
  private val GenresAndAge = """/([^/]+)/(\d{1,2}\+)/""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], format: List[String])

  private[pl] def parse(feed: String, filmList: String, cinema: Cinema): Seq[CinemaMovie] = {
    val pages = filmPages(filmList)
    val slots = TicketManagerFeed.screenings(feed, TicketManagerUrl).flatMap { screening =>
      val (bare, format) = FormatTags.extractFormatTags(screening.billedTitle)
      Option(unquoted(bare)).filter(_.nonEmpty).map(RawSlot(_, screening.dateTime, screening.bookingUrl, format))
    }
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = pages.get(titleKey(title)),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  /** The KDK film list's `title → page` links, keyed by [[titleKey]]. */
  private def filmPages(html: String): Map[String, String] =
    Jsoup.parse(html, FilmListUrl).select("h4.fusion-title-heading a[href]").asScala.toSeq
      .map(a => titleKey(a.text) -> a.attr("abs:href"))
      .filter { case (key, url) => key.nonEmpty && url.nonEmpty }
      .toMap

  private def unquoted(title: String): String =
    Quotes.replaceAllIn(title, "").collapsedSpaces

  private def titleKey(title: String): String = unquoted(title).toLowerCase(Locale.ROOT)

  private[pl] def parseDetail(html: String): FilmDetail = {
    val doc     = Jsoup.parse(html, FilmListUrl)
    val content = Option(doc.selectFirst(".fusion-content-tb"))
    val text    = content.map(_.text).getOrElse("")
    val header  = GenresAndAge.findFirstMatchIn(text)
    FilmDetail(
      synopsis       = content.map(synopsisOf).filter(_.nonEmpty),
      runtimeMinutes = content.flatMap(c => c.select("p").asScala.find(_.text.contains("Czas trwania")))
                         .flatMap(p => ScraperParse.hoursMinutesRuntime(p.text)),
      genres         = header.toSeq.flatMap(_.group(1).split(",")).map(_.trim).filter(_.nonEmpty),
      ageRating      = AgeRating.normalize(header.map(_.group(2))),
      posterUrl      = ScraperParse.ogImage(doc)
    )
  }

  /** The page body minus its boilerplate paragraphs: the all-bold header lines
   *  (title/genre/age/version, runtime, and on event-style pages the date and
   *  the `godz. … cena biletu` line) and the closing "Bilety do nabycia" note. */
  private def synopsisOf(content: Element): String =
    ScraperParse.cleanSynopsisWithout(content) { p =>
      val own = p.text.trim
      own.isEmpty || own.startsWith("Bilety do nabycia") || ScraperParse.isAllBold(p)
    }
}
