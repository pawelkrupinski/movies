package services.cinemas.pl

import java.util.Locale

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json.{JsArray, Json}
import services.cinemas.common.{AgeRating, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ListingPages, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Kino Remus (Kościerzyna), the Kościerski Dom Kultury's cinema. Its programme
 * lives on two sites, and each holds what the other lacks:
 *
 *   - The "Ticket Manager" booking site (`remus.hostingasp.pl`, ASP.NET MVC)
 *     renders its calendar in the browser, but the jQuery `eventCalendar`
 *     widget feeds from `/Repertuar/Kalendarz1JsonDane` — a JSON array with one
 *     object per screening, weeks ahead. Each object's `date` is the day
 *     stamped `23:59:00`; the real time, the title and the booking link sit in
 *     the object's `title`, an HTML fragment: `p.event-title` (`„Lalka” 2D`,
 *     `„Zapomniana wyspa” 2D dubbing`), `p.event-hour` (`czwartek, godz.17:00`)
 *     and `a.bt` (`/Bilety/Sala/<id>`). Its poster is one generic
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
  val FeedUrl          = s"${TicketManagerUrl}Repertuar/Kalendarz1JsonDane?filter=$TicketManagerUrl"
  val FilmListUrl      = "https://kdkkoscierzyna.pl/kino-remus/"

  /** Every quotation mark either site wraps a title in — both mix Polish
   *  „…” with straight and English quotes, sometimes within one title. */
  private val Quotes = "[„”“\"]".r

  private val ScreeningHour = raw"""godz\.\s*(${ScraperParse.ClockTextDotted})""".r

  /** The film page's header line, `„Lalka”/dramat, romans/13+/2D`: genres and
   *  age rating are the two slash-separated fields before the version. */
  private val GenresAndAge = """/([^/]+)/(\d{1,2}\+)/""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], format: List[String])

  private[pl] def parse(feed: String, filmList: String, cinema: Cinema): Seq[CinemaMovie] = {
    val pages = filmPages(filmList)
    val slots = Json.parse(feed).as[JsArray].value.toSeq.flatMap { event =>
      for {
        day   <- (event \ "date").asOpt[String].flatMap(ScraperParse.parseDate)
        slot  <- screening((event \ "title").asOpt[String].getOrElse(""), day)
      } yield slot
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

  /** One screening from an event's HTML fragment, or None when it lacks a
   *  title or a `godz.` time. */
  private def screening(fragment: String, day: LocalDate): Option[RawSlot] = {
    val html = Jsoup.parseBodyFragment(fragment, TicketManagerUrl)
    for {
      billed <- Option(html.selectFirst("p.event-title")).map(_.text)
      (bare, format) = ScraperParse.extractFormatTags(billed)
      title  = unquoted(bare) if title.nonEmpty
      hour   <- Option(html.selectFirst("p.event-hour")).flatMap(p => ScreeningHour.findFirstMatchIn(p.text))
      time   <- ScraperParse.parseHHmm(hour.group(1).replace('.', ':'))
    } yield RawSlot(title, LocalDateTime.of(day, time),
      Option(html.selectFirst("a.bt[href]")).map(_.attr("abs:href")).filter(_.nonEmpty), format)
  }

  /** The KDK film list's `title → page` links, keyed by [[titleKey]]. */
  private def filmPages(html: String): Map[String, String] =
    Jsoup.parse(html, FilmListUrl).select("h4.fusion-title-heading a[href]").asScala.toSeq
      .map(a => titleKey(a.text) -> a.attr("abs:href"))
      .filter { case (key, url) => key.nonEmpty && url.nonEmpty }
      .toMap

  private def unquoted(title: String): String =
    Quotes.replaceAllIn(title, "").replaceAll("\\s+", " ").trim

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
