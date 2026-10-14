package services.cinemas.de

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.libs.json._
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ListingPages, ScraperParse}
import services.cinemas.pl.NonMovieEventClassifier
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Arsenal – Institut für Film und Videokunst (Berlin) — a repertory house whose programme is
 * retrospectives (a Japanese one, 2026-10: "Kaidan", "Mozu", "Ototo", "Ogin-sama"). The
 * aggregators list those as a bare title with no year, director or runtime, which nothing
 * tells from a namesake; the house's own calendar states all of them per screening. It is the
 * JSON search API behind arsenal-berlin.de/kalender/:
 *
 *   GET /api/v1/search/calendar/?tx_solr[page]=<n>
 *     → content.colPos0[0].content.data.documents
 *         { pagination: { numberOfPages }, list: { results: [event…] } }   — 12 a page, by time
 *
 * one event per screening: `title`, `movieTitles` (more than one for a shared bill), `director`,
 * `country` ("Frankreich, Mauretanien, Algerien"), `year`, `playingTime` ("98 Min."),
 * `languageVersion` (OmU / OmeU / OmdU / DF), `dateText` ("Fri 16.10. 19:00" — yearless,
 * placed by [[ScraperParse.upcomingDate]]), `location`, `salesUrl` and the screening's own page
 * `url`. Every page is read: the calendar is the whole published programme.
 *
 * The foyer's video installation (`location` "Arsenal-Foyer", on all day) is no screening.
 *
 * A shared bill's year, director and country are its films' together ("1959, 1962"), so a
 * bill keeps none of them: they would name neither film.
 *
 * The screening's page ([[fetchFilmDetail]]) adds the cast (`mit`) and the house's German
 * synopsis; the listing already carries the year and director, so the identity model does not
 * wait on it (`defersTmdbResolution = false`).
 */
class ArsenalBerlinClient(
  http: HttpFetch,
  override val cinema: Cinema,
  /** The day the yearless dates are placed from, asked per scrape: today in Germany. */
  today: => LocalDate
) extends CinemaScraper with DetailEnricher {
  import ArsenalBerlinClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(CalendarPage)

  override val detailGroup: String = "arsenal-berlin"
  override def defersTmdbResolution: Boolean = false

  /** A durable 404/410 escapes rather than folding into None ([[DetailFetchOutcome]]). */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(html => parseDetail(Jsoup.parse(html, ref)))

  def fetch(): Seq[CinemaMovie] = {
    val from  = today
    val first = parsePage(HttpRead.page(http, pageUrl(1)), from)
    val rest  = ListingPages.readMore("arsenal-berlin-pages", 2 to first.pageCount, pageUrl)(HttpRead.page(http, _))
      .flatMap { case (_, body) => parsePage(body, from).screenings }
    toMovies((first.screenings ++ rest).filterNot(_.dateTime.toLocalDate.isBefore(from)), cinema)
  }
}

object ArsenalBerlinClient {
  val BaseUrl: String      = "https://www.arsenal-berlin.de"
  val CalendarPage: String = s"$BaseUrl/kalender/"

  def pageUrl(page: Int): String = s"$BaseUrl/api/v1/search/calendar/?tx_solr%5Bpage%5D=$page"

  final case class Page(screenings: Seq[Screening], pageCount: Int)

  final case class Screening(
    title:          String,
    shared:         Boolean,
    director:       Seq[String],
    countries:      Seq[String],
    year:           Option[Int],
    runtimeMinutes: Option[Int],
    format:         List[String],
    dateTime:       LocalDateTime,
    bookingUrl:     Option[String],
    pageUrl:        Option[String],
    posterUrl:      Option[String]
  )

  private val DayAndClock = raw"""(\d{1,2})\.(\d{1,2})\.\s+${ScraperParse.ClockParts}""".r.unanchored
  private val Minutes     = """(\d+)\s*Min""".r.unanchored
  private val Year        = """\b(1[89]\d\d|20\d\d)\b""".r

  /** One calendar page. A body without the documents envelope throws — a changed API or an
   *  error page is not a house with nothing on. */
  def parsePage(body: String, today: LocalDate): Page = {
    val documents = Json.parse(body) \ "content" \ "colPos0" \ 0 \ "content" \ "data" \ "documents"
    val pageCount = (documents \ "pagination" \ "numberOfPages").asOpt[Int].getOrElse(
      throw new IllegalStateException(s"Arsenal's calendar answered no pagination: ${body.take(300)}"))
    Page((documents \ "list" \ "results").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(parseScreening(_, today)), pageCount)
  }

  private def parseScreening(js: JsValue, today: LocalDate): Option[Screening] =
    for {
      title    <- text(js \ "title")
      if !(js \ "location").asOpt[Seq[String]].getOrElse(Nil).exists(_.contains("Foyer"))
      dateTime <- text(js \ "dateText").flatMap(dateTimeOf(_, today))
    } yield {
      val shared = (js \ "movieTitles").asOpt[Seq[String]].exists(_.size > 1)
      def fact[T](value: => T, none: T): T = if (shared) none else value
      Screening(
        title          = title,
        shared         = shared,
        director       = fact(list(text(js \ "director")), Nil),
        countries      = fact(list(text(js \ "country")), Nil),
        year           = fact(text(js \ "year").flatMap(singleYear), None),
        runtimeMinutes = fact(text(js \ "playingTime").flatMap(minutes), None),
        format         = text(js \ "languageVersion").toList.flatMap(languageVersion),
        dateTime       = dateTime,
        bookingUrl     = text(js \ "salesUrl"),
        pageUrl        = text(js \ "url").map(absolute),
        posterUrl      = text(js \ "image" \ "src").map(absolute)
      )
    }

  private def dateTimeOf(dateText: String, today: LocalDate): Option[LocalDateTime] =
    dateText match {
      case DayAndClock(day, month, hour, minute) =>
        for {
          monthDay <- ScraperParse.monthDay(day.toInt, month.toInt)
          date     <- ScraperParse.upcomingDate(monthDay, today)
          time     <- ScraperParse.clock(hour, minute)
        } yield date.atTime(time)
      case _ => None
    }

  /** The language version as the tokens the aggregators emit: OmdU is OmU (German subtitles). */
  private def languageVersion(version: String): List[String] = version.trim.toLowerCase(java.util.Locale.ROOT) match {
    case "omeu"               => List("OmeU")
    case "omu" | "omdu"       => List("OmU")
    case "ov" | "of" | "omov" => List("OV")
    case _                    => Nil
  }

  /** One row per film the house titles alike; its page is its first screening's. */
  private[de] def toMovies(screenings: Seq[Screening], cinema: Cinema): Seq[CinemaMovie] =
    screenings.filterNot(s => NonMovieEventClassifier.isNonScreening(s.title))
      .groupBy(_.title).toSeq.map { case (title, same) =>
        val byTime = same.sortBy(_.dateTime)
        CinemaMovie(
          movie     = Movie(title,
                        runtimeMinutes = byTime.flatMap(_.runtimeMinutes).headOption,
                        releaseYear    = byTime.flatMap(_.year).headOption,
                        countries      = byTime.map(_.countries).find(_.nonEmpty).getOrElse(Nil)),
          cinema    = cinema,
          posterUrl = byTime.flatMap(_.posterUrl).headOption,
          filmUrl   = byTime.flatMap(_.pageUrl).headOption,
          synopsis  = None,
          cast      = Nil,
          director  = byTime.map(_.director).find(_.nonEmpty).getOrElse(Nil),
          showtimes = byTime.map(s => Showtime(s.dateTime, s.bookingUrl, None, s.format))
                        .distinctBy(s => (s.dateTime, s.bookingUrl, s.format))
        )
      }.sortBy(_.movie.title)

  /** A screening's page: `Regie`, `mit` (the cast) and the unlabelled "Japan / 1962" over
   *  "102 Min. / DCP / OmeU" in its info list, and the house's synopsis beside it. */
  private[de] def parseDetail(document: Document): FilmDetail = {
    val items = document.select("ul.detail-info__list > li.detail-info__item").asScala.toSeq
    def labelled(label: String): Option[String] = items.collectFirst {
      case item if Option(item.selectFirst("p.detail-info__title")).exists(_.text.trim.equalsIgnoreCase(label)) =>
        Option(item.selectFirst("p.detail-info__text")).map(_.text.trim).filter(_.nonEmpty)
    }.flatten
    // The production line: the unlabelled item whose first line ends in a year.
    val production = items.filter(_.selectFirst("p.detail-info__title") == null)
      .flatMap(item => Option(item.selectFirst("p.detail-info__text")))
      .map(p => p.wholeText.split("\n").map(_.trim).filter(_.nonEmpty).toSeq)
      .find(_.headOption.exists(line => Year.findFirstIn(line.split("/").last).isDefined))
    val (countries, year) = production.flatMap(_.headOption).map { line =>
      val parts = line.split("/").map(_.trim).toSeq
      (list(Option(parts.init.mkString(", ")).filter(_.nonEmpty)), singleYear(parts.last))
    }.getOrElse((Nil, None))
    val synopsis = document.select("div.text__textblock p").asScala.map(_.text.trim).filter(_.nonEmpty)
    FilmDetail(
      synopsis       = Option(synopsis.mkString("\n\n")).filter(_.nonEmpty),
      cast           = list(labelled("mit")),
      director       = list(labelled("Regie")),
      runtimeMinutes = production.flatMap(_.drop(1).headOption).flatMap(minutes),
      releaseYear    = year,
      countries      = countries
    )
  }

  private def singleYear(s: String): Option[Int] = Year.findAllIn(s).toSeq match {
    case Seq(year) => Some(year.toInt)
    case _         => None
  }
  private def minutes(s: String): Option[Int] = s match {
    case Minutes(m) => m.toIntOption.filter(_ > 0)
    case _          => None
  }
  private def list(s: Option[String]): Seq[String] = s.toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
  private def absolute(path: String): String = if (path.startsWith("http")) path else BaseUrl + path
  private def text(js: JsLookupResult): Option[String] = js.asOpt[String].map(_.trim).filter(_.nonEmpty)
}
