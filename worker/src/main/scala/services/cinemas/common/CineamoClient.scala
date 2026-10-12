package services.cinemas.common

import models._
import play.api.libs.json._
import services.cinemas.pl.NonMovieEventClassifier
import tools.{HttpFetch, HttpRead}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import scala.util.Try

/**
 * One German venue's programme from Cineamo — the showtime platform ~1,300 German
 * cinemas publish through, read off its public JSON API (no auth):
 *
 *   GET https://api.cineamo.com/showings?cinemaId=<id>&page=<n>
 *     → { _page_count, _embedded: { showings: [showing…] } }   — 10 a page, by start time
 *
 * Every page is read, never a fixed window: Cineamo carries presales months ahead,
 * where Filmstarts stops at the end of the programme week. Page 1 says how many
 * pages there are; the rest are read side by side ([[ListingPages.readMore]]), and a
 * lost one leaves the listing incomplete rather than failing it.
 *
 * A showing's `name` is the VENUE's title for it, in German ("Shaun das Schaf - Spuk
 * im Kürbisfeld"); its embedded `content.name` is Cineamo's film record, in English
 * ("Shaun the Sheep: The Beast of Mossy Bottom") — carried as the original title, a
 * search hint, never shown. `cineamoMovieId` names that record ("567m"), kept as the
 * listing's `cineamo` catalogue id: a feed's claim of which film the slot is
 * (`CatalogueSources.FeedIds`). The record's description is English and is left
 * out, so the German synopsis enrichment finds no venue text to defer to.
 *
 * Start times are UTC instants; they are listed in German wall-clock time.
 */
class CineamoClient(
  http: HttpFetch,
  cinemaId: CineamoClient.CinemaId,
  override val cinema: Cinema,
  /** The day the horizon is measured from, asked per scrape — today in Germany. */
  today: => LocalDate
) extends CinemaScraper {
  import CineamoClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiUrl)

  /** Cineamo publishes no public venue page; the listing is the API's own. */
  override def sourceKey: Option[String] = Some(s"api.cineamo.com/cinema/${cinemaId.value}")

  def fetch(): Seq[CinemaMovie] = {
    val referenceDay = today
    val first = parsePage(HttpRead.page(http, pageUrl(cinemaId, 1)))
    val rest  = ListingPages.readMore("cineamo-pages", 2 to first.pageCount, (p: Int) => pageUrl(cinemaId, p))(HttpRead.page(http, _))
      .flatMap { case (_, body) => parsePage(body).showings }
    toMovies((first.showings ++ rest).filterNot(_.dateTime.toLocalDate.isBefore(referenceDay)), cinema)
  }
}

object CineamoClient {
  val ApiUrl: String = "https://api.cineamo.com"
  val Zone:   ZoneId = TimeZones.Germany

  /** A venue's id in Cineamo's API. */
  final case class CinemaId(value: String)

  def pageUrl(cinemaId: CinemaId, page: Int): String = s"$ApiUrl/showings?cinemaId=${cinemaId.value}&page=$page"

  final case class Page(showings: Seq[Showing], pageCount: Int)

  final case class Showing(
    title:          String,
    originalTitle:  Option[String],
    catalogueId:    Option[String],
    runtimeMinutes: Option[Int],
    year:           Option[Int],
    trailerUrl:     Option[String],
    dateTime:       LocalDateTime,
    bookingUrl:     Option[String],
    format:         List[String]
  )

  /** One `/showings` page. A body without the paging envelope throws — a changed API or
   *  an error page is not a venue with nothing on. */
  def parsePage(body: String): Page = {
    val js = Json.parse(body)
    val pageCount = (js \ "_page_count").asOpt[Int].getOrElse(
      throw new IllegalStateException(s"Cineamo answered no _page_count: ${body.take(300)}"))
    Page((js \ "_embedded" \ "showings").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(parseShowing), pageCount)
  }

  private def parseShowing(js: JsValue): Option[Showing] =
    for {
      title    <- text(js \ "name")
      if (js \ "state").asOpt[String].forall(_ == "scheduled")
      instant  <- (js \ "startDatetime").asOpt[String].flatMap(s => Try(Instant.parse(s)).toOption)
    } yield {
      val content = js \ "_embedded" \ "content"
      Showing(
        title          = title,
        originalTitle  = text(content \ "name").filter(_ != title),
        catalogueId    = text(js \ "cineamoMovieId"),
        runtimeMinutes = (content \ "duration").asOpt[Int].filter(_ > 0),
        year           = text(content \ "premiereDate").flatMap(d => Try(d.take(4).toInt).toOption),
        trailerUrl     = text(content \ "trailerUrl"),
        dateTime       = instant.atZone(Zone).toLocalDateTime,
        bookingUrl     = text(js \ "onlineTicketUrl"),
        format         = formatTokens(js)
      )
    }

  /** Cineamo's per-showing format flags, as the tokens Filmstarts emits for the same formats. */
  private val FormatFlags: List[(String, String)] = List(
    "isThreeDimensional" -> "3D",
    "isImax"             -> "IMAX",
    "is4DX"              -> "4DX",
    "isScreenX"          -> "SCREENX",
    "isDbox"             -> "DBOX",
    "isDolbyAtmos"       -> "ATMOS",
    "isInfinityVision"   -> "INFINITY"
  )

  /** Format tokens, then the language version: a subtitled showing is OmU (German
   *  subtitles) or OmeU (English); an original-language one without subtitles is OV.
   *  A German film or a dub is the unmarked default, as on Filmstarts. */
  def formatTokens(js: JsValue): List[String] = {
    def flag(name: String) = (js \ name).asOpt[Boolean].contains(true)
    val screen    = FormatFlags.collect { case (name, token) if flag(name) => token }
    val subtitles = text(js \ "subtitledLanguage")
    val version =
      if (flag("isSubtitled") || subtitles.isDefined) List(if (subtitles.contains("eng")) "OmeU" else "OmU")
      else if (flag("isOriginalLanguage") && !text(js \ "originalLanguage").contains("deu")) List("OV")
      else Nil
    screen ++ version
  }

  /** One row per film the venue titles alike, showtimes de-duplicated and in time order;
   *  a mystery screening ("Sneak Preview") names no film and is left out, as is a reading, gig or day pass
   *  the venue sells beside its films (`NonMovieEventClassifier.isNonScreening`). */
  private[common] def toMovies(showings: Seq[Showing], cinema: Cinema): Seq[CinemaMovie] =
    showings.filterNot(s => NonMovieEventClassifier.isNonScreening(s.title))
      .groupBy(_.title).toSeq.map { case (title, same) =>
        val film = same.head
        CinemaMovie(
          movie       = Movie(title, runtimeMinutes = same.flatMap(_.runtimeMinutes).headOption,
                              releaseYear = same.flatMap(_.year).headOption,
                              originalTitle = same.flatMap(_.originalTitle).headOption),
          cinema      = cinema,
          posterUrl   = None,
          filmUrl     = None,
          synopsis    = None,
          cast        = Nil,
          director    = Nil,
          showtimes   = same.map(s => Showtime(s.dateTime, s.bookingUrl, None, s.format))
                          .distinctBy(s => (s.dateTime, s.bookingUrl, s.format)).sortBy(_.dateTime),
          externalIds = same.flatMap(_.catalogueId).headOption.map(CatalogueSource -> _).toMap,
          trailerUrl  = film.trailerUrl
        )
      }.sortBy(_.movie.title)

  /** The catalogue-id source a Cineamo listing's film record is filed under. */
  val CatalogueSource: String = services.identity.CatalogueSources.Cineamo

  private def text(js: JsLookupResult): Option[String] = js.asOpt[String].map(_.trim).filter(_.nonEmpty)
}
