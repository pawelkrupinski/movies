package services.cinemas.uk

import models._
import org.jsoup.parser.Parser
import play.api.libs.json._
import services.cinemas.common.{AgeRating, CinemaScraper, ScraperParse, SlotsToMovies}
import services.cinemas.pl.OnlyMovieEventsFilter
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Savoy Systems — the UK independent-cinema ticketing platform whose venue sites are
 * served by a Delphi web app at `<venue host>/<Venue>.dll/` (Phoenix East Finchley is
 * `https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/`). Flicks lists some of
 * these venues with no times at all (Phoenix has never had one from it), while the
 * venue's own site carries the whole programme.
 *
 * The `WhatsOn` page is server-rendered, and every Savoy template embeds the WHOLE
 * programme in it as one script literal the page's own filters then render:
 *
 * {{{
 * var Events = {"Events":[{"ID":635683,"Title":"Pressure","TypeDescription":"Film",
 *   "RunningTime":101,"Year":"2026","Director":"Anthony Maras","Cast":"Andrew Scott, …",
 *   "Rating":"<img … alt=\"BBFC Rating: (15)\"/>","ImageURL":"…","URL":"…WhatsOn?f=635683",
 *   "Performances":[{"StartDate":"2026-10-15","StartTime":"1830","AuditoriumName":"Screen 1",
 *     "SU":"N","URL":"Booking?Booking=…TcsPerformance_635690…","IsOpenForSale":true}]}]};
 * }}}
 *
 * One request is the venue's whole scrape, every date it sells (Phoenix: eight months
 * ahead). Savoy is cinema ticketing, so every event is a screening — opera and ballet
 * relays, NT Live, a silent film with a live score included; the
 * [[OnlyMovieEventsFilter]] still drops a stray live gig a venue sells through it.
 *
 * @param dllBase the venue's app root, ending in `.dll/`.
 */
class SavoyClient(
  http:    HttpFetch,
  dllBase: String,
  override val cinema: Cinema,
  today:   => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import SavoyClient._

  private val whatsOnUrl = s"$dllBase$WhatsOnPage"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(dllBase)
  override def sourceUrl: Option[String] = Some(whatsOnUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    parse(HttpRead.page(http, whatsOnUrl), dllBase, cinema, today)
}

object SavoyClient {

  val WhatsOnPage = "WhatsOn"

  /** The script literal holding the programme. */
  private val EventsAssignment = "var Events ="

  /** "BBFC Rating: (12A)" inside the rating image's alt text. */
  private val BbfcAlt = """BBFC Rating:\s*\(([^)]+)\)""".r

  private case class Slot(event: JsObject, title: String, showtime: Showtime)

  /** The `WhatsOn` page → one [[CinemaMovie]] per titled event with a screening from
   *  `today` on. A page without the literal throws: that is a template change, not an
   *  empty programme. */
  def parse(html: String, dllBase: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val events = (eventsLiteral(html) \ "Events").as[Seq[JsObject]]
    val slots = for {
      event <- events
      title <- titleOf(event).toSeq
      perf  <- (event \ "Performances").asOpt[Seq[JsObject]].getOrElse(Nil)
      when  <- startOf(perf).toSeq if !when.toLocalDate.isBefore(today)
    } yield Slot(event, title, Showtime(
      dateTime   = when,
      bookingUrl = (perf \ "URL").asOpt[String].filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else s"$dllBase$u"),
      room       = (perf \ "AuditoriumName").asOpt[String].map(_.trim).filter(_.nonEmpty),
      format     = if ((perf \ "SU").asOpt[String].contains("Y")) List("SUB") else Nil
    ))

    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val event = group.head.event
      def text(field: String): Option[String] =
        (event \ field).asOpt[String].map(unescape).filter(_.nonEmpty)
      def names(field: String): Seq[String] =
        text(field).toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = (event \ "RunningTime").asOpt[Int].filter(_ > 0),
          releaseYear    = text("Year").flatMap(ScraperParse.yearIn),
          countries      = names("Country")
        ),
        cinema      = cinema,
        posterUrl   = text("ImageURL"),
        filmUrl     = text("URL"),
        synopsis    = text("Synopsis"),
        cast        = names("Cast"),
        director    = names("Director"),
        showtimes   = showtimes,
        externalIds = (event \ "ID").asOpt[Long].map(id => Map("savoy" -> id.toString)).getOrElse(Map.empty),
        ageRating   = text("Rating").flatMap(BbfcAlt.findFirstMatchIn(_)).flatMap(m => AgeRating.normalize(m.group(1)))
      )
    }
  }

  /** The JSON object assigned to `var Events`, cut at its own closing brace: Play's parser
   *  rejects the `</script>` that follows it rather than stopping after one value. */
  private def eventsLiteral(html: String): JsValue = {
    val at = html.indexOf(EventsAssignment)
    require(at >= 0, s"no '$EventsAssignment' literal in the page — the Savoy template changed")
    val start = html.indexOf('{', at)
    Json.parse(html.substring(start, endOfObject(html, start)))
  }

  /** The index just past the `}` closing the object opened at `start`, skipping braces
   *  inside string literals (titles and synopses carry them). */
  private def endOfObject(s: String, start: Int): Int = {
    var depth = 0; var inString = false; var i = start
    while (i < s.length) {
      val c = s.charAt(i)
      if (inString) { if (c == '\\') i += 1 else if (c == '"') inString = false }
      else if (c == '"') inString = true
      else if (c == '{') depth += 1
      else if (c == '}') { depth -= 1; if (depth == 0) return i + 1 }
      i += 1
    }
    throw new IllegalArgumentException("the 'var Events' literal never closes")
  }

  private def titleOf(event: JsObject): Option[String] =
    (event \ "Title").asOpt[String].map(unescape).map(_.replaceAll("\\s+", " ").trim).filter(_.nonEmpty)

  /** "2026-10-15" + "1830" → 2026-10-15T18:30. */
  private def startOf(perf: JsObject): Option[LocalDateTime] =
    for {
      day  <- (perf \ "StartDate").asOpt[String]
      hhmm <- (perf \ "StartTime").asOpt[String].filter(_.matches("\\d{4}"))
      at   <- Try(LocalDateTime.parse(s"${day}T${hhmm.take(2)}:${hhmm.drop(2)}")).toOption
    } yield at

  /** Savoy HTML-escapes its text fields inside the JSON ("Royal Ballet &amp; Opera"). */
  private def unescape(s: String): String = Parser.unescapeEntities(s, false).trim
}
