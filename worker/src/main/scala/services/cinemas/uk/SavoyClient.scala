package services.cinemas.uk

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.parser.Parser
import play.api.libs.json._
import services.cinemas.common.{AgeRating, CinemaScraper, ScraperParse, SlotsToMovies}
import services.cinemas.pl.OnlyMovieEventsFilter
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
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
 * BOOKING-ONLY VENUES. Some venues keep their own site elsewhere and use Savoy only
 * to sell tickets (Ilkley Cinema's box office is `book.ilkleycinema.co.uk`). Their
 * `WhatsOn` is an empty shell without the literal; the programme is the box office's
 * root page instead, server-rendered as one block per film:
 *
 * {{{
 * <h2 class="title"><a href="…TcsProgramme_12591345">Sense and Sensibility</a> (12A)</h2>
 * <tr><td class="PeformanceListDate">Monday 12 Oct 2026</td><td/>
 *   <td class="PeformanceListTimes"><span class="StartTimeAndStatus">
 *     <a class="Button" href="…TcsPerformance_12626215.TcsSection_523880">16:00</a></span>…
 * }}}
 *
 * Templates differ in the heading's tag and class and in whether the time is the
 * button's text ("16:00") or precedes it ("3:30pm <a>Book</a>"), so the page is read
 * in document order: each titled `TcsProgramme_` link — or, where a template links
 * none (Wetherby), the block's `title` heading — names the film the date rows after it
 * belong to. [[SavoyClient.BoxOffice]] picks that page.
 *
 * @param dllBase the venue's app root, ending in `.dll/`.
 * @param listing which page carries the programme: [[SavoyClient.WhatsOn]] on a full
 *                venue site, [[SavoyClient.BoxOffice]] on a booking-only one.
 */
class SavoyClient(
  http:    HttpFetch,
  dllBase: String,
  override val cinema: Cinema,
  today:   => LocalDate,
  listing: SavoyClient.Listing = SavoyClient.WhatsOn
) extends CinemaScraper with OnlyMovieEventsFilter {

  private val pageUrl = s"$dllBase${listing.page}"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(dllBase)
  override def sourceUrl: Option[String] = Some(pageUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    listing.parse(HttpRead.page(http, pageUrl), dllBase, cinema, today)
}

object SavoyClient {

  /** The page a Savoy venue's programme is read from, and how. */
  sealed trait Listing {
    def page: String
    def parse(html: String, dllBase: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie]
  }

  /** A full venue site: `WhatsOn`, the programme in its `var Events` literal. */
  case object WhatsOn extends Listing {
    val page = "WhatsOn"
    def parse(html: String, dllBase: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] =
      SavoyClient.parse(html, dllBase, cinema, today)
  }

  /** A booking-only host: the box office's root page, dated rows under each film. */
  case object BoxOffice extends Listing {
    val page = ""
    def parse(html: String, dllBase: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] =
      parseBoxOffice(html, dllBase, cinema, today)
  }

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

  /** "Monday 12 Oct 2026" — the weekday is ignored. */
  private val RowDate = """(\d{1,2})\s+([A-Za-z]{3,})\s+(\d{4})""".r

  /** "16:00", "3:30pm", "7.45 pm". */
  private val RowTime = s"""${ScraperParse.ClockPartsDotted}\\s*([ap]\\.?m\\.?)?""".r

  private val ProgrammeId = """TcsProgramme_(\d+)""".r

  /** A performance note such as "(Descriptive subtitles)" — the box office's own
   *  subtitled flag, which the `WhatsOn` literal carries as `"SU":"Y"`. */
  private val Subtitled = """(?i)\bsubtitle""".r

  /** "(12A)" or "(Cert.TBC)" after the title link. */
  private val Certificate = """\((?:Cert\.?\s*)?([^)]+)\)""".r

  /** A film's title heading that carries no `TcsProgramme_` link (Wetherby's template). */
  private val LinklessTitle = Seq("h1", "h2", "h3").map(h => s"$h.title:not(:has(a))").mkString(", ")

  /** The " / SHOWTIMES" the heading's own text ends on once its highlight span is gone. */
  private val TitleSuffix = """\s*/\s*$""".r

  private case class Programme(title: String, url: Option[String], id: Option[String],
                               certificate: Option[String], poster: Option[String], synopsis: Option[String])

  /** The booking-only root page → one [[CinemaMovie]] per film with a performance from
   *  `today` on. A row whose date cell is no date — a ticket product such as "Sponsor a
   *  Seat" filed among the films — is skipped. */
  def parseBoxOffice(html: String, dllBase: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val page = Jsoup.parse(html, dllBase)
    require(page.selectFirst("#Detail.PerformanceList") != null,
      "no '#Detail.PerformanceList' on the box office page — the Savoy template changed")
    val marks = page.select(s"a[href*=TcsProgramme_], $LinklessTitle, td.PeformanceListDate").asScala.toSeq
    val slots = marks.foldLeft((Option.empty[Programme], Vector.empty[(Programme, Showtime)])) {
      case ((current, acc), dateCell) if dateCell.hasClass("PeformanceListDate") =>
        (current, current.fold(acc)(programme => acc ++ rowShowtimes(programme, dateCell, today)))
      case ((current, acc), heading) => (programmeOf(heading).orElse(current), acc)
    }._2

    SlotsToMovies.fold(slots, _._1.title, _._2) { (title, group, showtimes) =>
      val programme = group.head._1
      CinemaMovie(
        movie       = Movie(title),
        cinema      = cinema,
        posterUrl   = programme.poster,
        filmUrl     = programme.url,
        synopsis    = programme.synopsis,
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = showtimes,
        externalIds = programme.id.map(id => Map("savoy" -> id)).getOrElse(Map.empty),
        ageRating   = programme.certificate.flatMap(AgeRating.normalize)
      )
    }
  }

  /** One date row's performances: each `StartTimeAndStatus` span's time on the row's day. */
  private def rowShowtimes(programme: Programme, dateCell: Element, today: LocalDate): Seq[(Programme, Showtime)] =
    for {
      day  <- rowDate(dateCell.text).toSeq if !day.isBefore(today)
      span <- Option(dateCell.parent).toSeq.flatMap(_.select("span.StartTimeAndStatus").asScala)
      time <- RowTime.findFirstMatchIn(span.text).flatMap(ScraperParse.meridiemClockAt(_, 1, 3)).toSeq
    } yield programme -> Showtime(
      dateTime   = day.atTime(time),
      bookingUrl = Option(span.selectFirst("a[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
      format     = if (Subtitled.findFirstIn(span.text).isDefined) List("SUB") else Nil)

  /** The film a `TcsProgramme_` link — or, where the template links none, the block's
   *  title heading ("Sense and Sensibility / SHOWTIMES") — names; `None` for an untitled
   *  (image) link, which leaves the film before it current. */
  private def programmeOf(mark: Element): Option[Programme] = {
    val isLink  = mark.tagName == "a"
    val heading = if (isLink) Option(mark.parent) else Some(mark)
    val text    = if (isLink) mark.text else TitleSuffix.replaceFirstIn(mark.ownText, "")
    Some(text.replaceAll("\\s+", " ").trim).filter(_.nonEmpty).map { title =>
      val block = Option(mark.closest(".programme")).orElse(heading.flatMap(h => Option(h.parent)))
      Programme(
        title       = title,
        url         = if (isLink) Some(mark.attr("abs:href")) else None,
        id          = if (isLink) ProgrammeId.findFirstMatchIn(mark.attr("href")).map(_.group(1)) else None,
        certificate = heading.flatMap(h => Certificate.findFirstMatchIn(h.ownText)).map(_.group(1).trim),
        poster      = block.flatMap(b => Option(b.selectFirst("img[src]"))).map(_.attr("abs:src")).filter(_.nonEmpty),
        synopsis    = block.flatMap(b => Option(b.selectFirst(".synopsis"))).map(_.text.trim)
          .filter(s => s.nonEmpty && !s.equalsIgnoreCase("tbc"))
      )
    }
  }

  private def rowDate(text: String): Option[LocalDate] =
    RowDate.findFirstMatchIn(text).flatMap { m =>
      ScraperParse.englishMonth(m.group(2))
        .flatMap(month => Try(LocalDate.of(m.group(3).toInt, month, m.group(1).toInt)).toOption)
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
