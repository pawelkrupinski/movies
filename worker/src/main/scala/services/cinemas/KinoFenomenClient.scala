package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Fenomen — the art-house cinema of Wojewódzki Dom Kultury im. Józefa
 * Piłsudskiego in Kielce. The venue's repertoire is published on a
 * server-rendered biletyna.pl iframe (venue id 639) at
 * `https://iframe639.biletyna.pl/?display=events`.
 *
 * Each event row (`div.iframe_all`) carries:
 *   - Date: `span.B-font-weight--bold` — `DD.MM.YYYY`
 *   - Time: the second `span.B-font-weight--bold` — `HH:MM`
 *   - Title: `div.iframe_all_event_title a[href^="/artist/view/id/"]` — raw title
 *     with a trailing `(2D/napisy)` / `(2D/oryginalny)` format tag and,
 *     for some entries, `| reżyseria: …` metadata after a pipe. Both are
 *     stripped before the title is stored.
 *   - Booking URL: `a.B-btn--accent[href^="/event/view/id/"]`
 *   - Poster: `img.img-responsive[src^="/file/get/"]` (first one, optional)
 *
 * The listing covers the next few weeks in one page — no pagination needed.
 */
class KinoFenomenClient(
  http: HttpFetch,
  override val cinema: Cinema = KinoFenomen
) extends CinemaScraper {

  import KinoFenomenClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(ListingUrl)
    val document  = Jsoup.parse(html)

    val slots = document.select("div.iframe_all").asScala.toSeq.flatMap(parseSlot)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title, releaseYear = head.year),
        cinema    = cinema,
        posterUrl = head.poster,
        filmUrl   = head.booking.map(u => if (u.startsWith("http")) u else BaseUrl + u),
        synopsis  = None,
        cast      = Seq.empty,
        director  = head.directors,
        showtimes = showtimes
      )
    }
  }
}

object KinoFenomenClient {

  val BaseUrl    = "https://iframe639.biletyna.pl"
  val ListingUrl = s"$BaseUrl/?display=events"

  private val DateFmt   = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")
  // Format tag at the end of artist names: "(2D/napisy)", "(3D/dubbing)", etc.
  private val FormatPat = """\s*\((\d[Dd]/[^)]+)\)\s*$""".r
  // The `| reżyseria: <names> |` segment of the artist-link text; names are a
  // comma-separated list bounded by the next pipe (or the end).
  private val DirectorPat = """(?i)reżyseria:\s*([^|]+)""".r
  // A `(YYYY)` paren year in the title itself (e.g. "Mikey i Nicky (1976)").
  private val ParenYearPat = """\((?:19|20)\d{2}\)""".r
  // Any in-range 4-digit year token (used inside the trailing metadata segment,
  // where it follows the production countries: "… Węgry 2025").
  private val YearTokenPat = """\b(?:19|20)\d{2}\b""".r

  /** Director(s) from the `| reżyseria: … |` metadata segment, comma-split.
   *  Empty when the listing carries no director metadata for the film. */
  def parseDirectors(rawTitle: String): Seq[String] =
    DirectorPat.findFirstMatchIn(rawTitle).map(_.group(1).trim).toSeq
      .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))

  /** Production year — from the trailing `| … Country YYYY` metadata segment, or
   *  a `(YYYY)` paren in the title. Only these two structured positions count, so
   *  a stray number in a bare title (no pipe metadata, no paren) is not mistaken
   *  for a year. None when neither is present. */
  def parseYear(rawTitle: String): Option[Int] = {
    val parts    = rawTitle.split('|')
    val metaYear = if (parts.length > 1)
      YearTokenPat.findFirstMatchIn(parts.drop(1).mkString(" ")).map(_.matched.toInt)
    else None
    metaYear.orElse(ParenYearPat.findFirstMatchIn(rawTitle).map(_.matched.filter(_.isDigit).toInt))
  }

  private[cinemas] case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    poster:    Option[String],
    format:    List[String],
    directors: Seq[String],
    year:      Option[Int]
  )

  private[cinemas] def parseSlot(row: org.jsoup.nodes.Element): Option[RawSlot] = {
    // Date + time — biletyna renders two bold spans: date then "godz." label
    // then the time bold span; select the event-date sub-block specifically.
    val dateBold = Option(row.selectFirst("div.event-date span.B-font-weight--bold"))
      .map(_.text.trim)
    val timeBold = row.select("div.event-date .B-text--nowrap span.B-font-weight--bold")
      .asScala.headOption.map(_.text.trim)

    val dt = for {
      d <- dateBold
      t <- timeBold
      ldt <- Try(LocalDateTime.parse(s"$d $t", DateFmt)).toOption
    } yield ldt

    // Raw title from the artist link — strip pipe-separated metadata + format tag.
    val rawTitle = Option(row.selectFirst("div.iframe_all_event_title a[href^=\"/artist/view/id/\"]"))
      .map(_.text.trim)
      .filter(_.nonEmpty)

    val title = rawTitle.map { t =>
      val beforePipe = t.split('|').head.trim
      FormatPat.replaceFirstIn(beforePipe, "")
    }

    val format = rawTitle.toList.flatMap { t =>
      FormatPat.findFirstMatchIn(t).toList.flatMap { m =>
        m.group(1).split("/").map(_.trim).filter(_.nonEmpty).toList
      }
    }

    val booking = Option(row.selectFirst("a.B-btn--accent[href^=\"/event/view/id/\"]"))
      .map(a => BaseUrl + a.attr("href"))

    val poster = Option(row.selectFirst("img.img-responsive[src^=\"/file/get/\"]"))
      .map(img => BaseUrl + img.attr("src"))

    val directors = rawTitle.toSeq.flatMap(parseDirectors)
    val year      = rawTitle.flatMap(parseYear)

    for {
      t  <- title
      ldt <- dt
    } yield RawSlot(t, ldt, booking, poster, format, directors, year)
  }
}
