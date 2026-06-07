package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.HttpFetch

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kameralne Cafe (Gdańsk, Trójmiasto) — a small arthouse café-cinema. Its
 * own site (`kinokameralnecafe.pl/repertuar/`) is a JS shell embedding a
 * biletyna.pl iframe, so we scrape biletyna directly. The venue page at
 * `biletyna.pl/Gdansk/Kino-Kameralne-Cafe` is server-rendered and — crucially —
 * carries a single `<script type="application/ld+json">` block: a schema.org
 * `Place` whose `events` array is the full programme as `ScreeningEvent`s.
 *
 * Each event gives us everything we need without a per-film detail fetch:
 *   - `name`      → film title
 *   - `startDate` → ISO-8601 with offset ("2026-06-06T18:00:00+02:00")
 *   - `url`       → biletyna film page (`/film/<slug>?eid=N#opis`), reused as
 *                   both the film URL and the booking link
 *   - `image`     → poster (`biletyna.pl/file/get/id/N`)
 *
 * biletyna.pl is a shared ticketing platform; the JSON-LD shape here is
 * generic enough that this parser could later back a shared biletyna client,
 * but for now this stays a self-contained per-cinema scraper. Films and
 * café/cultural screenings are all emitted as `ScreeningEvent`s with the same
 * shape, so there's no field to distinguish them on — we keep every screening.
 */
class KinoKameralneClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoKameralneClient.PageUrl)

  def fetch(): Seq[CinemaMovie] = KinoKameralneClient.parse(http.get(KinoKameralneClient.PageUrl), cinema)
}

object KinoKameralneClient {

  val PageUrl = "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe"

  // schema.org `startDate` is ISO-8601 with a zone offset; we keep only the
  // wall-clock LocalDateTime (the rest of the app reasons in Warsaw local time).
  private val IsoOffset = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  private case class RawSlot(title: String, dateTime: java.time.LocalDateTime, url: String, poster: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val slots = jsonLdBlocks(html).flatMap(parseEvents)

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, Some(s.url)))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = group.map(_.url).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  /** The bodies of every `<script type="application/ld+json">` block on the
   *  page. biletyna renders exactly one (the `Place`), but we scan all to stay
   *  robust to layout changes. */
  private def jsonLdBlocks(html: String): Seq[String] =
    Jsoup.parse(html)
      .select("script[type=application/ld+json]").asScala.toSeq
      .map(_.data())
      .filter(_.nonEmpty)

  /** Pull `ScreeningEvent`s out of one JSON-LD block. A `Place` node carries
   *  them under `events`; anything that doesn't parse or doesn't hold events
   *  yields nothing. */
  private def parseEvents(block: String): Seq[RawSlot] =
    Try(Json.parse(block)).toOption.toSeq.flatMap { json =>
      (json \ "events").asOpt[Seq[JsValue]].getOrElse(Seq.empty).flatMap(parseEvent)
    }

  private def parseEvent(ev: JsValue): Option[RawSlot] =
    for {
      title <- (ev \ "name").asOpt[String].map(_.trim).filter(_.nonEmpty)
      start <- (ev \ "startDate").asOpt[String]
      dt    <- Try(OffsetDateTime.parse(start, IsoOffset).toLocalDateTime).toOption
      url   <- (ev \ "url").asOpt[String].filter(_.nonEmpty)
    } yield RawSlot(
      title    = title,
      dateTime = dt,
      url      = url,
      poster   = (ev \ "image").asOpt[String].filter(_.nonEmpty)
    )
}
