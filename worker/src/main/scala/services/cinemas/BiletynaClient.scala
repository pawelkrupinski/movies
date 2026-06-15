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
 * Generic client for any cinema ticketed through the biletyna.pl platform. The
 * venue's place page (`biletyna.pl/<City>/<Venue>`) is server-rendered and
 * carries a single `<script type="application/ld+json">` block: a schema.org
 * `Place` whose `events` array is the full programme, one `ScreeningEvent` per
 * screening. Everything we need is in it — no per-film detail fetch:
 *   - `name`      → film title (some venues publish a descriptive
 *                   `„Title" | reżyseria: Director | Country Year` form; see
 *                   [[BiletynaClient.parseTitle]])
 *   - `startDate` → ISO-8601 with offset ("2026-06-06T18:00:00+02:00")
 *   - `url`       → biletyna film page (`/film/<slug>?eid=N#opis`), reused as
 *                   both the film URL and the booking link
 *   - `image`     → poster (`biletyna.pl/file/get/id/N`)
 *
 * One instance per venue, captured by its `pageUrl` + `cinema`, so adding a
 * biletyna-hosted cinema is a new catalog line, not a new client (OCP). Known
 * venues: ADA Kino Studyjne (Warszawa), Kino Kameralne Cafe (Gdańsk) and Kino
 * Pegaz / WCK (Wodzisław Śląski — previously scraped from Filmweb, which had
 * silently gone empty for it).
 *
 * biletyna.pl 403s our datacenter IP (Cloudflare waiting-room), so the catalog
 * routes these through the `bnFetch` seam — Zyte's residential egress in
 * production, the fixture fake in tests.
 *
 * @parameter http    HTTP client (the biletyna fetch seam in production).
 * @parameter pageUrl The venue's biletyna place page, e.g.
 *                `https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe`.
 * @parameter cinema  The [[Cinema]] source tag attached to every [[CinemaMovie]].
 */
class BiletynaClient(http: HttpFetch, pageUrl: String, override val cinema: Cinema)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(pageUrl)
  override def sourceUrl: Option[String] = Some(pageUrl)

  def fetch(): Seq[CinemaMovie] = BiletynaClient.parse(http.get(pageUrl), cinema)
}

object BiletynaClient {

  // schema.org `startDate` is ISO-8601 with a zone offset; we keep only the
  // wall-clock LocalDateTime (the rest of the app reasons in Warsaw local time).
  private val IsoOffset = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  private case class RawSlot(title: String, dateTime: java.time.LocalDateTime, url: String, poster: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val slots = jsonLdBlocks(html).flatMap(parseEvents)

    slots.groupBy(_.title).toSeq.flatMap { case (rawName, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, Some(s.url)))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val parsed = parseTitle(rawName)
        Some(CinemaMovie(
          movie     = Movie(
            title       = parsed.title,
            releaseYear = parsed.year,
            countries   = parsed.countries,
            rawTitle    = parsed.rawTitle
          ),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = group.map(_.url).headOption,
          synopsis  = None,
          cast      = Seq.empty,
          director  = parsed.directors,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }

  /** The film title plus any metadata split out of biletyna's descriptive
   *  title form (see [[parseTitle]]). For a plain title `rawTitle` is `None`,
   *  the lists empty — the client did no cleanup. */
  private case class ParsedTitle(
    title:     String,
    rawTitle:  Option[String],
    directors: Seq[String],
    countries: Seq[String],
    year:      Option[Int]
  )

  // A `reżyseria: …` segment carrying one or more comma-separated directors.
  private val DirectorSegment = """(?i)reżyseria\s*:\s*(.+)""".r
  // A trailing `Country[, Country…] Year` segment, e.g. "Czechy 2025" or
  // "Polska, Francja 2024".
  private val CountryYearSegment = """(.+?)\s+(\d{4})""".r

  /** Most biletyna venues publish the bare film title as the JSON-LD `name`,
   *  but some (e.g. Kinoteatr Rondo) publish the descriptive form
   *  `„Title" | reżyseria: Director[, …] | Country[, …] Year`. When the
   *  `reżyseria:` marker is present we treat the `|`-separated string as that
   *  form: the first segment (unquoted) is the title, and director / countries
   *  / year are lifted into their own fields. Without the marker the string is
   *  returned verbatim — a pipe in a plain title (a concert "Artist | Venue")
   *  must not be mistaken for a metadata separator. */
  private def parseTitle(raw: String): ParsedTitle = {
    val segments = raw.split('|').map(_.trim).filter(_.nonEmpty).toSeq
    if (segments.length < 2 || !segments.exists(DirectorSegment.matches))
      ParsedTitle(raw.trim, None, Seq.empty, Seq.empty, None)
    else {
      val directors = segments.collectFirst { case DirectorSegment(ds) => splitList(ds) }.getOrElse(Seq.empty)
      val (countries, year) = segments.tail.collectFirst {
        case CountryYearSegment(cs, y) => (splitList(cs), y.toIntOption)
      }.getOrElse((Seq.empty, None))
      ParsedTitle(stripQuotes(segments.head), Some(raw), directors, countries, year)
    }
  }

  private def splitList(s: String): Seq[String] =
    s.split(',').map(_.trim).filter(_.nonEmpty).toSeq

  // Strip the wrapping quotation marks biletyna puts around the title: Polish
  // „ … " (U+201E … U+201D), curly " … " (U+201C … U+201D) or plain ".
  private def stripQuotes(s: String): String =
    s.replaceAll("""^["„“]+|["”]+$""", "").trim

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
