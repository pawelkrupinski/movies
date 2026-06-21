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
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(pageUrl)
  override def sourceUrl: Option[String] = Some(pageUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = BiletynaClient.parse(http.get(pageUrl), cinema)
}

object BiletynaClient {

  // schema.org `startDate` is ISO-8601 with a zone offset; we keep only the
  // wall-clock LocalDateTime (the rest of the app reasons in Warsaw local time).
  private val IsoOffset = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  private case class RawSlot(eventType: Option[String], title: String, dateTime: java.time.LocalDateTime, url: String, poster: Option[String])

  /** schema.org `@type`s biletyna stamps on the venue's own LIVE stage/music
   *  programming, which shares the ticketing surface with its film screenings
   *  (a theatre play, a kabaret/stand-up night, a concert/recital, a film
   *  quiz). Films are `ScreeningEvent`; these are never films, so we drop them
   *  on the structured type — the high-precision signal the venue exposes —
   *  rather than the title (many, e.g. „Być Kobietą" — Czyli Szaleństwa
   *  Dojrzałej Młodości, a TheaterEvent, carry no event vocabulary for the
   *  title-based [[NonMovieEventClassifier]] to catch).
   *
   *  `ChildrensEvent` is deliberately NOT here: a real kids' film is tagged it
   *  too (e.g. „Willow i tajemniczy las"), so dropping the type would drop a
   *  film — the one regression worse than a stray event. Those are left to the
   *  title classifier ([[OnlyMovieEventsFilter]]), which catches the theatrical
   *  ones via their „…Teatralne popołudnie…" naming. Likewise any unrecognised
   *  type is kept: missing a non-film row is cosmetic, dropping a film is not. */
  private val NonFilmEventTypes = Set("TheaterEvent", "ComedyEvent", "MusicEvent", "DanceEvent", "Event")

  /** A non-film `@type` is NOT enough to drop an event: "event cinema"
   *  broadcasts (André Rieu, NT Live, a `retransmisja`) are screened content the
   *  app keeps, yet biletyna tags them `MusicEvent`/`TheaterEvent` just like a
   *  live concert. Honour the same broadcast veto the title classifier uses so
   *  these survive the structured-type filter. */
  private def isLiveEventType(slot: RawSlot): Boolean =
    slot.eventType.exists(NonFilmEventTypes) && !NonMovieEventClassifier.isScreenedBroadcast(slot.title)

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val slots = jsonLdBlocks(html).flatMap(parseEvents).filterNot(isLiveEventType)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, Some(s.url))) { (rawName, group, showtimes) =>
      val parsed = parseTitle(rawName)
      CinemaMovie(
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
      )
    }
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

  /** Pull every event out of one JSON-LD block, keeping its schema.org `@type`
   *  so [[parse]] can drop the venue's live stage/music programming (see
   *  [[NonFilmEventTypes]]). A `Place` node carries the events under `events`;
   *  anything that doesn't parse or doesn't hold events yields nothing. */
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
      eventType = (ev \ "@type").asOpt[String],
      title     = title,
      dateTime  = dt,
      url       = url,
      poster    = (ev \ "image").asOpt[String].filter(_.nonEmpty)
    )
}
