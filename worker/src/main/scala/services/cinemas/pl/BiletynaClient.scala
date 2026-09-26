package services.cinemas.pl

import tools.HttpFetch
import models._
import play.api.libs.json._
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

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
 * The place page stops at the venue's first 50 events (every busy venue shows
 * exactly 50: Kino Kameralne's run three weeks out of the 132 it sells). When
 * it is full, the rest comes from `/ajax/events?params[h]=<hall>` — the feed
 * the page's own "more events" scroller reads, keyed by the hall id the page
 * declares in its `get_filter` script variable — paged `ipp` at a time until
 * a page comes back short. Its records carry the same event id, title and
 * start time, and a `category_id` in place of the `@type`
 * ([[BiletynaClient.EventTypeByCategory]]). The two lists are merged on the
 * booking link, since the feed can omit an event the page still shows.
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
class BiletynaClient(http: HttpFetch, pageUrl: String, override val cinema: Cinema,
                     // The hall this page lists, where a venue has a page per hall
                     // (see `MultiListingScraper`); stamped on every showtime.
                     room: Option[String] = None)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(pageUrl)
  override def sourceUrl: Option[String] = Some(pageUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val html = http.get(pageUrl)
    BiletynaClient.parse(html, cinema, BiletynaClient.remainingEvents(http, pageUrl, html))
      .map(m => m.copy(showtimes = m.showtimes.map(_.copy(room = room))))
  }
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

  /** The events the place page lists (its JSON-LD), plus `more` — the feed
   *  records past the page's cap ([[remainingEvents]]) — merged on the booking
   *  link, so an event both carry counts once. */
  def parse(html: String, cinema: Cinema, more: Seq[JsValue] = Seq.empty): Seq[CinemaMovie] = {
    val slots = (jsonLdBlocks(html).flatMap(parseEvents) ++ more.flatMap(parseFeedEvent))
      .distinctBy(_.url)
      .filterNot(isLiveEventType)

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

  /** How many events the place page renders at most; a page holding exactly
   *  this many has more behind it. */
  private[pl] val PageEventCap = 50
  private val FeedPageSize    = 100
  // A venue with more than 2,000 upcoming events is a feed ignoring `page`,
  // not a programme; stop rather than loop.
  private val MaxFeedPages    = 20

  private val BiletynaOrigin = "https://biletyna.pl"
  private val HallFilter     = """get_filter\s*=\s*(\{.*?\});""".r

  /** The feed records for a capped place page (empty when the page isn't
   *  full), paged until one comes back short. Throws when the page is full but
   *  names no hall, or the feed never ends: a venue silently cut at 50 events is the failure this exists to
   *  prevent, so it fails loudly instead. */
  private[pl] def remainingEvents(http: HttpFetch, pageUrl: String, html: String): Seq[JsValue] =
    if (pageEventCount(html) < PageEventCap) Seq.empty
    else {
      val hall = HallFilter.findFirstMatchIn(html)
        .flatMap(m => (Json.parse(m.group(1)) \ "0" \ "hall_id").asOpt[Long])
        .getOrElse(throw new IllegalStateException(s"$pageUrl lists $PageEventCap events but names no hall to page the rest from"))
      val origin = CinemaScraper.hostsOf(pageUrl).headOption.fold(BiletynaOrigin)(host => s"https://$host")
      @annotation.tailrec
      def fetchFrom(page: Int, acc: Vector[JsValue]): Vector[JsValue] = {
        if (page > MaxFeedPages)
          throw new IllegalStateException(s"$pageUrl: event feed for hall $hall did not end after $MaxFeedPages pages")
        val records = feedRecords(http.get(s"$origin/ajax/events?params%5Bh%5D=$hall&h=$hall&ipp=$FeedPageSize&page=$page"))
        if (records.size < FeedPageSize) acc ++ records else fetchFrom(page + 1, acc ++ records)
      }
      fetchFrom(1, Vector.empty)
    }

  // Every event the page lists, parseable or not — the cap counts them all.
  private def pageEventCount(html: String): Int =
    jsonLdBlocks(html).map { block =>
      Try(Json.parse(block)).toOption.flatMap(json => (json \ "events").asOpt[JsArray]).fold(0)(_.value.size)
    }.sum

  private def feedRecords(json: String): Seq[JsValue] =
    (Json.parse(json) \ "events").toOption.toSeq.flatMap {
      case o: JsObject => o.values.toSeq
      case a: JsArray  => a.value.toSeq
      case _           => Seq.empty
    }

  /** The feed's `category_id` for each schema.org `@type` the place page stamps
   *  on the same event — measured over every catalogued venue on 2026-09-26
   *  (1,526 events on both lists, every id mapping to exactly one type). An
   *  unknown id maps to no type, which [[isLiveEventType]] keeps. */
  private[pl] val EventTypeByCategory: Map[Int, String] = Map(
    14 -> "ScreeningEvent",
    3  -> "TheaterEvent", 37 -> "TheaterEvent", 38 -> "TheaterEvent",
    4  -> "MusicEvent",
    2  -> "ComedyEvent", 17 -> "ComedyEvent",
    7  -> "Event",
    13 -> "ChildrensEvent",
    36 -> "Festival",
  )

  private val FeedDateTime = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private def parseFeedEvent(ev: JsValue): Option[RawSlot] =
    for {
      eventId <- (ev \ "event_id").asOpt[Long]
      title   <- (ev \ "artist_name").asOpt[String].map(_.trim).filter(_.nonEmpty)
      start   <- (ev \ "event_date").asOpt[String]
      dt      <- Try(java.time.LocalDateTime.parse(start, FeedDateTime)).toOption
      film    <- (ev \ "v2_artist_seo_url").asOpt[String].filter(_.nonEmpty)
    } yield RawSlot(
      eventType = (ev \ "category_id").asOpt[Int].flatMap(EventTypeByCategory.get),
      title     = title,
      dateTime  = dt,
      url       = s"$BiletynaOrigin$film?eid=$eventId#opis",
      poster    = (ev \ "thumb_file_id").asOpt[Long].map(id => s"$BiletynaOrigin/file/get/id/$id")
    )
}
