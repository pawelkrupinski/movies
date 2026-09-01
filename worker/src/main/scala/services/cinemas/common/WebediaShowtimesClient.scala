package services.cinemas.common

import tools.HttpFetch
import models._
import play.api.libs.json._

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Webedia website-JSON showtimes scraper — the surviving reach into the
 * AlloCiné / Filmstarts / SensaCine / Beyazperde family after the old
 * `api.allocine.fr` mobile REST API (partner key + SHA-1 `sig`) was
 * decommissioned: its whole `api.*` subdomain family (incl. `api.filmstarts.de`,
 * `api.screenrush.co.uk`) was removed from DNS in 2026, and the replacement
 * GraphQL backend (`graph.allocine.fr`) sits behind a per-session JWT.
 *
 * What still answers unauthenticated is the WEBSITE's own internal JSON, hit by
 * every Webedia country site with an identical path + schema — only the host,
 * the theater-id LETTER prefix and the (unused-here) localized city-list path
 * differ:
 *
 *   GET https://www.<host>/_/showtimes/theater-<id>/d-<YYYY-MM-DD>/p-<page>/
 *     → { results:[ { movie, showtimes }, … ],
 *         pagination:{ page, totalPages, itemsPerPage, totalItems }, nextDate, … }
 *
 * `results[].movie` carries `title` + `originalTitle` (a strong TMDB-resolution
 * hint for the German market, where the local title is a translation),
 * `data.productionYear`, `runtime` ("1 Std. 56 Min."), `genres`, the director
 * (`credits[]` where `position.name == "DIRECTOR"`) and a `poster`.
 * `results[].showtimes` is an object keyed by language-version bucket
 * (`original` / `dubbed` / `local` / …); each bucket is an array of screenings
 * whose `startsAt` is a local ISO `LocalDateTime` (no zone — it is already the
 * venue's wall-clock) and whose booking deep-link is `data.ticketing[].urls`.
 *
 * One instance serves one venue — its `theaterId` (Germany: "A0263", Spain:
 * "E0123") + the [[Cinema]] it feeds, mirroring [[FilmwebShowtimesClient]].
 * Everything that differs between the family's national sites is carried by
 * [[WebediaMarket]], so the same client serves Germany and Spain (and would
 * serve FR/TR/BR/MX by adding a market). TMDB enriches synopsis/cast downstream,
 * so this client only carries what the JSON actually provides.
 */
class WebediaShowtimesClient(
  http:      HttpFetch,
  market:    WebediaMarket,
  theaterId: String,              // e.g. "A0263" — the letter prefix is per-country
  override val cinema: Cinema,
  /** The day the horizon is measured from. `None` means "now, in the market's
   *  own zone" — an OPTION rather than a defaulted `LocalDate.now(market.zoneId)`
   *  because Scala 3 will not let a default argument read another parameter of
   *  the same list, and the market is where the zone lives. Same shape as
   *  [[FlicksClient]]. */
  today:     Option[LocalDate] = None
) extends ChunkedCinemaScraper {

  private val referenceDay: LocalDate = today.getOrElse(LocalDate.now(market.zoneId))

  import WebediaShowtimesClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(s"https://${market.host}")

  // The public, browser-renderable venue page. This ONE path is localized per
  // market (`/kinoprogramm/kino/` in Germany, `/cines/cine-` in Spain); the JSON
  // endpoint above is uniform across the family, so it is the only piece of the
  // URL shape that moved onto WebediaMarket when Spain landed.
  override def sourceUrl: Option[String] = Some(market.venuePageUrl(theaterId))

  // Each populated day is one chunk, run as its own `ScrapeChunk` task (see
  // ChunkedCinemaScraper / ScrapeChunkHandler). The days spread across the task
  // queue and the market's shared pace gate instead of bursting from a single
  // task that parks a worker thread for days×1s. The in-process `fetch()` the
  // trait composes (planChunks → fetchChunk → reduceChunks) is used only by the
  // deterministic fixture harness + unit tests.

  /** The days to scrape, read off the venue page's (`sourceUrl`)
   *  `data-showtimes-dates` attribute — the exact days that have screenings
   *  inside the site's own booking window, gap days excluded (verified empty in
   *  the per-day API) — ~28 days on Filmstarts, ~21 on SensaCine. Reading it once lifts the horizon from
   *  the old fixed 7-day grid to the site's full window WITHOUT firing a request
   *  per empty day: the page names precisely which days to fetch. Data does exist
   *  sparsely beyond that window, but it is unadvertised (no signal names those
   *  days) and it rolls into the window — and so into this list — well before its
   *  showtime, so a ~28-day horizon captures it in time without blind probing.
   *
   *  No fallback: this IS the nav/index fetch the `ChunkedCinemaScraper` contract
   *  allows, and its failure fails the whole scrape (recorded as a normal
   *  outcome). A fetch error propagates; a 200 that lacks the attribute entirely
   *  — the market's markup changed, or a block/error page came back — is a
   *  discovery failure too, so `parseShowtimeDates` returns `None` and we throw
   *  rather than silently scraping nothing. An attribute that IS present but
   *  lists no days is a legitimately empty venue (empty result, kept by the
   *  empty-guard), not a failure. Bounded to `[today, today+MaxHorizonDays]` so a
   *  stray attribute date can't balloon the chunk fan-out. */
  def planChunks(): Seq[String] = {
    val url  = sourceUrl.getOrElse(throw new IllegalStateException("WebediaShowtimesClient has no sourceUrl"))
    val html = http.get(url)
    parseShowtimeDates(html)
      .getOrElse(throw new IllegalStateException(
        s"$url carries no data-showtimes-dates attribute — ${market.host} markup changed?"))
      .filter(d => !d.isBefore(referenceDay) && !d.isAfter(referenceDay.plusDays(MaxHorizonDays.toLong)))
      .map(_.toString)
  }

  /** Fetch + parse ONE day into that day's films. A page-1 fetch failure THROWS
   *  so ONLY that day's chunk task reschedules (the per-day retry); the other
   *  days are unaffected. A day that ANSWERS with no films is a valid empty
   *  result, not a failure — Webedia serves an empty `results` for far-future
   *  days. Spillover pages (>20 films/day, rare) are best-effort: page 1 already
   *  answered, so a lost page 2 drops a few films rather than failing the day.
   *
   *  (The old monolithic fetch swallowed a day's failure to None and threw only
   *  if ALL days failed, to avoid feeding AdaptiveTimeoutScraper a fast-empty
   *  "success". Chunked scrapers skip that wrapper, and the total-failure case is
   *  now the empty reduce — no chunk lands → recordCinemaScrape's empty-guard
   *  keeps last-known data.) */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] = {
    val date  = LocalDate.parse(dateKey)
    val first = parsePage(http.get(showtimesUrl(market.host, theaterId, date, 1)), market)
    val extra = (2 to first.totalPages).flatMap { p =>
      Try(http.get(showtimesUrl(market.host, theaterId, date, p))).toOption.toSeq
        .flatMap(parsePage(_, market).films)
    }
    (first.films ++ extra).map(raw => toCinemaMovie(raw, raw.showtimes))
  }

  /** Merge every day's films into the venue's listing: one row per Webedia film
   *  id, showtimes unioned, deduped by (time, booking) and time-ordered — the same
   *  grouping the monolithic scrape used, so `reduceChunks ∘ fetchChunk ∘
   *  planChunks` equals the old `fetch()`. Overrides the identity default because
   *  Webedia rows carry no `filmUrl` (that default would collapse films to their
   *  title); the Webedia id (`externalIds("webedia")`) is the stable key. A film
   *  with no showtimes on any day drops out. */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    chunks.toSeq.sortBy(_._1).flatMap(_._2)
      .groupBy(m => m.externalIds.getOrElse("webedia", m.movie.title))
      .toSeq.sortBy(_._1)
      .flatMap { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes)
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None else Some(group.head.copy(showtimes = showtimes))
      }

  /** Build the venue-agnostic film row from one parsed Webedia result. The
   *  showtimes to attach are supplied by the caller — one day's in `fetchChunk`,
   *  the cross-day union in `reduceChunks`. */
  private def toCinemaMovie(raw: RawWebediaFilm, showtimes: Seq[Showtime]): CinemaMovie =
    CinemaMovie(
      movie = Movie(
        title          = raw.title,
        runtimeMinutes = raw.runtimeMinutes,
        releaseYear    = raw.year,
        genres         = raw.genres,
        // Carry the international title only when it differs from the German one —
        // for German films the site echoes the same string there.
        originalTitle  = raw.originalTitle.filter(_ != raw.title)
      ),
      cinema      = cinema,
      ageRating   = raw.ageRating,
      posterUrl   = raw.posterUrl,
      filmUrl     = None,   // no stable public film-page URL is derivable from the JSON
      synopsis    = raw.synopsis,
      cast        = Seq.empty,
      director    = raw.director,
      showtimes   = showtimes,
      externalIds = Map("webedia" -> raw.internalId.toString)
    )
}

object WebediaShowtimesClient {

  /** The shared scrape horizon — see [[services.cinemas.common.ScrapeHorizon]].
   *
   *  Was 34, chosen as "a touch above" Filmstarts' own ~28-day booking calendar.
   *  That reasoning is right about what Filmstarts advertises TODAY (a venue page
   *  probed 2026-08-30 offered 7 dates reaching +21d, and the German corpus's
   *  furthest screening sat at +28d) and wrong as a design: it is a per-client cap
   *  of exactly the kind `ScrapeHorizon` exists to abolish. The day list still
   *  comes from the venue page's own `data-showtimes-dates`, so this bound costs
   *  NOTHING while the source stays short — it only stops mattering the day the
   *  source's window grows, which the old comment itself anticipated ("or a future
   *  window bump").
   *
   *  That day is the dangerous one, because the failure is silent and total:
   *  `MovieCache`'s scrape-prune reads a film's absence from a listing as "it
   *  stopped screening", so any film living only beyond the cap is deleted on
   *  every COMPLETE scrape — no error, no failed fetch. That is precisely how the
   *  UK lost its whole advance-sale programme on 2026-07-27, and Germany has no
   *  aggregator fallback to have caught it. Sharing the one number is the cheap
   *  insurance. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  def showtimesUrl(host: String, theaterId: String, date: LocalDate, page: Int): String =
    s"https://$host/_/showtimes/theater-$theaterId/d-$date/p-$page/"

  private val ShowtimesDatesAttr = """data-showtimes-dates="([^"]*)"""".r
  private val IsoDate            = """\d{4}-\d{2}-\d{2}""".r

  /** The days a venue has showtimes on, read off the venue page's
   *  `data-showtimes-dates="[&quot;2026-07-19&quot;,…]"` — an HTML-entity-escaped
   *  JSON array of ISO dates spanning the site's own booking window with gap
   *  days omitted (~28 days on Filmstarts, ~21 on SensaCine). The entity-escaping is irrelevant to a date regex, so pull the
   *  ISO dates straight out of the attribute value; deduped and sorted.
   *
   *  `None` when the attribute is ABSENT — unexpected markup, so the caller fails
   *  the scrape rather than scrape nothing. `Some(dates)` when present, where
   *  `Some(Nil)` (an empty `[]`) is a legitimately empty venue, not a failure.
   *  Pure + public so a spec feeds it a recorded page directly. */
  def parseShowtimeDates(html: String): Option[Seq[LocalDate]] =
    ShowtimesDatesAttr.findFirstMatchIn(html).map { m =>
      IsoDate.findAllIn(m.group(1)).toSeq.flatMap(s => Try(LocalDate.parse(s)).toOption).distinct.sortBy(_.toString)
    }

  /** One parsed page: its films plus the response's total page count (so the
   *  caller knows whether to fetch p-2…). Pure so a spec can feed fixture bytes. */
  case class Page(films: Seq[RawWebediaFilm], totalPages: Int)

  /** The fields this client needs off one `results[]` element. */
  case class RawWebediaFilm(
    internalId:     Long,
    title:          String,
    originalTitle:  Option[String],
    year:           Option[Int],
    runtimeMinutes: Option[Int],
    genres:         Seq[String],
    director:       Seq[String],
    posterUrl:      Option[String],
    synopsis:       Option[String],
    ageRating:      Option[String],
    showtimes:      Seq[Showtime]
  )

  /** Parse one `/_/showtimes/theater-<id>/d-<date>/p-<n>/` response. Pure +
   *  public so the spec feeds it the recorded JSON directly. Takes the market
   *  because two things inside a result are language-shaped rather than
   *  structural: the `runtime` string's unit words and the version tokens. */
  def parsePage(json: String, market: WebediaMarket): Page = {
    val js = Try(Json.parse(json)).getOrElse(JsNull)
    val totalPages = (js \ "pagination" \ "totalPages").asOpt[Int].getOrElse(1)
    val films = (js \ "results").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(parseResult(_, market))
    Page(films, totalPages)
  }

  private def parseResult(js: JsValue, market: WebediaMarket): Option[RawWebediaFilm] = {
    val movie = js \ "movie"
    for {
      id    <- (movie \ "internalId").asOpt[Long]
      title <- (movie \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
    } yield RawWebediaFilm(
      internalId     = id,
      title          = title,
      originalTitle  = (movie \ "originalTitle").asOpt[String].map(_.trim).filter(_.nonEmpty),
      year           = (movie \ "data" \ "productionYear").asOpt[Int],
      runtimeMinutes = (movie \ "runtime").asOpt[String].flatMap(parseRuntime(_, market)),
      genres         = (movie \ "genres").asOpt[Seq[JsValue]].getOrElse(Nil)
        .flatMap(g => (g \ "translate").asOpt[String]).map(_.trim).filter(_.nonEmpty),
      director       = parseDirectors(movie \ "credits"),
      posterUrl      = (movie \ "poster" \ "url").asOpt[String].map(_.trim).filter(_.nonEmpty),
      // `synopsisFull` is HTML (`<p class="bo-p">…</p>` per paragraph), not prose —
      // flatten it, keeping the paragraph breaks, or the tags render as visible text.
      synopsis       = (movie \ "synopsisFull").asOpt[String]
        .map(tools.TextNormalization.stripHtmlKeepingParagraphs).filter(_.nonEmpty),
      // German FSK certificate off the first `releases[]` element that carries one
      // (a film's earliest releases can lack a certificate while a later one has
      // it). Kept verbatim — German "FSK 6", Spanish "+12" — because the local
      // spelling IS the recognisable label; `normalize` only drops blank/placeholder codes.
      ageRating      = AgeRating.normalize(
        (movie \ "releases").asOpt[Seq[JsValue]].getOrElse(Nil)
          .flatMap(r => (r \ "certificate" \ "code").asOpt[String]).headOption
          .map(market.certificateLabel)),
      showtimes      = parseShowtimes(js \ "showtimes", market)
    )
  }

  /** Director names off `credits[]` — every credit whose `position.name` is
   *  DIRECTOR, "First Last" (either part may be absent). */
  private def parseDirectors(credits: JsLookupResult): Seq[String] =
    credits.asOpt[Seq[JsValue]].getOrElse(Nil).collect {
      case c if (c \ "position" \ "name").asOpt[String].contains("DIRECTOR") =>
        val first = (c \ "person" \ "firstName").asOpt[String].getOrElse("")
        val last  = (c \ "person" \ "lastName").asOpt[String].getOrElse("")
        s"$first $last".trim
    }.filter(_.nonEmpty).distinct

  /** Flatten the version-bucketed `showtimes` object into a flat screening list.
   *  Each bucket (`original`/`dubbed`/`local`/…) is an array; the language
   *  version + projection become [[Showtime.format]] tokens. */
  private def parseShowtimes(js: JsLookupResult, market: WebediaMarket): Seq[Showtime] =
    js.asOpt[JsObject].map(_.fields.toSeq).getOrElse(Seq.empty).flatMap { case (_, bucket) =>
      bucket.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty).flatMap { s =>
        (s \ "startsAt").asOpt[String].flatMap(parseLocalDateTime).map { dt =>
          val booking = (s \ "data" \ "ticketing").asOpt[Seq[JsValue]].getOrElse(Nil)
            .flatMap(t => (t \ "urls").asOpt[Seq[String]].getOrElse(Nil))
            .headOption.map(cleanBookingUrl)
          Showtime(dt, booking, None, formatTokens((s \ "tags").asOpt[Seq[String]].getOrElse(Nil), market))
        }
      }
    }

  /** Format/version tokens for one screening, read from the clean namespaced
   *  `tags` (`Format.Projection.3d`, `Localization.Version.Original`,
   *  `Localization.Subtitle.*`) rather than the noisier `projection`/
   *  `diffusionVersion` fields (which spell 3D as `F_3D` and leave the original/
   *  subtitled version encoded only in the bucket + tags): the non-digital
   *  projection formats (3D, IMAX…) plus a language token — the market's own
   *  original-version abbreviation (`OV` in Germany, `VO` in Spain) and its
   *  subtitled one (`OmU` / `VOSE`). A plain dubbed digital screening yields no
   *  token: dubbing is the default in both markets, so it is the un-marked case.
   *
   *  Public, and taking the raw tag list rather than the screening object, so a
   *  spec can pin a tag COMBINATION the recorded captures do not hold — the same
   *  reason [[GatsbyBoxOfficeParser.formatTokens]] is public. A single day's
   *  capture of one venue is usually all-dubbed-digital (both of ours are), so
   *  the version branches would otherwise ship untested. */
  def formatTokens(rawTags: Seq[String], market: WebediaMarket): List[String] = {
    val tags = rawTags.map(_.toLowerCase)
    val projection = List(
      "format.projection.3d"   -> "3D",
      "format.projection.imax" -> "IMAX",
      "format.projection.4dx"  -> "4DX",
      "format.projection.dolby" -> "DOLBY"
    ).collect { case (needle, token) if tags.exists(_.contains(needle)) => token }
    val language =
      if (tags.exists(_.contains("subtitle")))              List(market.subtitledToken)
      else if (tags.exists(_.contains("version.original"))) List(market.originalVersionToken)
      else                                                  Nil
    (projection ++ language).distinct
  }

  private def parseLocalDateTime(s: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(s.trim)).toOption

  /** German "1 Std. 56 Min." → 116, Spanish "1h 56min" → 116, "56 Min." → 56.
   *  `None` when neither part is present.
   *
   *  The unit words come from the market, and the match is CASE-SENSITIVE on
   *  purpose. Germany's "Min" and Spain's "min" are the same letters, so a
   *  case-insensitive match would let the German market read a Spanish
   *  "1h 56min" as 56 minutes — an hour short, silently, and plausible enough
   *  that nothing downstream would flag it. Case-sensitivity makes a market
   *  applied to the wrong payload produce NOTHING rather than something wrong. */
  private def parseRuntime(s: String, market: WebediaMarket): Option[Int] = {
    def part(marker: String): Int =
      s"(\\d+)\\s*${java.util.regex.Pattern.quote(marker)}".r
        .findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    Some(part(market.hourMarker) * 60 + part(market.minuteMarker)).filter(_ > 0)
  }

  /** The relay booking URLs arrive with a trailing "; SSR" render marker glued
   *  on ("…&code=2D; SSR"); take the URL up to the first whitespace and drop a
   *  stray trailing ";". */
  private def cleanBookingUrl(url: String): String =
    url.trim.split("\\s+").headOption.getOrElse(url).stripSuffix(";")
}
