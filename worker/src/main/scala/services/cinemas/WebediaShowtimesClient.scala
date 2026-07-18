package services.cinemas

import models._
import play.api.libs.json._
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
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
 * One instance serves one venue — its `theaterId` (Germany: "A0263") + the
 * [[Cinema]] it feeds, mirroring [[FilmwebShowtimesClient]]. Host-parameterized
 * so the same client covers FR/ES/TR by swapping the host; only Germany
 * (`www.filmstarts.de`) is wired today. TMDB enriches synopsis/cast downstream,
 * so this client only carries what the JSON actually provides.
 */
class WebediaShowtimesClient(
  http:      HttpFetch,
  host:      String,              // e.g. "www.filmstarts.de"
  theaterId: String,              // e.g. "A0263" — the letter prefix is per-country
  override val cinema: Cinema,
  daysAhead: Int       = 6,
  today:     LocalDate = LocalDate.now(ZoneId.of("Europe/Berlin"))
) extends CinemaScraper {

  import WebediaShowtimesClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(s"https://$host")

  // The public, browser-renderable venue page. This ONE path segment
  // (`/kinoprogramm/kino/`) is Filmstarts/German-localized; the JSON endpoint
  // above is uniform across the family. Parameterize it when a second country lands.
  override def sourceUrl: Option[String] = Some(s"https://$host/kinoprogramm/kino/$theaterId/")

  def fetch(): Seq[CinemaMovie] = {
    val dates = (0 to daysAhead).map(today.plusDays(_))

    // One first page per date, fetched in parallel + tolerantly: a day whose
    // request fails or won't parse contributes nothing rather than failing the
    // batch (Webedia routinely serves an empty `results` for far-future days).
    val firstByDate = ParallelDetailFetch.keyed(
      "webedia-showtimes-day", dates, 1.minute, maxConcurrent = 2
    )(d => showtimesUrl(host, theaterId, d, 1)) { url =>
      Try(http.get(url)).toOption.map(parsePage)
    }

    // Tolerating a partial failure is right; tolerating a TOTAL one is not. If no
    // day answered at all the venue's showtimes are unknown, not absent — and
    // returning an empty Seq here reports it as a healthy, quick, film-less scrape:
    // `AdaptiveTimeoutScraper` records that duration as a SUCCESS, dragging the
    // host's rolling median down until the budget pins at its floor and starts
    // cutting the venues that ARE answering. Filmstarts 429ing every request on
    // 2026-07-18 is exactly that case. Fail loudly instead, so the scrape is
    // recorded as the failure it is and feeds no baseline.
    val answered = dates.flatMap(d => firstByDate.getOrElse(d, None))
    if (answered.isEmpty)
      throw new java.io.IOException(
        s"${cinema.displayName}: all ${dates.size} showtime requests to $host failed (theater $theaterId)")

    val firstFilms = dates.flatMap(d => firstByDate.getOrElse(d, None).toSeq.flatMap(_.films))

    // A day with >20 films spills onto extra pages (rare); fetch those too.
    val extraUrls = dates.flatMap { d =>
      firstByDate.getOrElse(d, None).toSeq.flatMap { page =>
        (2 to page.totalPages).map(p => showtimesUrl(host, theaterId, d, p))
      }
    }
    val extraByUrl = ParallelDetailFetch.keyed(
      "webedia-showtimes-page", extraUrls, 1.minute, maxConcurrent = 2
    )(identity) { url =>
      Try(http.get(url)).toOption.toSeq.flatMap(parsePage(_).films)
    }
    val allFilms = firstFilms ++ extraUrls.flatMap(u => extraByUrl.getOrElse(u, Seq.empty))

    // One row per film (same `internalId` across dates/pages), showtimes merged,
    // deduped by (time, booking) and time-ordered.
    allFilms.groupBy(_.internalId).toSeq.flatMap { case (id, group) =>
      val showtimes = group.flatMap(_.showtimes)
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie = Movie(
            title          = head.title,
            runtimeMinutes = head.runtimeMinutes,
            releaseYear    = head.year,
            genres         = head.genres,
            // Carry the international title only when it differs from the German
            // one — for German films the site echoes the same string there.
            originalTitle  = head.originalTitle.filter(_ != head.title)
          ),
          cinema      = cinema,
          posterUrl   = head.posterUrl,
          filmUrl     = None,   // no stable public film-page URL is derivable from the JSON
          synopsis    = head.synopsis,
          cast        = Seq.empty,
          director    = head.director,
          showtimes   = showtimes,
          externalIds = Map("webedia" -> id.toString)
        ))
      }
    }
  }
}

object WebediaShowtimesClient {

  def showtimesUrl(host: String, theaterId: String, date: LocalDate, page: Int): String =
    s"https://$host/_/showtimes/theater-$theaterId/d-$date/p-$page/"

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
    showtimes:      Seq[Showtime]
  )

  /** Parse one `/_/showtimes/theater-<id>/d-<date>/p-<n>/` response. Pure +
   *  public so the spec feeds it the recorded JSON directly. */
  def parsePage(json: String): Page = {
    val js = Try(Json.parse(json)).getOrElse(JsNull)
    val totalPages = (js \ "pagination" \ "totalPages").asOpt[Int].getOrElse(1)
    val films = (js \ "results").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty).flatMap(parseResult)
    Page(films, totalPages)
  }

  private def parseResult(js: JsValue): Option[RawWebediaFilm] = {
    val movie = js \ "movie"
    for {
      id    <- (movie \ "internalId").asOpt[Long]
      title <- (movie \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
    } yield RawWebediaFilm(
      internalId     = id,
      title          = title,
      originalTitle  = (movie \ "originalTitle").asOpt[String].map(_.trim).filter(_.nonEmpty),
      year           = (movie \ "data" \ "productionYear").asOpt[Int],
      runtimeMinutes = (movie \ "runtime").asOpt[String].flatMap(parseRuntime),
      genres         = (movie \ "genres").asOpt[Seq[JsValue]].getOrElse(Nil)
        .flatMap(g => (g \ "translate").asOpt[String]).map(_.trim).filter(_.nonEmpty),
      director       = parseDirectors(movie \ "credits"),
      posterUrl      = (movie \ "poster" \ "url").asOpt[String].map(_.trim).filter(_.nonEmpty),
      // `synopsisFull` is HTML (`<p class="bo-p">…</p>` per paragraph), not prose —
      // flatten it, keeping the paragraph breaks, or the tags render as visible text.
      synopsis       = (movie \ "synopsisFull").asOpt[String]
        .map(tools.TextNormalization.stripHtmlKeepingParagraphs).filter(_.nonEmpty),
      showtimes      = parseShowtimes(js \ "showtimes")
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
  private def parseShowtimes(js: JsLookupResult): Seq[Showtime] =
    js.asOpt[JsObject].map(_.fields.toSeq).getOrElse(Seq.empty).flatMap { case (_, bucket) =>
      bucket.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty).flatMap { s =>
        (s \ "startsAt").asOpt[String].flatMap(parseLocalDateTime).map { dt =>
          val booking = (s \ "data" \ "ticketing").asOpt[Seq[JsValue]].getOrElse(Nil)
            .flatMap(t => (t \ "urls").asOpt[Seq[String]].getOrElse(Nil))
            .headOption.map(cleanBookingUrl)
          Showtime(dt, booking, None, formatTokens(s))
        }
      }
    }

  /** Format/version tokens for one screening, read from the clean namespaced
   *  `tags` (`Format.Projection.3d`, `Localization.Version.Original`,
   *  `Localization.Subtitle.*`) rather than the noisier `projection`/
   *  `diffusionVersion` fields (which spell 3D as `F_3D` and leave the original/
   *  subtitled version encoded only in the bucket + tags): the non-digital
   *  projection formats (3D, IMAX…) plus a language token — `OV` for an
   *  original-version screening, `OmU` when subtitled. A plain dubbed digital
   *  screening yields no token (the German default). */
  private def formatTokens(s: JsValue): List[String] = {
    val tags = (s \ "tags").asOpt[Seq[String]].getOrElse(Nil).map(_.toLowerCase)
    val projection = List(
      "format.projection.3d"   -> "3D",
      "format.projection.imax" -> "IMAX",
      "format.projection.4dx"  -> "4DX",
      "format.projection.dolby" -> "DOLBY"
    ).collect { case (needle, token) if tags.exists(_.contains(needle)) => token }
    val language =
      if (tags.exists(_.contains("subtitle")))          List("OmU")
      else if (tags.exists(_.contains("version.original"))) List("OV")
      else                                              Nil
    (projection ++ language).distinct
  }

  private def parseLocalDateTime(s: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(s.trim)).toOption

  /** "1 Std. 56 Min." → 116; "56 Min." → 56. `None` when neither part is present. */
  private def parseRuntime(s: String): Option[Int] = {
    val hours   = """(\d+)\s*Std""".r.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    val minutes = """(\d+)\s*Min""".r.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    Some(hours * 60 + minutes).filter(_ > 0)
  }

  /** The relay booking URLs arrive with a trailing "; SSR" render marker glued
   *  on ("…&code=2D; SSR"); take the URL up to the first whitespace and drop a
   *  stray trailing ";". */
  private def cleanBookingUrl(url: String): String =
    url.trim.split("\\s+").headOption.getOrElse(url).stripSuffix(";")
}
