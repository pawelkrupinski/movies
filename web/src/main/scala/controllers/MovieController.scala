package controllers

import models._
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import play.api.Mode
import services.movies.TitleNormalizer
import services.readmodel.WebReadModel

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime}

case class CinemaShowtimes(cinema: Cinema, showtimes: Seq[Showtime])

// ── JSON API types ──────────────────────────────────────────────────────
case class ApiShowtime(time: String, format: String, room: Option[String], bookingURL: Option[String])
case class ApiCinemaShowings(cinema: String, cinemaURL: Option[String], showtimes: Seq[ApiShowtime])
case class ApiDayShowings(date: String, label: String, cinemas: Seq[ApiCinemaShowings])
case class ApiRatings(
  imdb: Option[Double], imdbURL: Option[String],
  metascore: Option[Int], metacriticURL: Option[String],
  rottenTomatoes: Option[Int], rottenTomatoesURL: Option[String],
  filmweb: Option[Double], filmwebURL: Option[String]
)
case class ApiFilm(
  title: String, slug: String, posterURL: Option[String], fallbackPosterURLs: Seq[String],
  runtimeMinutes: Option[Int], releaseYear: Option[Int], genres: Seq[String],
  // Age rating / certificate (UK BBFC "15"/"PG"/…); omitted when the film has none.
  ageRating: Option[String],
  ratings: ApiRatings,
  countries: Seq[String], directors: Seq[String], cast: Seq[String],
  showings: Seq[ApiDayShowings]
)

/** Detail-only payload for `GET /api/details`: the heavy text (synopsis) and
 *  trailers that the grid / filters never need. Split off the listing so the
 *  latency-sensitive `/api/repertoire` stays lean; clients fetch both in
 *  parallel and merge by `title`. Only films carrying a synopsis or at least
 *  one trailer are emitted. */
case class ApiFilmDetails(
  title: String, originalTitle: Option[String], synopsis: Option[String], trailerURLs: Seq[String]
)

object ApiFilmDetails {
  implicit val writes: Writes[ApiFilmDetails] = Json.writes[ApiFilmDetails]

  def from(fs: FilmSchedule): ApiFilmDetails = ApiFilmDetails(
    title         = fs.movie.title,
    // The genuinely-distinct original title and the embed-ready trailer URLs are
    // pre-resolved on the read-model document (the redundancy check + URL transform
    // ran at projection time), so clients render them unconditionally.
    originalTitle = fs.resolved.originalTitle,
    synopsis      = fs.synopsis,
    trailerURLs   = fs.resolved.trailerUrls,
  )

  def hasContent(d: ApiFilmDetails): Boolean =
    d.synopsis.nonEmpty || d.trailerURLs.nonEmpty || d.originalTitle.nonEmpty
}

// ── Cinema universe + area grouping (static per city) ──────────────────────
/** One collapsible cinema group in a split city: its [[CinemaArea]] label +
 *  stable slug, and the display names of the venues it holds. */
case class ApiCinemaArea(name: String, slug: String, cinemas: Seq[String])
/** `GET /:city/api/cinemas` — the city's full cinema universe (every venue, in
 *  city order, including ones with no showings today) plus its area grouping.
 *  A flat city returns an empty `areas`; a split city (e.g. London) returns one
 *  entry per compass area. Lets the mobile filter render the same collapsible,
 *  per-area (de)selectable list the web filter builds server-side. */
case class ApiCityCinemas(cinemas: Seq[String], areas: Seq[ApiCinemaArea])

object ApiCityCinemas {
  implicit val apiCinemaAreaWrites: Writes[ApiCinemaArea] = Json.writes[ApiCinemaArea]
  implicit val writes: Writes[ApiCityCinemas] = Json.writes[ApiCityCinemas]

  def from(city: City): ApiCityCinemas = ApiCityCinemas(
    cinemas = city.cinemaDisplayNames,
    areas   = city.areas.map(g => ApiCinemaArea(g.area.label, g.area.slug, g.cinemaDisplayNames)),
  )
}

object ApiFilm {
  implicit val apiShowtimeWrites: Writes[ApiShowtime] = Json.writes[ApiShowtime]
  implicit val apiCinemaShowingsWrites: Writes[ApiCinemaShowings] = Json.writes[ApiCinemaShowings]
  implicit val apiDayShowingsWrites: Writes[ApiDayShowings] = Json.writes[ApiDayShowings]
  implicit val apiRatingsWrites: Writes[ApiRatings] = Json.writes[ApiRatings]
  implicit val apiFilmWrites: Writes[ApiFilm] = Json.writes[ApiFilm]

  def from(fs: FilmSchedule): ApiFilm = {
    val resolved = fs.resolved
    val cinemaUrlMap = fs.cinemaFilmUrls.map { case (c, url) => c.displayName -> url }.toMap
    ApiFilm(
      title            = fs.movie.title,
      // The film's canonical path segment on the web (`/{city}/movie/{slug}`).
      // Served rather than derived client-side: the fold handles Polish and
      // German diacritics, ß, and Cyrillic, and a Swift copy plus a Kotlin copy
      // would be two more places for it to drift from `tools.Slugify`.
      slug             = fs.slug.getOrElse(""),
      posterURL        = fs.posterUrl,
      fallbackPosterURLs = resolved.fallbackPosterUrls,
      runtimeMinutes   = fs.movie.runtimeMinutes,
      releaseYear      = fs.movie.releaseYear,
      genres           = fs.movie.genres,
      ageRating        = resolved.ageRating,
      ratings          = ApiRatings(
        imdb              = resolved.ratings.imdb,
        imdbURL           = resolved.ratings.imdbUrl,
        metascore         = resolved.ratings.metascore,
        metacriticURL     = Some(resolved.ratings.metacriticUrl),
        rottenTomatoes    = resolved.ratings.rottenTomatoes,
        rottenTomatoesURL = Some(resolved.ratings.rottenTomatoesUrl),
        filmweb           = resolved.ratings.filmweb,
        filmwebURL        = Some(resolved.ratings.filmwebUrl)
      ),
      countries        = fs.movie.countries,
      directors        = fs.director,
      cast             = fs.cast,
      showings         = fs.showings.map { case (date, cinemas) =>
        ApiDayShowings(
          date    = date.toString,
          label   = CardFormat.date(date),
          cinemas = cinemas.map { cs =>
            ApiCinemaShowings(
              cinema    = cs.cinema.displayName,
              cinemaURL = cinemaUrlMap.get(cs.cinema.displayName),
              showtimes = cs.showtimes.map { st =>
                ApiShowtime(
                  time       = CardFormat.time(st.dateTime),
                  format     = st.format.mkString(" "),
                  room       = st.room,
                  bookingURL = st.bookingUrl
                )
              }
            )
          }
        )
      }
    )
  }
}

case class FilmSchedule(
                         movie: Movie,
                         posterUrl: Option[String],
                         synopsis: Option[String],
                         cast: Seq[String],
                         director: Seq[String],
                         cinemaFilmUrls: Seq[(Cinema, String)],
                         showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
                         // The fully-resolved metadata document this schedule was built from —
                         // ratings, poster fallbacks, original title, trailers. Replaces the
                         // old `Option[MovieRecord]`: the web no longer holds MovieRecords.
                         resolved: ResolvedMovie,
                         // This film's `/{city}/movie/{slug}` address, assigned over the whole
                         // corpus by `FilmSlugs` so two same-titled films get one each. `None`
                         // only for a title that folds to no usable slug — `FilmHref` answers
                         // those with the legacy query form. Carried on the schedule rather
                         // than re-derived per call site so the card link, the canonical
                         // og:url, the sitemap and the JSON-LD can't disagree.
                         slug: Option[String]
                       )

/**
 * Builds the per-city [[FilmSchedule]] view from the denormalised read model:
 * this city's [[CityScreening]] documents joined to their [[ResolvedMovie]]. The web
 * never touches the `movies` collection or a MovieRecord — the merge already
 * happened at projection time.
 */
class MovieControllerService(readModel: WebReadModel) extends Logging {

  def toSchedules(city: City): Seq[FilmSchedule] =
    toSchedules(city, LocalDateTime.now(city.zoneId))

  /** Overload with an injectable `now` so tests can pin the clock to a fixture's
   * capture date. Scoped to `city`: `readModel.screeningsForCity` already
   * returns only this city's cinemas' screenings, so a film playing only
   * elsewhere drops out here.
   *
   * Ordering-tolerant join: a screening document whose `ResolvedMovie` hasn't landed
   * yet (the movie-before-screenings write order can still be observed in the
   * reverse order over two independent change streams) simply contributes
   * nothing until the movie document arrives — no half-rendered card. */
  def toSchedules(city: City, now: LocalDateTime): Seq[FilmSchedule] = {
    readModel.screeningsForCity(city.slug).groupBy(_.filmId).toSeq.flatMap { case (filmId, screenings) =>
      readModel.movie(filmId).flatMap { resolved =>
        // Flatten this city's future showtimes. A film with no future showing in
        // this city drops out of its list view (its documents stay in the store).
        val allShowtimes: Seq[(Cinema, Showtime)] = screenings.flatMap { sc =>
          MovieControllerService.cinemaByName(sc.cinema).toSeq.flatMap { cinema =>
            sc.showtimes.iterator.filter(_.isUpcoming(now)).map(st => (cinema, st))
          }
        }
        if (allShowtimes.isEmpty) None
        else {
          val earliest = allShowtimes.map(_._2.dateTime).min
          val byDate: Seq[(LocalDate, Seq[CinemaShowtimes])] =
            allShowtimes
              .groupBy(_._2.dateTime.toLocalDate)
              .toSeq.sortBy(_._1)
              .map { case (date, slots) =>
                val perCinema = slots
                  .groupBy(_._1)
                  // `displayName` is the tiebreaker so two cinemas sharing a film at
                  // the same earliest showtime render in a stable order (the
                  // "Kino Malta vs Kino Meduza" snapshot-flake fix).
                  .toSeq.sortBy { case (cinema, ss) => (ss.map(_._2.dateTime).min, cinema.displayName) }
                  .map { case (cinema, ss) => CinemaShowtimes(cinema, ss.map(_._2).sortBy(_.dateTime)) }
                (date, perCinema)
              }
          val cinemaFilmUrls: Seq[(Cinema, String)] =
            screenings
              .flatMap(sc => MovieControllerService.cinemaByName(sc.cinema).flatMap(c => sc.filmUrl.map(c -> _)))
              .sortBy(_._1.displayName)
          Some((earliest, filmSchedule(resolved, cinemaFilmUrls, byDate, city)))
        }
      }
    }.sortBy { case (earliest, fs) => (earliest, fs.movie.title) }.map(_._2)
  }

  /** Assemble a [[FilmSchedule]] from a resolved movie + its (possibly empty)
   *  showings. Shared by the live `toSchedules` join and the deep-link
   *  resilience fallback below, so both materialise the schedule identically. */
  private def filmSchedule(resolved: ResolvedMovie,
                           cinemaFilmUrls: Seq[(Cinema, String)],
                           showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
                           city: City): FilmSchedule =
    FilmSchedule(
      movie = Movie(resolved.title, resolved.runtimeMinutes, resolved.releaseYear, countries = resolved.countries, genres = resolved.genres),
      posterUrl = resolved.posterUrl,
      synopsis = resolved.synopsisFor(city),
      cast = resolved.cast,
      director = resolved.directors,
      cinemaFilmUrls = cinemaFilmUrls,
      showings = showings,
      resolved = resolved,
      slug = readModel.filmSlugs.slugFor(resolved._id)
    )

  def film(city: City, title: String): Option[FilmSchedule] = {
    def lookup(t: String): Option[FilmSchedule] = {
      val needle = normalizeTitle(t)
      toSchedules(city).find(s => normalizeTitle(s.movie.title) == needle)
    }
    // Telegram (and some other chat apps) re-percent-encode a pasted URL whose
    // query already carries %XX escapes: our `%20` becomes `%2520`, `%C5%BC`
    // becomes `%25C5%25BC`. Play decodes that once, so `title` arrives with a
    // literal `%20` / `%C5%BC` still in it and the direct match misses. On a
    // miss, decode the residual escapes once more and retry.
    val decoded: Option[String] =
      Option(title)
        .filter(MovieControllerService.looksPercentEncoded)
        .map(t => URLDecoder.decode(t, StandardCharsets.UTF_8))
    lookup(title).orElse(decoded.flatMap(lookup))
      .orElse(knownMovieFallback(city, title, decoded))
  }

  /** Resolve the canonical `/{city}/movie/{slug}` address.
   *
   *  `FilmSlugs` assigned the address, so it is also what reverses it — one
   *  film per slug, whether or not another film shares its title. The re-slug
   *  scan behind it is the fallback for a slug the map doesn't know: a link
   *  minted before a re-key, or the sub-second window while the read model
   *  reloads. Re-slugging alone is what USED to resolve every address, and on a
   *  same-title pair it could only ever reach one of the two films — it stays
   *  as a safety net, not as the rule.
   *
   *  The fallback tie-breaks on the title rather than taking the head, because
   *  `toSchedules` orders by earliest showtime and that shifts through the day. */
  def filmBySlug(city: City, slug: String): Option[FilmSchedule] = {
    val addressed = readModel.filmSlugs.idFor(slug)
    def matches(id: String, title: String): Boolean =
      addressed.fold(tools.Slugify(title) == slug)(_ == id)

    toSchedules(city).filter(s => matches(s.resolved._id, s.movie.title)).minByOption(_.movie.title)
      .orElse {
        readModelFallback(
          city,
          readModel.allMovies().filter(m => matches(m._id, m.title)).minByOption(_.title),
          reference = s"slug='$slug'"
        )
      }
  }

  /** Resilience for film deep-links: a title the read model KNOWS but that has no
   *  live schedule in this city right now must not 404 a shared/bookmarked link.
   *  The common cause is a sub-second window while the worker re-projects or
   *  re-keys the film — its `web_movies` and `web_screenings` documents arrive
   *  over two independent change streams, so the `toSchedules` join momentarily
   *  drops it (see [[services.readmodel.ReadModelProjectionMetrics]] for the
   *  worker-side `films_pruned` / reprojection signal). Render the movie with an
   *  empty showings list instead; it self-heals on the next load once both
   *  documents land. A genuinely-ended run resolves the same way (better than a
   *  404 for an old link); a title the read model has never seen still returns
   *  None. Each hit is logged so the rate of "a link would have broken" is
   *  visible alongside the worker metrics. */
  private def knownMovieFallback(city: City, title: String, decoded: Option[String]): Option[FilmSchedule] = {
    def byTitle(t: String): Option[ResolvedMovie] = {
      val needle = normalizeTitle(t)
      readModel.allMovies().find(m => normalizeTitle(m.title) == needle)
    }
    readModelFallback(city, byTitle(title).orElse(decoded.flatMap(byTitle)), reference = s"title='$title'")
  }

  /** Shared tail of both deep-link resolvers (by title and by slug): render the
   *  read model's copy of the movie with no showings, and log that a link would
   *  otherwise have broken. `reference` names whichever key the caller looked up,
   *  so the log line stays actionable. */
  private def readModelFallback(city: City, resolved: Option[ResolvedMovie], reference: String): Option[FilmSchedule] =
    resolved.map { movie =>
      logger.warn(s"film deep-link served from the read model without a live ${city.slug} schedule " +
        s"(reprojection/rekey gap or ended run): $reference filmId=${movie._id}")
      filmSchedule(movie, cinemaFilmUrls = Seq.empty, showings = Seq.empty, city)
    }

  private def normalizeTitle(title: String): String = TitleNormalizer.normalize(title)
}

object MovieControllerService {
  /** displayName → Cinema (cinemas are `Source`s, so reuse the shared map). */
  private def cinemaByName(name: String): Option[Cinema] =
    Source.byDisplayName.get(name).collect { case c: Cinema => c }

  private val PercentEscape = "%[0-9A-Fa-f]{2}".r

  /** Does the string still contain an unresolved `%XX` escape? Used to spot a
   *  doubly-encoded title (see [[MovieControllerService.film]]) without
   *  touching the normal, already-decoded path. */
  private def looksPercentEncoded(s: String): Boolean =
    PercentEscape.findFirstIn(s).isDefined
}

class MovieController( cc: ControllerComponents,
                       movieControllerService: MovieControllerService,
                       readModel: WebReadModel,
                       // NO `UserRepository` — deliberately, and it is the strongest
                       // form of the promise `CachePolicy` needs. This
                       // controller renders every page it serves without the means to
                       // find out who asked for it, so no response of its can vary by
                       // session, so a shared cache holding the listing cannot leak
                       // anybody.
                       // Who is signed in is `AuthController`'s question, answered
                       // per client at `/api/me`.
                       oauthProviders: Set[String],
                       environment: Mode,
                       responseCache: EncodedResponseCache,
                       ogCardService: tools.OgCardService,
                       cityOgCardService: tools.CityOgCardService,
                       // The ONE country this deployment serves — which cities are
                       // ours (`withCity`) and which the sitemap advertises. Injected
                       // rather than read from `Country.fromEnv` at each use so a spec
                       // can exercise a non-Polish host by passing one, instead of
                       // mutating the process-global env that parallel suites share.
                       servingCountry: models.Country = models.Country.fromEnv,
                     )(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) with Logging {

  // The country this deployment serves — the rules its corpus was keyed under,
  // so the city OG card folds titles the way the worker keyed them.
  private val normalizer: TitleNormalizer = TitleNormalizer.forCountry(servingCountry)

  /** Does this client take gzip — the ONE compressed form the origin offers.
   *
   *  ⚠️ NOT BROTLI, THOUGH IT WOULD BE A THIRD SMALLER. It was built here
   *  (eb90279cd) and taken off the wire (cedfaa4f6, 6ef854b81), then deleted:
   *  Cloudflare caches one variant per URL and does not forward the client's
   *  real `Accept-Encoding` even on a `BYPASS` response. It asked the origin with
   *  its own header, got br, and — barred by `no-transform` from re-compressing —
   *  DECOMPRESSED for gzip-only clients: 3,789,572 B on `/uk/manchester/` where
   *  297,089 was right, and 1,470,154 on `/api/repertoire`, the Android app's own
   *  fetch. Offering a second coding needs an edge that keys its cache on the
   *  encoding, which this zone cannot express. */
  private def acceptsGzip(request: RequestHeader): Boolean =
    AcceptEncoding.acceptsGzip(request.headers.get("Accept-Encoding"))

  // The plain HTML pages (`/{city}/`, `/{city}/movies`) are byte-identical for
  // EVERY visitor at a given cache version — signed in or not, which is the whole
  // point of `_authMenu` no longer knowing — so we serve a pre-rendered,
  // pre-gzipped blob keyed on the request path (which fully determines the
  // output: city and page type).
  //
  // THE PREDICATE USED TO ASK `user.isEmpty`, AND THAT WAS THE CEILING. It meant
  // a signed-in visitor rendered fresh and uncacheably, but far more expensively
  // it meant the response could never be offered to a shared cache at all: the
  // page differed per visitor, so Cloudflare had to be told `private, no-cache`
  // and the edge held nothing but the JSON. Nobody's name reaches this render any
  // more, so the only things left that change the bytes are the ones below.
  //
  // Filter queries are the only thing left that bypasses it: they move the OG
  // meta, and `request.path` — the blob's key — drops the query string. That
  // costs them the blob, not the conditional GET: `renderIndex` still runs them
  // through `conditionalCompressed` keyed on the query, for the validators. A client
  // that cannot take gzip no longer bypasses it either, because it never needed
  // to: `conditionalCompressed` serves that client the uncompressed body with the
  // same validators, and the `Vary: Accept-Encoding` both branches carry is what
  // keeps the two spellings apart in a shared cache. `/api/repertoire` has been
  // shared-cacheable on exactly those terms since it was first offered to the edge.
  private def cacheablePlainPage(request: RequestHeader): Boolean =
    request.queryString.isEmpty

  private def ifModifiedSinceCurrent(request: RequestHeader, lastMod: java.time.Instant): Boolean =
    request.headers.get("If-Modified-Since").exists { ims =>
      scala.util.Try(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.parse(ims))
        .map(java.time.Instant.from)
        .toOption
        .exists(!lastMod.isAfter(_))
    }

  /** Conditional-GET + compressed-response cache for a response that is
   *  byte-identical for every client at the current [[MovieCache]] version.
   *
   *  A client whose `If-None-Match` offers this response's validator, or whose
   *  `If-Modified-Since` is still current, gets a bodiless 304 — so a browser
   *  refresh revalidates cheaply and reuses its cached copy instead of
   *  re-downloading the body.
   *
   *  Otherwise the body is served gzipped ([[AcceptEncoding.acceptsGzip]]) from
   *  the versioned [[EncodedResponseCache]]. Naming the `Content-Encoding`
   *  ourselves is also what keeps Play's `GzipFilter` off the response: it skips
   *  anything that already declares one. A `cacheBody = false` caller, or a
   *  client that refuses gzip, gets the body uncompressed and the filter handles
   *  it instead.
   *
   *  `policy` decides what a cache may do with the result — see [[CachePolicy]] for
   *  the two, and for why every one of them carries `no-transform`. */
  private def conditionalCompressed(request: RequestHeader, contentType: String, vary: String,
                                 policy: CachePolicy, cacheKey: String = "",
                                 city: Option[City] = None,
                                 cacheBody: Boolean = true)(body: => String): Result = {
    // THE VALIDATOR IS PER CITY, not model-wide. `readModel.lastModified` moves
    // when anything anywhere changes, so validating London's payload with it
    // meant a Warsaw showtime expired London's ETag: every city looked like it
    // changed every couple of minutes, and the client 304s and the edge cache
    // both lost most of their value. `lastModifiedFor` moves only when the bytes
    // THIS city renders can have changed -- including the corpus-wide film-address
    // reshuffles that genuinely do reach every city. `None` means a payload that
    // really is model-wide.
    //
    // AND THE CITY'S CALENDAR DAY FLOORS IT. Every city-scoped payload here is
    // anchored on `LocalDate.now(city.zoneId)`: the listing renders that day's
    // `data-next-day` (the midnight the document retires itself at) and the
    // expiry stamps the client prunes forward from, and `/api/repertoire` cuts
    // its window from the same date. The read-model stamp knows nothing about
    // that -- so a quiet night would answer a request made AFTER midnight with a
    // 304 for a body rendered before it, handing back a document that has
    // already told itself to reload and reloads into the same 304. Flooring at
    // the day's start retires every held copy at the boundary the payload
    // itself names, and stays monotonic because both inputs only ever advance.
    val lastMod  = MovieController.dayFlooredValidator(
      city.fold(readModel.lastModified)(c => readModel.lastModifiedFor(c.slug)),
      city.map(_.zoneId))
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    val httpDate = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
      .format(lastMod.atOffset(java.time.ZoneOffset.UTC))
    // `no-transform` IS WHAT LETS THE ETAG BELOW REACH ANYONE. Cloudflare deletes
    // the ETag from every `text/html` response these two zones serve -- measured
    // 2026-09-06 on an UNCACHED (`cf-cache-status: BYPASS`) page, so it is not a
    // stale stored copy, and on BOTH domains; a weak tag was stripped exactly as a
    // strong one was, while the JSON built by this same method came through
    // untouched. The transform it is reserving the right to make is visible in the
    // response: the origin sends `content-encoding: gzip` and the edge hands the
    // client `content-encoding: br`, having recompressed the body. Bytes it rewrites
    // are bytes no validator of ours can describe, so it drops ours rather than
    // forward a lie. `no-transform` withdraws the permission (it also covers Email
    // Obfuscation, Rocket Loader and Mirage -- none of which have anything to do
    // here: the listing carries no email address and the edge body contains no
    // `cdn-cgi` marker).
    //
    // WHAT IT COST. Giving up the edge's brotli was worth it on its own: Cloudflare
    // answers a conditional ONLY from an ETag, so with none to send, a refreshing
    // browser was handed the entire body under a 200, and a revalidation went from
    // 287,531 bytes to a bodiless 304. But the full fetch did grow, 228,940 ->
    // 287,531, +26% -- and building the brotli here instead could not get it back;
    // see `acceptsGzip`.
    val cacheControl: Seq[(String, String)] = policy match {
      case CachePolicy.BrowserOnly         => Seq("Cache-Control" -> "private, no-cache, no-transform")
      case CachePolicy.RevalidatedAnywhere => Seq("Cache-Control" -> "public, max-age=0, must-revalidate, no-transform")
    }
    // ⚠️ THE KEY MUST CARRY EVERY INPUT THAT CHANGES THE BODY, and `request.path`
    // does not.
    //
    // It drops the query string: `?days=7` and the full payload are the same
    // path, so keying on it alone would serve one client's window to another --
    // silently, with a 200 and a plausible body. `cacheKey` is the normalised,
    // parsed parameter rather than the raw query wherever a blob is kept, so a
    // crawler appending `?foo=1` cannot mint unbounded entries.
    //
    // It also drops the HOST, and one deployment serves two of them -- a
    // country's own domain and the shared brand apex (`Country.servesApex`).
    // `og:url`, `<link rel=canonical>` and the JSON-LD are all built from
    // `PageMeta.origin`, so a blob rendered for the first host was handed to
    // the second advertising the wrong canonical URL for the page. A SHARED
    // cache keys on host itself and never saw this; it was ours getting it
    // wrong.
    val bodyKey = PageMeta.host(request) + request.path + cacheKey

    // AN ETAG AS WELL AS Last-Modified, BECAUSE A SHARED CACHE NEEDS ONE.
    //
    // Measured against the live edge on 2026-09-05: once Cloudflare holds a copy,
    // an `If-Modified-Since` against it comes back 200 WITH THE WHOLE BODY --
    // Cloudflare answers a conditional from cache off the ETag, and these
    // responses had none. So letting the edge hold them traded the mobile apps'
    // 0-byte 304s for ~750 KB payloads: better for the origin, worse for the
    // phone. (The TTL that first exposed this is gone; the ETag it forced is
    // what makes revalidation work at all.)
    // `/api/catalog` never had the problem precisely because it carries one.
    //
    // Derived from the read-model version and `bodyKey` rather than hashing the
    // body: the body is the expensive thing here (it is why the gzip cache
    // exists) and the version already changes exactly when the body does. ONE
    // key answers both "which body is this" questions -- which blob to reuse,
    // and which validator to stamp -- so the two cannot disagree about what
    // counts as a different page. They used to: the blob learned about the host
    // (below) and the ETag did not, leaving both hosts' pages sharing a
    // validator that named only the path.
    //
    // ⚠️ WEAK (`W/`), AND IT HAS TO BE — Cloudflare strips a STRONG ETag off
    // anything it serves as HTML. Measured 2026-09-06 on the same URL with the
    // same `Accept-Encoding: gzip`: straight at the k3s node with Cloudflare
    // bypassed, `/uk/manchester/` carried `etag: "7ea9812c-6a9cfc88"`; through
    // the edge it carried none, while `/uk/manchester/api/repertoire` — this
    // same line — kept its ETag both ways. The two bodies were byte-identical
    // (3826089 bytes), so nothing had in fact been rewritten: the zone simply
    // has an HTML-transforming feature on, and a strong validator is a promise
    // about BYTES that Cloudflare will not forward when it reserves the right
    // to change them. That is the whole reason fa7f5dd5a's ETag was not
    // reaching the edge it was added for.
    //
    // `W/` is not a concession made to get past that — it is what this
    // validator always was. It is `bodyKey.hashCode` + the read-model stamp, a
    // CONTENT VERSION rather than a hash of the body, and since b84004f84 the
    // filtered branch keeps no blob, so two responses sharing one validator can
    // legitimately differ in which showtimes have already started. The same tag
    // also goes on BOTH representations below -- gzip and identity -- which a
    // strong validator is flatly not permitted to do, since a strong tag promises
    // byte-equality and the two share no bytes at all. Weak says all of that out
    // loud.
    val etag = "W/\"" + Integer.toHexString(bodyKey.hashCode) + "-" + lastMod.getEpochSecond.toHexString + "\""
    val validators: Seq[(String, String)] = ("Last-Modified" -> httpDate) +: ("ETag" -> etag) +: cacheControl

    if (MovieController.offersValidator(request.headers.get("If-None-Match"), etag)
        || ifModifiedSinceCurrent(request, lastMod))
      // ⚠️ `Vary` ON THE 304 TOO, not just on the 200s below. RFC 9110 §15.4.5 makes
      // it a MUST, not a nicety -- a 304 "MUST generate any of the following header
      // fields that would have been sent in a 200 to the same request:
      // Content-Location, Date, ETag, and Vary". And this response needs it more
      // than most: ONE weak validator covers BOTH representations of the page --
      // gzip and identity -- which is legitimate precisely because it is weak, but
      // it means a cache that stores this 304's headers without being told the
      // response varies by `Accept-Encoding` can hand a gzip body to a client
      // that refused it.
      //
      // Measured on the live origin before this line existed: a 304 came back
      // `vary: Origin`, while the 200 beside it said `vary: Accept-Encoding,Origin`.
      // Cloudflare adds the missing token on the way out, so the edge looked
      // correct and the origin was not -- and nothing else in front of us would.
      NotModified.withHeaders((("Vary" -> vary) +: validators)*)
    else if (cacheBody && acceptsGzip(request)) {
      // Gzipped HERE, and stamped `Content-Encoding` HERE, which is also what
      // keeps Play's GzipFilter off it: the filter skips any response that already
      // names an encoding. The uncached branch below deliberately names none, and
      // the filter gzips it on the way out.
      val bytes = responseCache.gzippedBody(bodyKey, lastMod)(body)
      Ok(bytes).as(contentType)
        .withHeaders((Seq("Content-Encoding" -> "gzip", "Vary" -> vary) ++ validators)*)
    } else
      // An uncached response, or a client that refuses gzip: leave it uncompressed
      // and let the GzipFilter handle it, which is what keeps a filter variant
      // from minting a blob.
      Ok(body).as(contentType).withHeaders((("Vary" -> vary) +: validators)*)
  }

  private val HtmlContentType = "text/html; charset=utf-8"
  private val HtmlVary        = "Accept-Encoding"

  // Every city-scoped handler wraps its body in this so resolution + not-found
  // behaviour lives in one place — see `ServedCity` for the country scope.
  private def withCity(slug: String)(f: City => Result): Result = ServedCity.resolve(slug, servingCountry)(f)

  // Persist the viewed city so the bare `/` landing can bounce a returning
  // visitor straight to it. Readable by JS (httpOnly = false) so the client can
  // also honour it; long-lived; scoped to the deployment's MOUNT POINT so it
  // rides every request of this country's site and none of a neighbour's — on
  // the shared brand domain a cookie at "/" would be sent to (and overwritten
  // by) `/de` and `/us`, bouncing a UK visitor's landing to a city that country
  // does not serve.
  private def cityCookie(city: City): Cookie =
    Cookie("city", city.slug, maxAge = Some(60 * 60 * 24 * 365), path = city.country.mountPath, httpOnly = false)

  /** The main "Filmy" listing — repertoire view, full corpus, OG meta derived
   *  from `?…` filter parameters. Shared between `/` and `/movies` (no
   *  parameters) so both URLs are interchangeable; `/movies` with one of the
   *  browse-axis parameters still routes through `browse` below to the
   *  per-director / per-cast / per-country page.
   *
   *  RENDERED FOR NOBODY — AND THEREFORE OFFERED TO THE EDGE. This handler
   *  cannot ask who is making the request, and that is the safety argument
   *  rather than a side effect of it: the controller holds no `UserRepository`,
   *  `views.html.repertoire` has no parameter to take a `models.User`, and
   *  `_authMenu` has nothing to draw. So the bytes cannot depend on the session
   *  cookie, and a shared cache cannot hand one visitor's page to another — the rule
   *  `CachePolicy.RevalidatedAnywhere` states, met structurally rather than by
   *  inspection.
   *  Whoever is signed in is layered on after first paint, by `shared.js` off
   *  `/api/me` (which is `no-store`, so it cannot be shared either).
   *
   *  `?filter=` variants keep `private, no-cache`: still client-independent, but
   *  combinatorially many and not worth an edge entry each. They do carry the
   *  same validators, so revalidating one comes back 304 rather than re-sending
   *  the listing — what they skip is the shared blob, not the conditional GET. */
  private def renderIndex(city: City, request: RequestHeader): Result = {
    implicit val c: City = city
    if (cacheablePlainPage(request)) {
      // 304 short-circuits before any work; on a 200 cache hit `renderIndexHtml`
      // (and its data-prep) never runs either.
      // NO `Set-Cookie` HERE, AND THAT IS THE WHOLE POINT OF THE BRANCH.
      // Cloudflare bypasses any response carrying one: measured, this page went
      // `DYNAMIC` -> `BYPASS` the moment a Cache Rule made it eligible, with the
      // `city=` cookie the only thing left on it. The cookie exists so the bare
      // `/` landing can bounce a returning visitor to their city; it is
      // `httpOnly = false` precisely so the client owns it, and `shared.js`
      // writes it on load with the same name, path and lifetime. The filtered
      // branch below is `private, no-cache`, so it keeps setting it server-side
      // and a visitor with no JS is still remembered.
      conditionalCompressed(request, HtmlContentType, HtmlVary, CachePolicy.RevalidatedAnywhere,
                         city = Some(city))(renderIndexHtml(city, request).body)
    } else {
      // A FILTER VARIANT STILL GETS VALIDATORS, JUST NOT A BLOB.
      //
      // `private, no-cache` tells the browser to store the page and re-validate
      // before every re-use -- but a re-validation with nothing to validate
      // AGAINST cannot come back 304, so every refresh of a shared
      // `?date=tomorrow` link re-downloaded the whole listing (265 KB gzipped,
      // 3.8 MB of HTML for Manchester). An ETag makes that refresh free when the
      // city has not moved, without promising any cache it may serve the copy
      // unasked.
      //
      // `cacheBody = false` is the other half of the decision above: these
      // variants stay out of the shared gzip cache, which is an LRU over BYTES,
      // so one-off filter combinations cannot evict the bare city pages that
      // earn their place there. The GzipFilter still compresses on the way out.
      //
      // The whole query string is the key, not a normalised subset of it: the
      // page puts the request's own URL in `og:url`, so two spellings that mean
      // the same thing really do render different bytes and must not share a
      // validator. Nothing is stored per key, so an appended `?foo=1` costs a
      // hash and nothing else.
      //
      // WHAT THE ETAG PROMISES, PRECISELY: the read-model version this page was
      // cut from, not a hash of the bytes. With no blob pinning them, a body
      // re-rendered a few minutes later differs in the showtimes that have since
      // started -- under the same validator, so a client holding the earlier
      // copy keeps it until the city's stamp moves or its midnight arrives. That
      // is the same age the branch above serves from its blob between two stamps,
      // and the page is built for it: `data-expires` prunes the lapsed showtimes
      // client-side and `data-next-day` retires the document at midnight. What
      // it costs that the blob branch does not is byte-identity between two
      // clients holding one validator, which only a SHARED cache could observe
      // -- and `private, no-cache` is exactly the instruction that none may.
      conditionalCompressed(request, HtmlContentType, HtmlVary, CachePolicy.BrowserOnly,
                         cacheKey = "|q=" + request.rawQueryString, city = Some(city),
                         cacheBody = false)(renderIndexHtml(city, request).body)
        .withCookies(cityCookie(city))
    }
  }

  private def renderIndexHtml(city: City, request: RequestHeader)(implicit c: City): play.twirl.api.Html = {
    // One clock for both the filtering and the page's own expiry countdown —
    // `_repertoireView` counts forward from `renderedAt`, so it has to be the
    // instant the schedules were actually pruned at.
    val now       = LocalDateTime.now(city.zoneId)
    val schedules = movieControllerService.toSchedules(city, now)
    val meta      = FilterDescription.forIndex(city, request.queryString, schedules)
    views.html.repertoire(
      schedules,
      city.cinemaDisplayNames,
      city.cinemaPillMap,
      devMode, oauthProviders, renderedAt = now,
      pageTitle       = meta.title,
      pageDescription = meta.description,
      pageUrl         = PageMeta.canonicalUrl(request),
      fbAppId         = PageMeta.fbAppId,
      // og:url keeps the filtered request URL (so a shared filtered link
      // previews the filter), but the canonical folds `/{city}/movies` and every
      // `?filter` variation back to the bare listing.
      canonicalUrl    = PageMeta.origin(request) + CityPath(city) + "/",
    )
  }

  def index(city: String): Action[AnyContent] = Action { request => withCity(city)(renderIndex(_, request)) }

  private def renderBrowse(city: City, heading: String, films: Seq[FilmSchedule], request: RequestHeader): Result = {
    implicit val c: City = city
    // Client-independent like the listing (nobody is rendered into it), but a
    // facet URL is one of combinatorially many and earns no edge entry.
    Ok(views.html.browse(
      films, heading, devMode, oauthProviders,
      pageUrl = PageMeta.canonicalUrl(request),
      fbAppId = PageMeta.fbAppId,
      // A FACET IS UI STATE, NOT A PAGE. `?cast=` alone is one URL per cast
      // member per city, so the set of these is combinatorial rather than
      // merely large, and every one of them is a near-duplicate of the city
      // listing built from the same films. og:url keeps the filtered URL (a
      // shared filtered link should preview its filter), the canonical folds
      // them back onto the listing, and `noindex,follow` keeps the crawler
      // walking through to the film pages, which ARE the content.
      //
      // robots.txt already disallows this path; this is the second line, for
      // the crawlers that ignore it — see the note on `_ogTagsApp`.
      canonicalUrl = PageMeta.origin(request) + CityPath(city) + "/",
      robots = MovieController.FacetRobots,
    )).withHeaders("Cache-Control" -> "private, no-cache").withCookies(cityCookie(city))
  }

  /** The four legacy Polish param names (`kraj`/`rezyser`/`aktor`/`gatunek`) are still
   *  bound and still filter. They were the only spelling until the facets were renamed to
   *  English for the shared route table, so every link minted before that — a bookmark, a
   *  shared URL, anything already crawled — carries them. Dropping the binding did not 404
   *  those; it fell through to the no-axis case and rendered the UNFILTERED city listing,
   *  a 200 with the wrong content, which is the failure mode nobody reports. The English
   *  name wins when both are present. */
  def browse(city: String, country: Option[String], director: Option[String], cast: Option[String], genre: Option[String],
             kraj: Option[String] = None, rezyser: Option[String] = None,
             aktor: Option[String] = None, gatunek: Option[String] = None): Action[AnyContent] = Action { request =>
    withCity(city) { c =>
      val all = movieControllerService.toSchedules(c)
      (country.orElse(kraj), director.orElse(rezyser), cast.orElse(aktor), genre.orElse(gatunek)) match {
        case (Some(name), _, _, _) => renderBrowse(c, name, all.filter(_.movie.countries.contains(name)), request)
        case (_, Some(name), _, _) => renderBrowse(c, name, all.filter(_.director.contains(name)),        request)
        case (_, _, Some(name), _) => renderBrowse(c, name, all.filter(_.cast.contains(name)),            request)
        case (_, _, _, Some(name)) => renderBrowse(c, name, all.filter(_.movie.genres.contains(name)),    request)
        // `/{city}/movies` with no filter axis is the main listing — the same
        // view as `/{city}/`. The browse view only kicks in for the per-axis
        // pages reached from the meta-link rows on /movie.
        case _                     => renderIndex(c, request)
      }
    }
  }

  // robots.txt — see `RobotsTxt` for what goes in it and why. The one decision
  // that lives here is WHICH of its two shapes this request wants: the brand
  // front door speaks for every country mounted under the apex, a country's own
  // site only for itself. The `/*/og-image` + `/*/movie/og-image` PNG endpoints
  // are deliberately NOT disallowed — Facebook honours robots.txt when fetching
  // `og:image`, so blocking them would break every share preview.
  def robotsTxt: Action[AnyContent] = Action { request =>
    val body =
      if (servingCountry.servesApex(PageMeta.host(request))) RobotsTxt.frontDoor(mountedUnderApex)
      else RobotsTxt.forCountry(PageMeta.origin(request) + servingCountry.pathPrefix, servingCountry)
    Ok(body).as("text/plain; charset=utf-8")
  }

  /** The countries that share the brand domain, and so the ones the front door's
   *  `robots.txt` and `sitemap.xml` have to speak for: a crawler reads both only
   *  at a host's ROOT, which none of them owns. Poland is excluded by having no
   *  path prefix — it is a different host with a root of its own. */
  private def mountedUnderApex: Seq[models.Country] =
    models.Country.switchable.filter(_.pathPrefix.nonEmpty)

  /** `sitemap.xml` — the full crawl map: landing, every city listing + plan, and
   *  every film each city is currently showing. Built from the warm read model
   *  (`toSchedules` per city is a cheap in-memory join), so it always reflects
   *  what's actually live. Cached for an hour at the edge/browser; the corpus
   *  changes on the order of scrape cadence, not per request. */
  def sitemap: Action[AnyContent] = Action { request =>
    // Scope to THIS deployment's country — a `KINOWO_COUNTRY=pl` (Poland) host must
    // not advertise the UK/Germany cities that also live in the global `City.all`
    // (those pages render empty on this host, so crawling them is pure waste). Each
    // country's own deployment sitemaps its own cities. Same scope the landing +
    // navbar use (`Country.fromEnv`).
    val body =
      if (servingCountry.servesApex(PageMeta.host(request))) SitemapBuilder.index(mountedUnderApex)
      else {
        val entries = servingCountry.cities.map(c => c -> movieControllerService.toSchedules(c))
        val lastmod = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
          .format(readModel.lastModified.atOffset(java.time.ZoneOffset.UTC))
        // The bare ORIGIN plus the country: every `<loc>` picks the mount point
        // up from the city (or, for the landing, from the country) via the same
        // builders the pages themselves use, so a country sharing the brand
        // domain neither drops the prefix nor doubles it.
        SitemapBuilder.build(PageMeta.origin(request), servingCountry, entries, lastmod = Some(lastmod))
      }
    Ok(body).as("application/xml; charset=utf-8")
      .withHeaders("Cache-Control" -> "public, max-age=3600")
  }

  /** Conditional-GET wrapper for the JSON API endpoints — the same mechanism as
   *  the HTML pages (see [[conditionalCompressed]]): a current `If-Modified-Since`
   *  yields a bodiless 304 (what warm mobile clients hit), otherwise the payload
   *  is served from the shared gzip cache. The endpoints don't set
   *  `Cache-Control` (mobile manages its own revalidation), so `revalidate` is
   *  off. Both the listing and the details payload track the same city's cache
   *  mtime, so a 304 on one is a 304 on the other. */
  private def conditionalJson(request: Request[AnyContent], city: City, cacheKey: String = "")(body: => play.api.libs.json.JsValue): Result =
    conditionalCompressed(request, "application/json", vary = "Accept-Encoding",
                       CachePolicy.RevalidatedAnywhere, cacheKey = cacheKey, city = Some(city))(
      play.api.libs.json.Json.stringify(body)
    )

  /** Lean listing — everything the grid + filters need, no heavy detail text.
   *  Latency-sensitive; clients hit this on the critical path. */
  def apiRepertoire(city: String, days: Option[Int] = None): Action[AnyContent] = Action { request =>
    withCity(city) { c =>
      val window = MovieController.dayWindow(days)
      conditionalJson(request, c, cacheKey = MovieController.windowCacheKey(window)) {
        val today     = java.time.LocalDate.now(c.zoneId)
        val schedules = movieControllerService.toSchedules(c)
        Json.toJson(MovieController.withinWindow(schedules, today, window).map(ApiFilm.from))
      }
    }
  }

  /** Detail-only payload (synopsis + trailers), keyed by title. Clients fetch
   *  this in parallel with the listing and merge; keeping it off
   *  `/{city}/api/repertoire` halves the listing's gzip size. */
  def apiDetails(city: String): Action[AnyContent] = Action { request =>
    withCity(city) { c =>
      conditionalJson(request, c) {
        val details = movieControllerService.toSchedules(c)
          .map(ApiFilmDetails.from)
          .filter(ApiFilmDetails.hasContent)
        Json.toJson(details)
      }
    }
  }

  /** The city's cinema universe + area grouping (static). Mobile fetches this
   *  once per city to render the collapsible, per-area cinema filter — the
   *  counterpart of the server-side `CINEMA_AREAS` the web page is handed. */
  def apiCinemas(city: String): Action[AnyContent] = Action { request =>
    withCity(city)(c => conditionalJson(request, c)(Json.toJson(ApiCityCinemas.from(c))))
  }

  /** `/{city}/film…` and `/{city}/filmy` — the pre-rename Polish spellings of
   *  the detail page and the browse facets, 301'd onto `/{city}/movie…` and
   *  `/{city}/movies`. Kept routable indefinitely for the same reason the
   *  `?title=` form is: search indexes, shared links and installed app builds
   *  all still carry the old address.
   *
   *  The sub-path form takes the whole remainder rather than a `:slug` because
   *  the rename moved `/film/og-image` too, and one wildcard covers both. */
  def filmLegacy(city: String): Action[AnyContent] = Action { request =>
    movedToRenamedPath(city, "movie", request)
  }

  def filmSubPathLegacy(city: String, rest: String): Action[AnyContent] = Action { request =>
    movedToRenamedPath(city, s"movie/$rest", request)
  }

  def browseLegacy(city: String): Action[AnyContent] = Action { request =>
    movedToRenamedPath(city, "movies", request)
  }

  /** 301 onto `/{prefix}/{city}/{tail}`, query string intact.
   *
   *  Resolved through `withCity` so the mount prefix comes off the CITY, the
   *  same way every other URL builder here gets it — Play strips
   *  `play.http.context` before matching, so a redirect assembled from the
   *  route's own `:city` alone would drop the `/uk` and land off-site. It also
   *  means an unknown city 404s here rather than being bounced onto a URL that
   *  404s one hop later.
   *
   *  The query string rides along verbatim: `?title=`, the browse facets
   *  (including the legacy Polish `kraj`/`rezyser`/… spellings) and the shared
   *  filter links all live there, and dropping them would answer 200 with the
   *  wrong content — the failure mode nobody reports. */
  private def movedToRenamedPath(city: String, tail: String, request: RequestHeader): Result =
    withCity(city) { c =>
      val path = s"${c.country.pathPrefix}/${c.slug}/$tail"
      MovedPermanently(if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}")
    }

  /** The canonical film page, addressed by slug. */
  def filmBySlug(city: String, slug: String): Action[AnyContent] = Action { request =>
    withCity(city) { implicit c =>
      movieControllerService.filmBySlug(c, slug) match {
        case Some(schedule) => renderFilm(schedule, request)
        case None           => NotFound(s"Film not found: $slug")
      }
    }
  }

  /** The pre-slug `?title=…` address. Kept routable indefinitely — it is what
   *  every link minted before the switch carries, including the ~10k URLs the
   *  old sitemap put in search indexes and the share links installed app builds
   *  still generate — but answered with a 301 so crawlers consolidate on the
   *  slug and users land on the canonical address. */
  def film(city: String, title: String): Action[AnyContent] = Action { request =>
    withCity(city) { implicit c =>
      movieControllerService.film(c, title) match {
        // A title with no usable slug has no other address to offer, so it
        // renders here rather than 301-ing to itself.
        case Some(schedule) if schedule.slug.isDefined =>
          MovedPermanently(FilmHref.forSlug(schedule.slug, schedule.movie.title))
        case Some(schedule) => renderFilm(schedule, request)
        case None           => NotFound(s"Film not found: $title")
      }
    }
  }

  private def renderFilm(schedule: FilmSchedule, request: Request[AnyContent])(implicit c: City): Result = {
    // `request.uri` would carry the raw inbound encoding; use the canonical
    // FilmHref form instead so the og:url matches the link the page exposes
    // elsewhere. Scheme/host come from PageMeta so the X-Forwarded-* workaround
    // (Play 3.0's `request.secure` ignores the `trustedProxies` knob on this Fly
    // setup) is in one place.
    val canonicalUrl = PageMeta.origin(request) + FilmHref.forSlug(schedule.slug, schedule.movie.title)
    val ogImageUrl   = PageMeta.origin(request) + FilmHref.ogImage(schedule.movie.title)
    // Nobody is rendered into this page either, so `no-cache` (revalidate, keep
    // the browser copy, bfcache works) replaces the `no-store` a signed-in render
    // used to need. It stops short of offering itself to a shared cache only
    // because a per-film edge entry wants its own validator analysis, not because
    // the bytes are anyone's.
    Ok(views.html.film(schedule, canonicalUrl, OgCardAssembly.previewDescription(schedule), ogImageUrl, devMode, oauthProviders))
      .withHeaders("Cache-Control" -> "private, no-cache")
      .withCookies(cityCookie(c))
  }

  /** The 1200×630 Open Graph share card (PNG) for a film — what `og:image` /
   *  `twitter:image` on the film page point at. Composited server-side
   *  ([[tools.OgCardService]]) so the full poster + title + rating badges sit
   *  inside one landscape image that the preview UIs can't crop the poster out
   *  of. Cached a day at the edge (the card only changes when ratings / poster
   *  do, and `OgCardService` memoises the bytes per those inputs). */
  def ogImage(city: String, title: String): Action[AnyContent] = Action {
    withCity(city) { c =>
      movieControllerService.film(c, title) match {
        case Some(schedule) =>
          val bytes = ogCardService.card(
            schedule.movie.title,
            OgCardAssembly.cardSubtitle(schedule),
            OgCardAssembly.cardRatingBadges(schedule),
            // Primary poster first, then the cinema fallbacks: the primary is
            // often a Multikino origin whose Cloudflare 403s our datacentre
            // egress IP — Hetzner's since the move off Fly, and the block
            // followed us rather than being about any one provider — so the
            // card must be free to walk to a reachable fallback (see OgCardService).
            schedule.posterUrl.toSeq ++ schedule.resolved.fallbackPosterUrls,
            c.country.shareHost,
            director = OgCardAssembly.cardDirector(schedule),
            // The PNG card draws plain text — drop the markdown emphasis markers.
            synopsis = schedule.synopsis.map(tools.SynopsisMarkdown.strip)
          )
          Ok(bytes).as(tools.OgCardRenderer.MimeType).withHeaders("Cache-Control" -> "public, max-age=86400")
        case None => NotFound(s"Film not found: $title")
      }
    }
  }

  /** The 1200×630 per-city Open Graph card (PNG): a montage of the city's
   *  current posters under the [[FilterDescription.cityHeading]] overlay
   *  ("Repertuar kin w {locative}" / "Cinema listings in {city}"),
   *  composited server-side ([[tools.CityOgCardService]]) — fully dynamic, no
   *  committed image. NOT yet wired into the page's `og:image`; reachable
   *  directly at `/:city/og-image` for review. */
  def cityOgImage(city: String): Action[AnyContent] = Action {
    withCity(city) { c =>
      // A different (deduped, poster-bearing) set of the city's films each day —
      // and the cache key carries the date so the card regenerates daily.
      val day   = java.time.LocalDate.now(c.zoneId)
      val films = OgCardAssembly.dailyCardFilms(movieControllerService.toSchedules(c), day.toEpochDay, count = 5, normalizer)
        .map(OgCardAssembly.toCityCardFilm)
      val bytes = cityOgCardService.card(s"${c.slug}|$day", FilterDescription.cityHeading(c), c.country.brandName, c.country.shareHost, films, c.country.filmwebEnabled)
      // 1h, not a day: the card tracks the live repertoire (which shifts through
      // the day), and a shorter TTL means a regenerated card surfaces promptly.
      Ok(bytes).as(tools.OgCardRenderer.MimeType).withHeaders("Cache-Control" -> "public, max-age=3600")
    }
  }

  private def devOnly(result: => play.api.mvc.Result): play.api.mvc.Result = DevMode.gate(environment)(result)
  private def devMode: Boolean = DevMode.enabled(environment)
}

object MovieController {

  private val OfferedEntityTag = """(?:[Ww]/)?("[^"]*")""".r

  /** The opaque part of an entity tag — the quoted string, with any `W/` weakness
   *  marker dropped. `W/"abc"` and `"abc"` both reduce to `"abc"`. */
  private def opaqueTag(entityTag: String): String =
    entityTag.trim.stripPrefix("W/").stripPrefix("w/")

  /** Does the client's `If-None-Match` offer `etag`?
   *
   *  WEAK comparison, which is the one RFC 9110 §13.1.2 mandates for
   *  `If-None-Match` — two tags match when their opaque parts are equal, whether
   *  or not either carries the `W/` marker. Exact string equality was survivable
   *  only while we emitted one spelling of one tag and nothing in the path
   *  touched it; now that the validator IS weak, a cache is free to hand it back
   *  bare, and answering that with a 200 would re-send the whole ~750 KB listing
   *  to a client that already holds it.
   *
   *  The header is a LIST — a browser holding two variants of a URL offers both,
   *  comma-separated — so every tag in it is considered, not just a header that
   *  equals ours outright. Tags are matched by pattern rather than split on `,`
   *  because a comma is a legal character inside the quoted part; a tag we fail
   *  to parse simply does not match, which costs a body, never a wrong 304.
   *
   *  `*` matches any current representation, per the same section. */
  def offersValidator(ifNoneMatch: Option[String], etag: String): Boolean =
    ifNoneMatch.exists { header =>
      header.trim == "*" ||
        OfferedEntityTag.findAllMatchIn(header).exists(_.group(1) == opaqueTag(etag))
    }

  /** The validator instant for a payload, floored at the start of the day it was
   *  rendered for.
   *
   *  `modelStamp` is when the read model this payload draws on last moved. That
   *  is the whole story for a payload that says the same thing at any hour, and
   *  none of it for the ones here, which are cut against `LocalDate.now(zone)`:
   *  the listing carries that day's retire-at-midnight stamp, and the repertoire
   *  API cuts its window from that date. On a quiet night the model stamp can
   *  sit still across midnight, and a client revalidating at 00:05 would then be
   *  told 304 for a body belonging to the day before -- a document that has
   *  already scheduled its own reload, and that reloads straight back into the
   *  same 304.
   *
   *  So a zoned payload's validator is the LATER of the two. Both inputs only
   *  advance, so the result is monotonic, which is what a validator has to be:
   *  a repeated value must mean unchanged bytes. `None` is a payload with no
   *  day in it, which keeps the model stamp alone.
   */
  def dayFlooredValidator(modelStamp: java.time.Instant,
                          zone: Option[java.time.ZoneId],
                          now: java.time.Instant = java.time.Instant.now()): java.time.Instant =
    zone.map(z => now.atZone(z).toLocalDate.atStartOfDay(z).toInstant) match {
      case Some(dayStart) if dayStart.isAfter(modelStamp) => dayStart
      case _                                              => modelStamp
    }

  /** How many days from today a listing request wants, or `None` for everything.
   *
   *  Clamped rather than trusted: the parameter reaches us from a URL, and an
   *  unbounded one is a way to ask for arbitrary work. `<= 0` is meaningless, so
   *  it reads as "no window" only when the parameter is absent -- an explicit
   *  `days=0` is clamped to one day rather than silently returning everything,
   *  because an empty answer is easier to notice than a 700 KB one. */
  def dayWindow(days: Option[Int]): Option[Int] = days.map(n => math.max(1, math.min(n, MaxDayWindow)))

  /** The ceiling on `?days=`. The corpus reaches ~10 months ahead (London's last
   *  date was 2027-07-03 when this was written), so anything past a year is the
   *  whole payload by another name. */
  val MaxDayWindow: Int = 400

  /** Part of the gzip cache key, so two windows cannot share one entry. Spelled
   *  out rather than derived from the raw query so `?days=07` and `?days=7` land
   *  on the same entry instead of two identical ones. */
  def windowCacheKey(window: Option[Int]): String = window.fold("")(n => s"|days=$n")

  /** Films that have at least one showing inside the window, carrying only the
   *  showings inside it.
   *
   *  CALENDAR DAYS FROM TODAY, not "the first N dates that have showings": a film
   *  whose only screening is in December must not appear in `days=7` just because
   *  it happens to be the next date on its own list. A film left with nothing in
   *  the window is dropped entirely rather than emitted with an empty
   *  `showings` -- an empty film is a card the client would have to render and
   *  then hide. */
  def withinWindow(schedules: Seq[FilmSchedule], today: java.time.LocalDate,
                   window: Option[Int]): Seq[FilmSchedule] = window match {
    case None => schedules
    case Some(n) =>
      val limit = today.plusDays(n.toLong)
      schedules.flatMap { fs =>
        val kept = fs.showings.filter { case (date, _) => !date.isBefore(today) && date.isBefore(limit) }
        if (kept.isEmpty) None else Some(fs.copy(showings = kept))
      }
  }

  /** What the faceted browse pages tell a crawler about themselves.
   *
   *  `follow` and not `none`: the point is to keep the facet URLs out of an
   *  index, not to hide the film links they carry. Those links are the reason
   *  the page is worth crawling at all, and each one lands on a film page that
   *  IS indexable. */
  val FacetRobots = "noindex,follow"


}
