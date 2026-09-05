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
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

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
                       // Every collaborator the dev-only /debug pages read (corpus,
                       // staging, queue, cadence, read-model dump), keyed by country.
                       // In prod a single stack — this deployment's country; locally
                       // in Dev one per switchable country, so /debug can switch which
                       // country's db it shows via `?country=xx` same-origin instead of
                       // hopping to the other country's prod host (which 404s /debug).
                       debugCountries: DebugCountries,
                       // NO `UserRepository` — deliberately, and it is the strongest
                       // form of the promise `SharedMaxAgeSeconds` needs. This
                       // controller renders every page it serves without the means to
                       // find out who asked for it, so no response of its can vary by
                       // session and `s-maxage` on the listing cannot leak anybody.
                       // Who is signed in is `AuthController`'s question, answered
                       // per client at `/api/me`.
                       // Gate for the state-mutating /…/debug/rehydrate trigger
                       // (the other /debug pages are dev-only; rehydrate runs in
                       // every mode, so it needs the admin gate instead).
                       adminAction: AdminAction,
                       oauthProviders: Set[String],
                       environment: Mode,
                       responseCache: GzippedResponseCache,
                       ogCardService: tools.OgCardService,
                       cityOgCardService: tools.CityOgCardService,
                       // `cinema displayName -> public source-page URL`, the same
                       // links /uptime shows, sourced from the UptimeMonitor tag
                       // snapshot. Evaluated per request so it tracks live retags;
                       // used only by the /debug table to link cinema names.
                       cinemaSourceUrls: () => Map[String, String] = () => Map.empty,
                       // The ONE country this deployment serves — which cities are
                       // ours (`withCity`) and which the sitemap advertises. Injected
                       // rather than read from `Country.fromEnv` at each use so a spec
                       // can exercise a non-Polish host by passing one, instead of
                       // mutating the process-global env that parallel suites share.
                       servingCountry: models.Country = models.Country.fromEnv,
                     )(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) with Logging {

  // The country this deployment serves — the rules its corpus was keyed under,
  // so /debug orders staging rows by the same anchor the worker wrote.
  private val normalizer: TitleNormalizer = TitleNormalizer.forCountry(servingCountry)

  private def acceptsGzip(request: RequestHeader): Boolean =
    request.headers.get("Accept-Encoding").exists(_.toLowerCase.contains("gzip"))

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
  // meta, and `request.path` — the blob's key — drops the query string. A client
  // that cannot take gzip no longer bypasses it either, because it never needed
  // to: `conditionalGzipped` serves that client the uncompressed body with the
  // same validators, and the `Vary: Accept-Encoding` both branches carry is what
  // keeps the two spellings apart in a shared cache. `/api/repertoire` has been
  // shared-cacheable on exactly those terms since it got `s-maxage`.
  private def cacheablePlainPage(request: RequestHeader): Boolean =
    request.queryString.isEmpty

  private def ifModifiedSinceCurrent(request: RequestHeader, lastMod: java.time.Instant): Boolean =
    request.headers.get("If-Modified-Since").exists { ims =>
      scala.util.Try(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.parse(ims))
        .map(java.time.Instant.from)
        .toOption
        .exists(!lastMod.isAfter(_))
    }

  /** Conditional-GET + gzip-cache for a response that is byte-identical for
   *  every client at the current [[MovieCache]] version. A client whose
   *  `If-Modified-Since` is still current gets a bodiless 304 — so a browser
   *  refresh re-validates cheaply and re-uses its cached copy instead of
   *  re-downloading the body. Otherwise the body is served, from the shared
   *  versioned, path-keyed gzip cache when the client accepts gzip (declaring
   *  `Content-Encoding: gzip` makes the GzipFilter pass it through rather than
   *  double-compress). `revalidate` adds `Cache-Control: private, no-cache` so
   *  the browser caches the page yet always re-validates before re-use — the
   *  pages change when showtimes do, so we never want a stale copy served
   *  without a check. */
  private def conditionalGzipped(request: RequestHeader, contentType: String, vary: String,
                                 revalidate: Boolean, shared: Boolean = false,
                                 cacheKey: String = "", city: Option[City] = None)(body: => String): Result = {
    // THE VALIDATOR IS PER CITY, not model-wide. `readModel.lastModified` moves
    // when anything anywhere changes, so validating London's payload with it
    // meant a Warsaw showtime expired London's ETag: every city looked like it
    // changed every couple of minutes, and the client 304s and the edge cache
    // both lost most of their value. `lastModifiedFor` moves only when the bytes
    // THIS city renders can have changed -- including the corpus-wide film-address
    // reshuffles that genuinely do reach every city. `None` means a payload that
    // really is model-wide.
    val lastMod  = city.fold(readModel.lastModified)(c => readModel.lastModifiedFor(c.slug))
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    val httpDate = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
      .format(lastMod.atOffset(java.time.ZoneOffset.UTC))
    val cacheControl: Seq[(String, String)] =
      if (revalidate) Seq("Cache-Control" -> "private, no-cache")
      else if (shared) Seq("Cache-Control" -> s"public, max-age=0, s-maxage=${MovieController.SharedMaxAgeSeconds}")
      else Nil
    // AN ETAG AS WELL AS Last-Modified, BECAUSE A SHARED CACHE NEEDS ONE.
    //
    // Measured against the live edge on 2026-09-05: once Cloudflare holds a copy,
    // an `If-Modified-Since` against it comes back 200 WITH THE WHOLE BODY --
    // Cloudflare answers a conditional from cache off the ETag, and these
    // responses had none. So adding `s-maxage` traded the mobile apps' 0-byte
    // 304s for ~750 KB payloads: better for the origin, worse for the phone.
    // `/api/catalog` never had the problem precisely because it carries one.
    //
    // Derived from the read-model version and the cache key rather than hashing
    // the body: the body is the expensive thing here (it is why the gzip cache
    // exists) and the version already changes exactly when the body does. The
    // key is in it so two windows of the same path cannot share a validator.
    val etag = "\"" + Integer.toHexString((request.path + cacheKey).hashCode) + "-" + lastMod.getEpochSecond.toHexString + "\""
    val validators: Seq[(String, String)] = ("Last-Modified" -> httpDate) +: ("ETag" -> etag) +: cacheControl

    if (request.headers.get("If-None-Match").contains(etag) || ifModifiedSinceCurrent(request, lastMod))
      NotModified.withHeaders(validators*)
    else if (acceptsGzip(request)) {
      // ⚠️ THE KEY MUST CARRY EVERY INPUT THAT CHANGES THE BODY, and `request.path`
      // does not: it drops the query string. `?days=7` and the full payload are
      // the same path, so keying on it alone would serve one client's window to
      // another -- silently, with a 200 and a plausible body. `cacheKey` is the
      // normalised, parsed parameter rather than the raw query, so a crawler
      // appending `?foo=1` cannot mint unbounded entries.
      val bytes = responseCache.gzippedBody(request.path + cacheKey, lastMod)(body)
      Ok(bytes).as(contentType)
        .withHeaders((Seq("Content-Encoding" -> "gzip", "Vary" -> vary) ++ validators)*)
    } else
      Ok(body).as(contentType).withHeaders((("Vary" -> vary) +: validators)*)
  }

  private val HtmlContentType = "text/html; charset=utf-8"
  private val HtmlVary        = "Accept-Encoding"

  // Resolve the `/{city}/…` slug; 404 on an unknown city. Every city-scoped
  // handler wraps its body in this so resolution + not-found behaviour lives
  // in one place.
  /** Resolve a city slug against THIS deployment's country, 404ing anything else.
   *
   *  Resolving is not enough on its own: `City.bySlug` searches the global
   *  `City.all` (the union across every country), so Berlin resolves on the
   *  Poland host too — it is a real city, just not a Polish one. Serving it 200
   *  with an empty body is worse than a 404, because an empty listing is
   *  indistinguishable from a genuine "nothing on today": a client caches it
   *  along with the `Last-Modified` this deployment stamps, and the German
   *  deployment then answers that timestamp with a 304, leaving the client
   *  stranded on an empty listing for a city that has a full one. That is how a
   *  cross-country deep link came up as "no screenings" in the iOS app.
   *
   *  Same scope `sitemap` applies, for the same reason — a `KINOWO_COUNTRY=pl`
   *  host owns Poland's cities and nothing else. Note this is a COUNTRY scope,
   *  not a data one: a Polish city with no films today still renders (and still
   *  answers `[]`), because "we don't serve this city" and "this city is quiet
   *  tonight" are different answers.
   */
  private def withCity(slug: String)(f: City => Result): Result =
    City.bySlug(slug).filter(servingCountry.cities.contains) match {
      case Some(c) => f(c)
      case None    => NotFound(messages("error.unknownCity", slug))
    }

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
   *  cookie, and `s-maxage` cannot hand one visitor's page to another — the rule
   *  `SharedMaxAgeSeconds` states, met structurally instead of by inspection.
   *  Whoever is signed in is layered on after first paint, by `shared.js` off
   *  `/api/me` (which is `no-store`, so it cannot be shared either).
   *
   *  `?filter=` variants keep `private, no-cache`: still client-independent, but
   *  combinatorially many and not worth an edge entry each. */
  private def renderIndex(city: City, request: RequestHeader): Result = {
    implicit val c: City = city
    if (cacheablePlainPage(request)) {
      // 304 short-circuits before any work; on a 200 cache hit `renderIndexHtml`
      // (and its data-prep) never runs either.
      conditionalGzipped(request, HtmlContentType, HtmlVary, revalidate = false, shared = true,
                         city = Some(city))(renderIndexHtml(city, request).body)
        .withCookies(cityCookie(city))
    } else {
      Ok(renderIndexHtml(city, request))
        .withHeaders("Cache-Control" -> "private, no-cache")
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
   *  the HTML pages (see [[conditionalGzipped]]): a current `If-Modified-Since`
   *  yields a bodiless 304 (what warm mobile clients hit), otherwise the payload
   *  is served from the shared gzip cache. The endpoints don't set
   *  `Cache-Control` (mobile manages its own revalidation), so `revalidate` is
   *  off. Both the listing and the details payload track the same city's cache
   *  mtime, so a 304 on one is a 304 on the other. */
  private def conditionalJson(request: Request[AnyContent], city: City, cacheKey: String = "")(body: => play.api.libs.json.JsValue): Result =
    conditionalGzipped(request, "application/json", vary = "Accept-Encoding", revalidate = false,
                       shared = true, cacheKey = cacheKey, city = Some(city))(
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

  def debug(): Action[AnyContent] = Action { request =>
    devOnly {
      // The debug table is the global corpus; the only thing the view needs a
      // city for is the /movie fallback link on a row with no live showtimes
      // anywhere — give it the default city for that edge case.
      implicit val c: City = City.all.head
      // Which country's corpus to show (the boot country unless a Dev-only
      // ?country= switch selected another). `stack` binds every debug read below
      // to that country's Mongo db.
      val country = debugCountries.resolve(request)
      val stack   = debugCountries.stackFor(country)
      // Pulled on demand from Mongo: the web doesn't keep the `movies` model
      // warm, so the corpus dump reads the source rows the read model is
      // projected from directly. `findAllForListing` drops each row's per-cinema
      // `showtimes` (~58% of the corpus bytes, measured) server-side — the table
      // renders only metadata + counts; the showtimes are fetched per-row on
      // expand via `/debug/details`.
      //
      // Both `movies` and `pending_movies` are full-collection scans. `/debug`
      // is dev-only, so it is ALWAYS served over the local→prod Mongo tunnel,
      // where a single such cursor runs ~6 s (see `MovieRepository.findAll`).
      // Reading the two collections one after the other made every reload ~12 s;
      // firing them concurrently brings a cold load back down to a single scan's
      // latency. The 70 s outer wait sits just above each read's own 60 s
      // timeout so an inner timeout fires (and logs) first.
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val moviesFuture  = Future(stack.movieRepository.findAllForListing())
      val stagingFuture = Future(stack.stagingRepository.findAll())
      // The same bounded, index-backed queue snapshot `/debug/queue` serves —
      // read here too so the staging rows can be ORDERED by their place in the
      // queue (the page renders only the first `StagingRowLimit`, so the most
      // imminent rows must sort to the top server-side; the client poll then
      // repaints the live badge in place, but does not reorder).
      val queueFuture   = Future(stack.taskQueue.monitor(MovieController.DebugQueueActiveLimit))
      val (movies, (staging, queue)) =
        Await.result(moviesFuture.zip(stagingFuture.zip(queueFuture)), 70.seconds)
      val staged = staging.sortBy(r => (r.title.toLowerCase, r.cinema.displayName))
      Ok(views.html.debug(
        movies.sortBy(_.title.toLowerCase),
        // The SELECTED country's rules, not the deployment's: /debug can switch
        // countries, and a row's display title must read as its own corpus keyed it.
        stack.movieRepository.normalizer,
        MovieController.orderStagingByQueue(staged, queue.active, normalizer),
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(stack)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** How far behind the local read-mirror this stack reads through is, for the
   *  debug navbar's badge. Read per render rather than cached: it is two bounded
   *  queries against a loopback Mongo (~12–26ms), and a number that can itself go
   *  stale is exactly the thing this exists to stop. `None` in prod, where the
   *  pages read the source and there is no copy to be behind. */
  private def mirrorAge(stack: DebugStack): Option[services.MirrorFreshness.Age] =
    services.MirrorFreshness.describe(stack.mirrorFreshness.newestUpdate(), java.time.Instant.now())

  /** Dev-only: the per-(rating source, film) adaptive refresh cadence. Films are
   *  grouped by their current refresh interval, slowest (most backed-off / stable)
   *  first, with the last two displayed-value changes shown on hover. Reads the
   *  worker-written `rating_cadence` collection + resolves titles from the corpus. */
  def cadence(): Action[AnyContent] = Action { request =>
    devOnly {
      val country = debugCountries.resolve(request)
      val stack   = debugCountries.stackFor(country)
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val recordsFuture = Future(stack.ratingCadenceReader.all())
      val titlesFuture  = Future(stack.movieRepository.findAllForListing())
      val (records, rows) = Await.result(recordsFuture.zip(titlesFuture), 70.seconds)
      val titleByTmdb = rows.flatMap(r => r.record.tmdbId.map(_ -> r.title)).toMap
      implicit val c: City = City.all.head   // only for the shared debug navbar's city link
      Ok(views.html.cadence(services.cadence.CadenceReport.build(records, titleByTmdb.get), java.time.Instant.now(),
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(stack)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** Dev-only: the heavy per-source breakdown for ONE corpus row, fetched lazily
   *  by the /debug table when a row is expanded. Rendering every row's breakdown
   *  inline (each iterates `Cinema.all` × day × showtime) built one giant `Html`
   *  string that OOM'd the view on the full corpus; serving them per-row on
   *  demand keeps the initial /debug render to the light data rows only. The `id`
   *  is the row's Mongo `_id` (`StoredMovieRecord.idOf`), the same value the table
   *  rows are keyed on. */
  def debugDetails(id: String): Action[AnyContent] = Action { request =>
    devOnly {
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val stack = debugCountries.stackFor(debugCountries.resolve(request))
      stack.movieRepository.findById(id) match {
        case Some(row) =>
          // The per-source enrichment log, joined on the tmdbId-keyed rating key.
          // Two bounded `_id in [...]` lookups (4 keys each), not the readers'
          // full-collection reads — this runs per row-expand. Issued CONCURRENTLY:
          // the reads are independent, and against a remote Mongo the round-trip
          // is the whole cost (see `buildFrom`).
          val statuses = services.attempts.FilmAttemptReport.buildFrom(
            row.record.tmdbId, stack.attemptReader, stack.ratingCadenceReader)
          Ok(views.html.debugDetails(row.title, row.year, row.record,
            stack.movieRepository.normalizer, cinemaSourceUrls(), statuses))
        case None      => NotFound("no such row")
      }
    }
  }

  /** Dev-only: the active tasks in the durable queue (worked-on first, then the
   *  waiting block oldest-first), so the /debug staging table's queue columns can
   *  show, per row, whether an enrichment task already exists and its place in the
   *  queue. The page polls this; it's a bounded, index-backed `monitor` read (the
   *  same one `/tasks/data` serves), so the cost scales with viewers-while-open,
   *  not queue churn. Only the fields the page matches on are shipped — type,
   *  dedup key, state; a waiting task's place is already encoded by its list
   *  position. */
  def debugQueue(): Action[AnyContent] = Action { request =>
    devOnly {
      val snap = debugCountries.stackFor(debugCountries.resolve(request)).taskQueue.monitor(MovieController.DebugQueueActiveLimit)
      Ok(play.api.libs.json.Json.obj(
        "active" -> snap.active.map { t =>
          play.api.libs.json.Json.obj(
            "taskType" -> t.taskType,
            "dedupKey" -> t.dedupKey,
            "state"    -> t.state
          )
        }
      ))
    }
  }

  /** Dev-only: dump the warm read cache the web actually serves from — the
   *  `WebReadModel`'s in-memory `web_movies` + `web_screenings` views — so you
   *  can see exactly what a request would resolve against (vs `/debug`, which
   *  pulls the source `movies` corpus from Mongo on demand). */
  def debugReadModel(): Action[AnyContent] = Action { request =>
    devOnly {
      implicit val c: City = City.all.head
      val country    = debugCountries.resolve(request)
      val stack      = debugCountries.stackFor(country)
      val movies     = stack.readModelMovies().sortBy(_.title.toLowerCase)
      val screenings = stack.readModelScreenings().groupBy(_.filmId)
      Ok(views.html.debugReadModel(movies, screenings, stack.readModelLastModified(),
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(stack)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** Dev-only: force a TMDB re-enrich of one film from the /debug row button.
   *  Enqueues a `ResolveTmdb` task the worker's `ResolveTmdbHandler` consumes;
   *  that re-resolves the row and writes the TMDB-side fields, and the worker's
   *  `EnrichmentReaper` then re-runs every rating refresher for the row on its
   *  next pass. Idempotent per (title, year): a repeat
   *  click while one is queued returns `duplicate`. Returns JSON for the page's
   *  fetch. */
  def reenrich(title: String, year: Option[Int]): Action[AnyContent] = Action { request =>
    devOnly {
      if (title.isEmpty) BadRequest(play.api.libs.json.Json.obj("error" -> "missing title"))
      else {
        val result = debugCountries.stackFor(debugCountries.resolve(request)).taskQueue.enqueue(
          services.tasks.TaskType.ResolveTmdb,
          services.tasks.EnrichTaskKeys.resolveTmdbDedup(title, year),
          // `force` so the operator's explicit re-enrich re-resolves even an
          // already-resolved row (the normal flow's guard would otherwise skip it).
          services.tasks.EnrichTaskKeys.resolveTmdbPayload(title, year, force = true)
        )
        Ok(play.api.libs.json.Json.obj(
          "title"     -> title,
          "year"      -> year,
          "enqueued"  -> (result == services.tasks.EnqueueResult.Added),
          "duplicate" -> (result == services.tasks.EnqueueResult.Duplicate)
        ))
      }
    }
  }

  /** Dev-only visual-tuning page. Renders the real `_movieCard` partial(s)
   *  inside a `.tune-scope` wrapper plus a slider panel that drives the CSS
   *  custom properties the production card styles read. Self-contained: the
   *  sample films are built in-process so the page works regardless of cache
   *  state. */
  def tune(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.tune(MovieController.tuneSampleFilms))
      }
    }
  }

  /** Dev-only tuning page for the film-detail view — live sliders over the real
   *  `_filmDetailContent` for the title / meta / Seanse typography. */
  def tuneFilm(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.tuneFilm(MovieController.tuneSampleFilm))
      }
    }
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
    // used to need. It stops short of `s-maxage` only because a per-film edge
    // entry wants its own validator analysis, not because the bytes are anyone's.
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

  /** Reload the in-memory read-model caches from Mongo. Available in every mode
   * (unlike the rest of the debug endpoints, which are dev-only) so a fly.io
   * instance whose caches drifted from the derived collections can be reconciled
   * without a redeploy — but since it runs in prod and mutates state, it's gated
   * by [[AdminAction]] (login session + ADMIN_ALLOWLIST) rather than left open. */
  def rehydrate(city: String): Action[AnyContent] = adminAction {
    withCity(city) { _ =>
      val count = readModel.reload()
      Ok(s"rehydrated $count rows\n").as("text/plain; charset=utf-8")
    }
  }


  private def devOnly(result: => play.api.mvc.Result): play.api.mvc.Result = DevMode.gate(environment)(result)
  private def devMode: Boolean = DevMode.enabled(environment)
}

object MovieController {

  /** How long a SHARED cache (Cloudflare) may serve one of the public JSON
   *  payloads before it must re-check with us.
   *
   *  SIXTY SECONDS, AND THE NUMBER IS MEASURED RATHER THAN PICKED. The
   *  validator these responses carry is `WebReadModel.lastModifiedFor(city)`,
   *  which bumps when the bytes THAT CITY renders can have changed. Sampled
   *  against production on 2026-09-05 the model-wide stamp it replaced moved
   *  every couple of minutes -- but most of those moves were some other city's
   *  showtimes, which is exactly why the validator is now per city. A minute
   *  remains inside the interval a single city's content changes on, so an edge
   *  copy cannot outlive the data it was made from by more than one tick.
   *
   *  `max-age=0` ALONGSIDE IT IS THE POINT, not an oversight: browsers and the
   *  mobile apps keep revalidating on every request exactly as they did before
   *  this existed, so `If-Modified-Since` and the 304s behave identically. Only
   *  the SHARED cache is allowed to answer without asking, and only for a minute.
   *  What changes is WHO answers the conditional request: with an edge copy
   *  present Cloudflare returns the 304 itself instead of waking the JVM for a
   *  payload it will not send. Verified against the live edge: the origin's
   *  strong ETag survives the proxy unweakened and both `If-None-Match` and
   *  `If-Modified-Since` already round-trip to a 0-byte 304.
   *
   *  ⚠️ ONLY FOR RESPONSES THAT ARE BYTE-IDENTICAL FOR EVERY CLIENT. The bare
   *  city listing now qualifies and takes it: no template it reaches accepts a
   *  `models.User`, so no session cookie can move a byte of it (see
   *  `renderIndex`). What is per-user went the other way instead — `/api/me` and
   *  `/api/me/state` answer about one person and say `private, no-store`
   *  (`PerUserResponse`), and `shared.js` layers their answer onto the cached
   *  page after first paint. Filtered listings, facet pages and film pages stay
   *  `private, no-cache`: client-independent too, but not worth an edge entry
   *  each without their own validator analysis. A shared cache in front of
   *  anything per-user would serve one visitor's state to another. */
  val SharedMaxAgeSeconds: Int = 60

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

  /** Cap on the active tasks `/debug/queue` returns per poll — high enough to
   *  cover a backed-up enrichment queue so a pending movie's place is still
   *  resolvable, without an unbounded scan. */
  private val DebugQueueActiveLimit = 1000

  /** How many staging rows `/debug` renders. The header still shows the full
   *  `pending_movies` count; only the table is capped (and the page's live
   *  count-tracking JS caps appends to the same number). */
  val StagingRowLimit = 20

  /**
   * Order staging rows by their place in the durable queue — the same ranking
   * the /debug "Queue #" badge shows, so the rows that sort to the top (and thus
   * survive the `StagingRowLimit` cap) are the ones the worker is about to touch:
   *   1. a row with a worked-on `staging-*` task (▶ running) sorts first;
   *   2. then by best waiting place (1-based, oldest-first among waiting tasks);
   *   3. then queued-but-past-the-snapshot, then no-task last.
   * Ties keep the incoming order (the caller pre-sorts by title, cinema).
   *
   * `active`'s waiting tasks must be oldest-first, as `TaskQueue.monitor` returns
   * them (in one block, after the worked-on rows). This mirrors
   * the page's `waitingPlaces`/`badgeFor` JS (debug.scala.html) — keep the two in
   * sync so the server order and the live badge agree.
   */
  def orderStagingByQueue(
    staging: Seq[services.staging.StagingRecord],
    active:  Seq[services.tasks.TaskSummary],
    normalizer: TitleNormalizer,
  ): Seq[services.staging.StagingRecord] = {
    import services.tasks.TaskState
    // 1-based place of each waiting dedupKey among the waiting tasks (first seen).
    val waitingPlaces = {
      val b = scala.collection.mutable.LinkedHashMap.empty[String, Int]
      var i = 0
      active.foreach { t =>
        if (t.state == TaskState.Waiting) { i += 1; b.getOrElseUpdate(t.dedupKey, i) }
      }
      b.toMap
    }
    // Active `staging-*` tasks grouped by the film anchor their dedupKey embeds
    // (the segment after the `staging-*` prefix). Mirrors the JS `stagingTasksFor`.
    val byAnchor: Map[String, Seq[services.tasks.TaskSummary]] =
      active.flatMap { t =>
        if (t.taskType.startsWith("Staging")) t.dedupKey.split('|').lift(1).map(_ -> t) else None
      }.groupMap(_._1)(_._2)
    def rank(anchor: String): Double = byAnchor.get(anchor) match {
      case None | Some(Nil)                                        => Double.PositiveInfinity // no task
      case Some(ts) if ts.exists(_.state == TaskState.WorkedOn)    => 0d                       // ▶ running
      case Some(ts) =>
        val places = ts.flatMap(t => waitingPlaces.get(t.dedupKey))
        if (places.isEmpty) 1e9d else places.min.toDouble                                      // waiting / queued-past-snapshot
    }
    // sortBy is stable, so equal-rank rows keep the caller's (title, cinema) order.
    staging.sortBy(r => rank(normalizer.sanitize(r.title)))
  }

  /** Deterministic sample cards for the `/debug/tune` page — built in process
   *  so the tuning page renders the real `_movieCard` partial without depending
   *  on live cache contents. The set is a deliberate spread of edge cases so
   *  every pill row, rating variant, and vertical gap is on screen at once:
   *
   *   1. `rich`        — all four ratings (RT fresh), two cinemas, two days.
   *   2. `manyTimes`   — long wrapping title + 3 genres, one cinema with many
   *                      showtimes whose format tokens all differ, so the pills
   *                      wrap across several rows with wide format badges.
   *   3. `rotten`      — RT below 60 (the `.rotten` red variant) + a low
   *                      single-digit IMDb, so the rotten styling and the
   *                      narrowest rating values show.
   *   4. `extremes`    — the widest possible values: IMDb 10.0, Metacritic 100,
   *                      RT 100%, Filmweb 10.0 — stress-tests pill width.
   *   5. `metaOnly`    — only the Metacritic bare-number pill, alone on its row.
   *   6. `noRatings`   — no enrichment at all, so the ratings row is absent and
   *                      the meta→date gap collapses to just the title gap.
   *   7. `seniorClub`  — a programme-prefixed long title (the separate-row case)
   *                      with a single no-booking showtime (the `<span>` badge
   *                      variant, not the `<a>` one).
   *   8. `sparse`      — one rating, one cinema, one showtime: the loosest case.
   */
  private[controllers] def tuneSampleFilms: Seq[FilmSchedule] = {
    val base = LocalDate.of(2026, 6, 4)
    def at(d: LocalDate, h: Int, m: Int): LocalDateTime = d.atTime(h, m)

    def slot(d: LocalDate, h: Int, m: Int, fmt: List[String], booking: Boolean = true): Showtime =
      Showtime(
        at(d, h, m),
        bookingUrl = if (booking) Some("https://example.test/book") else None,
        room       = Some("Sala 1"),
        format     = fmt
      )

    // Build a resolved-movie sample directly (the web no longer holds
    // MovieRecords). Rating hrefs are placeholders — this page tunes layout, not
    // links — and `weightedRating` uses the production formula so the grid's
    // data-rating sort behaves as in prod.
    def res(
      title:     String,
      genres:    Seq[String],
      runtime:   Option[Int],
      year:      Option[Int],
      imdb:      Option[Double] = None,
      metascore: Option[Int]    = None,
      rt:        Option[Int]    = None,
      filmweb:   Option[Double] = None
    ): ResolvedMovie = {
      val weighted = {
        val ns = Seq(imdb, filmweb, metascore.map(_ / 10.0), rt.map(_ / 10.0)).flatten
        if (ns.isEmpty) 0.0 else ns.sum / ns.size
      }
      ResolvedMovie(
        _id = title, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
        runtimeMinutes = runtime, releaseYear = year, genres = genres, countries = Seq.empty,
        directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
        ratings = ResolvedRatings(
          imdb = imdb, imdbUrl = imdb.map(_ => "https://www.imdb.com/"),
          metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
          rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
          filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
        ),
        weightedRating = weighted
      )
    }

    def film(resolved: ResolvedMovie, showings: Seq[(LocalDate, Seq[CinemaShowtimes])]): FilmSchedule =
      FilmSchedule(
        movie          = Movie(resolved.title, runtimeMinutes = resolved.runtimeMinutes, releaseYear = resolved.releaseYear, genres = resolved.genres),
        posterUrl      = resolved.posterUrl,
        synopsis       = resolved.synopsis,
        cast           = resolved.cast,
        director       = resolved.directors,
        cinemaFilmUrls = Seq.empty,
        showings       = showings,
        resolved       = resolved,
        slug           = FilmHref.slugOf(resolved.title)
      )

    val rich = film(
      res("Incepcja", Seq("Sci-Fi", "Akcja"), Some(148), Some(2010), imdb = Some(8.8), metascore = Some(74), rt = Some(87), filmweb = Some(7.6)),
      Seq(
        base -> Seq(
          CinemaShowtimes(Multikino, Seq(slot(base, 17, 30, List("2D", "NAP")), slot(base, 20, 15, List("2D")))),
          CinemaShowtimes(Helios,    Seq(slot(base, 18, 0, List("IMAX", "2D"))))
        ),
        base.plusDays(1) -> Seq(
          CinemaShowtimes(Multikino, Seq(slot(base.plusDays(1), 19, 45, List("2D", "DUB"))))
        )
      )
    )

    // One cinema, eight showtimes, every slot a different format token set so
    // none is stripped as "common" — the badges wrap to several rows and the
    // wide tokens (4DX, VOSE, ATMOS) stress the pill's max width.
    val manyTimes = film(
      res("Spider-Man: Poprzez multiwersum (wersja rozszerzona)", Seq("Animacja", "Akcja", "Przygodowy"), Some(140), Some(2023), imdb = Some(8.6), metascore = Some(86), rt = Some(95), filmweb = Some(7.9)),
      Seq(base -> Seq(CinemaShowtimes(CinemaCityKinepolis, Seq(
        slot(base, 10, 0,  List("2D", "DUB")),
        slot(base, 12, 30, List("3D", "DUB")),
        slot(base, 14, 15, List("IMAX", "NAP")),
        slot(base, 16, 0,  List("4DX")),
        slot(base, 18, 20, List("VOSE")),
        slot(base, 20, 0,  List("ATMOS", "NAP")),
        slot(base, 21, 30, List("2D", "NAP", "ATMOS")),
        slot(base, 23, 0,  List("3D"))
      ))))
    )

    val rotten = film(
      res("Morbius", Seq("Akcja", "Horror"), Some(104), Some(2022), imdb = Some(4.3), metascore = Some(35), rt = Some(15), filmweb = Some(4.1)),
      Seq(base -> Seq(CinemaShowtimes(Helios, Seq(slot(base, 19, 0, List("2D", "NAP"))))))
    )

    val extremes = film(
      res("Ojciec chrzestny", Seq("Dramat", "Kryminał"), Some(175), Some(1972), imdb = Some(10.0), metascore = Some(100), rt = Some(100), filmweb = Some(10.0)),
      Seq(base -> Seq(CinemaShowtimes(KinoPalacowe, Seq(slot(base, 16, 45, List("2D", "NAP"))))))
    )

    val metaOnly = film(
      res("Aftersun", Seq("Dramat"), Some(102), Some(2022), metascore = Some(95)),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 20, 30, List("NAP"))))))
    )

    val noRatings = film(
      res("Pokaz przedpremierowy: Niezatytułowany film", Seq("Dramat"), None, Some(2026)),
      Seq(base -> Seq(CinemaShowtimes(Rialto, Seq(slot(base, 18, 15, List("NAP"))))))
    )

    val seniorClub = film(
      res("Kino Seniora: Niebo nad Berlinem", Seq("Dramat", "Fantasy"), Some(128), Some(1987), imdb = Some(8.0), filmweb = Some(7.8)),
      Seq(base -> Seq(CinemaShowtimes(KinoApollo, Seq(slot(base, 12, 0, List("NAP"), booking = false)))))
    )

    val sparse = film(
      res("Cicha noc", Seq("Dramat"), Some(98), Some(2017), filmweb = Some(7.1)),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 21, 0, List("2D"))))))
    )

    Seq(rich, manyTimes, rotten, extremes, metaOnly, noRatings, seniorClub, sparse)
  }

  /** One fully-populated film (synopsis + cast + director, which the listing
   *  samples leave empty) for the `/debug/tune/movie` page, so every meta block
   *  renders and its fonts are tunable. Built off the rich sample's ratings +
   *  multi-cinema showings tree. */
  private[controllers] def tuneSampleFilm: FilmSchedule =
    tuneSampleFilms.head.copy(
      synopsis       = Some(
        "Dom Cobb to wytrawny złodziej, najlepszy w niebezpiecznej sztuce ekstrakcji — " +
        "wykradania cennych sekretów z głębi podświadomości podczas snu. Tym razem dostaje " +
        "szansę na odkupienie: zadanie odwrotne, zaszczepienie idei zamiast jej kradzieży. " +
        "Tekst celowo długi, by dało się dostroić rozmiar i odstępy opisu na ekranie filmu."
      ),
      cast           = Seq("Leonardo DiCaprio", "Joseph Gordon-Levitt", "Elliot Page", "Tom Hardy", "Ken Watanabe"),
      director       = Seq("Christopher Nolan"),
      cinemaFilmUrls = Seq(Multikino -> "https://example.test/incepcja")
    )
}
