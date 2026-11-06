package controllers

import models._
import play.api.libs.json.{Json, Writes}

// ── JSON API types ──────────────────────────────────────────────────────
case class ApiShowtime(time: String, format: String, room: Option[String], bookingURL: Option[WebHref])
case class ApiCinemaShowings(cinema: String, cinemaURL: Option[WebHref], showtimes: Seq[ApiShowtime])
case class ApiDayShowings(date: String, label: String, cinemas: Seq[ApiCinemaShowings])
case class ApiRatings(
  imdb: Option[Double], imdbURL: Option[WebHref],
  metascore: Option[Int], metacriticURL: Option[WebHref],
  rottenTomatoes: Option[Int], rottenTomatoesURL: Option[WebHref],
  filmweb: Option[Double], filmwebURL: Option[WebHref],
  // Some(true) when `filmwebURL` is the film's own Filmweb page but Filmweb has no rating yet (a new release): the apps show
  // a rating-less pill. Without it `filmwebURL` is a search page whenever the film has no stored link, so the apps could not
  // tell. Omitted otherwise, so builds that predate the field decode as before. (IMDb needs none: `imdbURL` is present only
  // for a known IMDb id, rated or not.)
  filmwebRatingPending: Option[Boolean] = None
)
case class ApiFilm(
  title: String, slug: String,
  // What the search matches besides the title: the original title and its romanisation ([[SearchTitles]]).
  searchTitles: Seq[String],
  posterURL: Option[WebHref], fallbackPosterURLs: Seq[WebHref],
  runtimeMinutes: Option[Int], releaseYear: Option[Int], genres: Seq[String],
  // Age rating / certificate (UK BBFC "15"/"PG"/…); omitted when the film has none.
  ageRating: Option[String],
  ratings: ApiRatings,
  countries: Seq[String], directors: Seq[String], cast: Seq[String],
  showings: Seq[ApiDayShowings],
  // Some(true) while the film's identity is provisional (`ResolvedMovie.provisional`: the take decided on the inputs as
  // they stand, an answer it reads not filed yet); omitted otherwise.
  provisional: Option[Boolean] = None,
  // The master-set id ([[services.cinemas.GenreNames]]) of each of `genres` that has one, in order — a stable,
  // language-independent key beside the display names. Additive: clients that ignore it are unaffected.
  genreIds: Seq[String] = Seq.empty
)

/** Detail-only payload for `GET /api/details`: the heavy text (synopsis) and
 *  trailers that the grid / filters never need. Split off the listing so the
 *  latency-sensitive `/api/repertoire` stays lean; clients fetch both in
 *  parallel and merge by `title`. Only films carrying a synopsis or at least
 *  one trailer are emitted. */
case class ApiFilmDetails(
  title: String, originalTitle: Option[String], synopsis: Option[String], trailerURLs: Seq[WebHref]
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
    trailerURLs   = fs.resolved.trailerUrls.flatMap(WebHref.of).map(_.asciiEncoded),
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
 *  per-area (de)selectable list the web filter builds server-side.
 *
 *  `seasons` names the venues that screen only in a season (open-air, summer, drive-in;
 *  [[models.Opening.Seasonal]]): see [[CinemaSeasons]]. ABSENT when the city has none, so
 *  every payload without one is byte-for-byte what it was and a decoder that does not know
 *  the key is never handed it empty. */
case class ApiCityCinemas(cinemas: Seq[String], areas: Seq[ApiCinemaArea], seasons: Option[Seq[ApiCinemaSeason]] = None)

object ApiCityCinemas {
  implicit val apiCinemaAreaWrites: Writes[ApiCinemaArea] = Json.writes[ApiCinemaArea]
  implicit val writes: Writes[ApiCityCinemas] = Json.writes[ApiCityCinemas]

  /** `today` is the city's own day, off the injected clock: it decides `offSeason`. */
  def from(city: City, today: java.time.LocalDate): ApiCityCinemas = ApiCityCinemas(
    cinemas = city.cinemaDisplayNames,
    areas   = city.areas.map(g => ApiCinemaArea(g.area.label, g.area.slug, g.cinemaDisplayNames)),
    seasons = Option(CinemaSeasons.of(city.cinemas, today)).filter(_.nonEmpty),
  )
}

object ApiFilm {
  implicit val apiShowtimeWrites: Writes[ApiShowtime] = Json.writes[ApiShowtime]
  implicit val apiCinemaShowingsWrites: Writes[ApiCinemaShowings] = Json.writes[ApiCinemaShowings]
  implicit val apiDayShowingsWrites: Writes[ApiDayShowings] = Json.writes[ApiDayShowings]
  implicit val apiRatingsWrites: Writes[ApiRatings] = Json.writes[ApiRatings]
  implicit val apiFilmWrites: Writes[ApiFilm] = Json.writes[ApiFilm]

  /** `language` is the city's: the day labels are spelled in it. */
  def from(fs: FilmSchedule, language: java.util.Locale): ApiFilm = {
    val resolved = fs.resolved
    val cinemaUrlMap = fs.linkableCinemaFilmUrls.map { case (c, href) => c.displayName -> href }.toMap
    ApiFilm(
      title            = fs.movie.title,
      // The film's canonical path segment on the web (`/{city}/movie/{slug}`).
      // Served rather than derived client-side: the fold handles Polish and
      // German diacritics, ß, and Cyrillic, and a Swift copy plus a Kotlin copy
      // would be two more places for it to drift from `tools.Slugify`.
      slug             = fs.slug.getOrElse(""),
      searchTitles     = fs.searchTitles,
      // Every URL below is a [[WebHref]], http(s) only — both apps hand the links
      // to the system, where a scraped custom-scheme or `intent:` URL would open
      // another app — and goes through AsciiUrl: the mobile models decode these
      // fields as `URL`, and a strict parser fails the whole listing on one
      // scraped poster link with a Polish letter in it.
      posterURL        = fs.posterHref.map(_.asciiEncoded),
      fallbackPosterURLs = fs.fallbackPosterHrefs.map(_.asciiEncoded),
      runtimeMinutes   = fs.movie.runtimeMinutes,
      releaseYear      = fs.movie.releaseYear,
      genres           = fs.movie.genres,
      ageRating        = resolved.ageRating,
      ratings          = ratingsOf(resolved.ratings),
      countries        = fs.movie.countries,
      directors        = fs.director,
      cast             = fs.cast,
      showings         = fs.showings.map { case (date, cinemas) =>
        ApiDayShowings(
          date    = date.toString,
          label   = CardFormat.date(date, fs.asOf, language),
          cinemas = cinemas.map(cinemaShowingsOf(_, cinemaUrlMap))
        )
      },
      provisional      = Option.when(resolved.provisional)(true),
      genreIds         = fs.movie.genres.flatMap(services.cinemas.GenreNames.idOf).distinct
    )
  }

  private def ratingsOf(ratings: ResolvedRatings): ApiRatings = ApiRatings(
    imdb              = ratings.imdb,
    imdbURL           = ratings.imdbUrl.flatMap(WebHref.of).map(_.asciiEncoded),
    metascore         = ratings.metascore,
    metacriticURL     = WebHref.of(ratings.metacriticUrl).map(_.asciiEncoded),
    rottenTomatoes    = ratings.rottenTomatoes,
    rottenTomatoesURL = WebHref.of(ratings.rottenTomatoesUrl).map(_.asciiEncoded),
    filmweb           = ratings.filmweb,
    filmwebURL        = WebHref.of(ratings.filmwebUrl).map(_.asciiEncoded),
    filmwebRatingPending = Option.when(ratings.filmweb.isEmpty && ratings.filmwebLinked)(true)
  )

  private def cinemaShowingsOf(cs: CinemaShowtimes, cinemaUrlMap: Map[String, WebHref]): ApiCinemaShowings =
    ApiCinemaShowings(
      cinema    = cs.cinema.displayName,
      cinemaURL = cinemaUrlMap.get(cs.cinema.displayName).map(_.asciiEncoded),
      showtimes = cs.showtimes.map { st =>
        ApiShowtime(
          time       = CardFormat.time(st.dateTime),
          format     = st.format.mkString(" "),
          room       = st.room,
          bookingURL = st.bookingUrl.flatMap(WebHref.of).map(_.asciiEncoded)
        )
      }
    )
}
