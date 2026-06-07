package services.cinemas

import models._
import play.api.libs.json._
import services.cinemas.HeliosNuxt.{BookingBase, cleanTitle}
import services.movies.MovieService
import tools.{HeliosFetch, HttpFetch, ParallelDetailFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import scala.concurrent.duration._
import scala.util.Try

// `today` is injected so tests can pin it: the REST `/screening` + `/event`
// URLs bake the date window, and the recorded fixtures were captured for a
// specific day. The composition root passes the real Warsaw date; fixture
// wirings pass the fixture's capture date so the recorded URLs still match.
class HeliosClient(
  http:  HttpFetch    = HeliosFetch,
  cfg:   HeliosCinema = HeliosNuxt.Poznan,
  today: LocalDate    = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  override val cinema: Cinema = cfg.cinema

  private val sourceId   = cfg.sourceId
  private val PageUrl     = cfg.pageUrl
  private val ApiBase    = "https://restapi.helios.pl/api"
  private val WarsawZone = ZoneId.of("Europe/Warsaw")
  private val OffsetDtf  = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  // ── Fetch ─────────────────────────────────────────────────────────────────

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PageUrl, ApiBase, BookingBase)

  def fetch(): Seq[CinemaMovie] = {
    val rest     = fetchRestData()
    val enriched = enrichFromRest(HeliosNuxt.buildMovies(http.get(PageUrl), cfg), rest)
    mergeDuplicateFilms(removeLessSpecificOverlaps(enriched ++ restOnlyMovies(enriched, rest))).sortBy(_.movie.title)
  }

  private case class RestData(
    screeningsById: Map[String, ApiScreening],
    movieDetails:   Map[String, ApiMovieInfo],
    screenNames:    Map[String, String]
  )

  private def fetchRestData(): RestData = {
    val in6 = today.plusDays(6)
    val window = s"dateTimeFrom=${today}T00:00:00&dateTimeTo=${in6}T23:59:59"
    val screeningsUrl = s"$ApiBase/cinema/$sourceId/screening?$window"
    // `/event` honours the SAME date window as `/screening`. Without it the
    // endpoint returns the cinema's ENTIRE event history — ~4400 events, 9 MB,
    // ~99% of them in the past — and the 18-20s spent hauling-then-discarding
    // that payload was the single dominant cost of a Helios scrape (the whole
    // fetch dropped from ~20s to ~1s once windowed). We only ever use events
    // whose screenings fall in the window `/screening` already covers, so the
    // filter loses nothing.
    val eventsUrl = s"$ApiBase/cinema/$sourceId/event?$window"

    // `/screening` lists regular film screenings; event screenings (anime, concerts,
    // sports broadcasts) are excluded from it but carry the same id→screenId mapping
    // under `/event`. Merge both so room/format enrichment covers events too. The two
    // sets are disjoint; on the (unexpected) collision the regular screening wins.
    val regular         = parseApiScreenings(Try(http.getAsync(screeningsUrl).join()).getOrElse("[]"))
    val eventScreenings = parseEventScreenings(Try(http.getAsync(eventsUrl).join()).getOrElse("[]"))
    val screeningsById  = eventScreenings ++ regular

    val movieBodies    = fetchBodies("helios-movies", screeningsById.values.map(_.movieId).filter(_.nonEmpty).toSeq.distinct)(id => s"$ApiBase/movie/$id")
    val screenBodies   = fetchBodies("helios-screens", screeningsById.values.map(_.screenId).toSeq.distinct)(id =>
      s"$ApiBase/cinema/$sourceId/screen/$id")

    RestData(
      screeningsById = screeningsById,
      movieDetails   = movieBodies.flatMap { case (id, body) => parseApiMovieBody(body).map(id -> _) },
      screenNames    = screenBodies.flatMap { case (id, body) =>
        Try((Json.parse(body) \ "name").asOpt[String]).toOption.flatten.map(id -> _)
      }
    )
  }

  // ── REST parsing ──────────────────────────────────────────────────────────

  private case class ApiScreening(
    id:       String,
    movieId:  String,
    screenId: String,
    release:  String,
    dateTime: Option[LocalDateTime]
  )

  private def parseApiScreenings(body: String): Map[String, ApiScreening] =
    Try(Json.parse(body).as[JsArray]).map { arr =>
      arr.value.flatMap { s =>
        for {
          id       <- (s \ "id").asOpt[String]
          movieId  <- (s \ "movieId").asOpt[String]
          screenId <- (s \ "screenId").asOpt[String]
        } yield {
          val release  = (s \ "release").asOpt[String].getOrElse("")
          val dateTime = (s \ "screeningTimeFrom").asOpt[String]
            .flatMap(ts => Try(ZonedDateTime.parse(ts, OffsetDtf).withZoneSameInstant(WarsawZone).toLocalDateTime).toOption)
          id -> ApiScreening(id, movieId, screenId, release, dateTime)
        }
      }.toMap
    }.getOrElse(Map.empty)

  // The `/event` endpoint returns one entry per event screening (a flat array),
  // each with its `screeningId`, `screenId` and `release`. We map them
  // into the same ApiScreening shape used for room/format enrichment. `movieId` is
  // left empty — events already arrive fully described via NUXT, so they neither need
  // nor have a REST movie body to fetch (the empty id is filtered out before fetching).
  private def parseEventScreenings(body: String): Map[String, ApiScreening] =
    Try(Json.parse(body).as[JsArray]).map { arr =>
      arr.value.flatMap { e =>
        for {
          id       <- (e \ "screeningId").asOpt[String]
          screenId <- (e \ "screenId").asOpt[String]
        } yield {
          val release  = (e \ "release").asOpt[String].getOrElse("")
          val dateTime = (e \ "timeFrom").asOpt[String]
            .flatMap(ts => Try(ZonedDateTime.parse(ts, OffsetDtf).withZoneSameInstant(WarsawZone).toLocalDateTime).toOption)
          id -> ApiScreening(id, movieId = "", screenId, release, dateTime)
        }
      }.toMap
    }.getOrElse(Map.empty)

  private case class ApiMovieInfo(
    title:         Option[String],
    duration:      Option[Int],
    description:   Option[String],
    cast:          Seq[String],
    director:      Seq[String],
    year:          Option[Int],
    posterUrl:     Option[String],
    slug:          Option[String],
    countries:     Seq[String],
    genres:        Seq[String],
    trailerUrl:    Option[String]
  )

  private def parseApiMovieBody(body: String): Option[ApiMovieInfo] =
    Try(Json.parse(body)).toOption.flatMap { js =>
      (js \ "title").asOpt[String].filter(_.nonEmpty).map { title =>
        ApiMovieInfo(
          title         = Some(title),
          duration      = (js \ "duration").asOpt[Int],
          description   = (js \ "description").asOpt[String].filter(_.nonEmpty).map(tools.TextNormalization.stripHtml),
          cast          = (js \ "filmCast").asOpt[String].filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
          director      = (js \ "director").asOpt[String].filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
          year          = (js \ "yearOfProduction").asOpt[String].flatMap(s => Try(s.take(4).toInt).toOption),
          posterUrl     = (js \ "posters").asOpt[JsArray]
                            .flatMap(_.value.headOption)
                            .flatMap(_.asOpt[String])
                            .filter(_.startsWith("http")),
          slug          = (js \ "slug").asOpt[String].filter(_.nonEmpty),
          countries     = (js \ "country").asOpt[String].map(_.trim).filter(_.nonEmpty)
                            .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
          // Helios ships `genres: [{id, name, description}]` with lowercase
          // Polish names ("science fiction", "dramat"). Title-case at the
          // write boundary so the display layer doesn't have to special-case
          // this source's casing.
          genres        = (js \ "genres").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
                            .flatMap(g => (g \ "name").asOpt[String]).filter(_.nonEmpty)
                            .map(tools.TextNormalization.titleCaseIfAllLower),
          // Helios ships `trailers: [<youtube embed URL>]`. Take the first
          // entry; for non-embed YouTube URLs (live API has been seen to mix
          // both shapes) we re-canonicalise to the watch URL and let the
          // view layer convert back to /embed/ via TrailerEmbed.
          trailerUrl    = (js \ "trailers").asOpt[JsArray]
                            .flatMap(_.value.headOption).flatMap(_.asOpt[String])
                            .filter(_.nonEmpty)
                            .flatMap(u => services.movies.TrailerEmbed.youTubeId(u)
                              .map(id => s"https://www.youtube.com/watch?v=$id"))
        )
      }
    }

  private def fetchBodies(label: String, ids: Seq[String])(urlFor: String => String): Map[String, String] =
    ParallelDetailFetch.keyed(label, ids, 1.minute)(urlFor) { url =>
      Try(http.get(url)).toOption
    }.collect { case (id, Some(body)) => id -> body }

  // ── REST enrichment of NUXT movies ────────────────────────────────────────
  //
  // NUXT is the primary source — it lists every screening on the repertoire page,
  // including events and screenings outside the 6-day REST window. REST adds
  // room name, format (2D/NAP/ATMOS…), and richer movie metadata for the subset
  // of screenings it covers (regular screenings via `/screening`, event screenings
  // via `/event`). We match REST screenings to NUXT showtimes by the shared
  // screening UUID (NUXT sourceId == REST screening id), then look up the REST
  // movie body via screeningsById to enrich each NUXT movie.

  private def enrichFromRest(movies: Seq[CinemaMovie], rest: RestData): Seq[CinemaMovie] =
    movies.map { cm =>
      // Match REST screening → REST movie body via the shared screening UUID. Only
      // accept the enrichment when the REST title matches the NUXT title (after
      // cleaning) — Cyrillic vs Latin variants of the same Ukrainian film share a
      // booking UUID but are listed separately in the output.
      val restInfo = cm.showtimes
        .flatMap(_.bookingUrl).flatMap(extractScreeningId)
        .flatMap(rest.screeningsById.get).headOption
        .flatMap(s => rest.movieDetails.get(s.movieId))
        .filter(_.title.map(cleanTitle).contains(cm.movie.title))

      cm.copy(
        movie     = cm.movie.copy(
          runtimeMinutes = cm.movie.runtimeMinutes.orElse(restInfo.flatMap(_.duration)),
          releaseYear    = restInfo.flatMap(_.year),
          countries      = restInfo.map(_.countries).getOrElse(cm.movie.countries),
          genres         = restInfo.map(_.genres).getOrElse(cm.movie.genres)
        ),
        posterUrl  = restInfo.flatMap(_.posterUrl).orElse(cm.posterUrl),
        synopsis   = restInfo.flatMap(_.description),
        cast       = restInfo.map(_.cast).getOrElse(Seq.empty),
        director   = restInfo.map(_.director).getOrElse(Seq.empty),
        trailerUrl = restInfo.flatMap(_.trailerUrl).orElse(cm.trailerUrl),
        showtimes  = cm.showtimes.map(enrichShowtime(_, rest))
      )
    }

  // REST sometimes lists movies NUXT does not — e.g. Ukrainian-titled UA screenings
  // that share their underlying screening UUID with the Cyrillic NUXT entry but
  // appear in the REST API under a Latin-transliterated title. Include any REST
  // movie whose (cleaned) title is not represented in the NUXT-derived list.
  private def restOnlyMovies(nuxtCovered: Seq[CinemaMovie], rest: RestData): Seq[CinemaMovie] = {
    val nuxtTitles = nuxtCovered.map(_.movie.title).toSet
    rest.screeningsById.values.toSeq
      .groupBy(_.movieId).toSeq
      .flatMap { case (movieId, screenings) =>
        rest.movieDetails.get(movieId)
          .filterNot(_.title.map(cleanTitle).exists(nuxtTitles))
          .map(info => CinemaMovie(
            movie = Movie(
              title          = cleanTitle(info.title.getOrElse(movieId)),
              runtimeMinutes = info.duration,
              releaseYear    = info.year,
              countries      = info.countries,
              genres         = info.genres
            ),
            cinema    = cfg.cinema,
            posterUrl = info.posterUrl,
            filmUrl   = info.slug.map(s => s"${cfg.baseUrl}/filmy/$s"),
            synopsis   = info.description,
            cast       = info.cast,
            director   = info.director,
            trailerUrl = info.trailerUrl,
            showtimes  = screenings.toSeq.flatMap(restShowtime(_, rest)).distinct.sortBy(_.dateTime)
          ))
      }
      .filter(_.showtimes.nonEmpty)
  }

  private def restShowtime(s: ApiScreening, rest: RestData): Option[Showtime] =
    s.dateTime.map(dt => Showtime(
      dateTime   = dt,
      bookingUrl = Some(s"$BookingBase/${s.id}?cinemaId=$sourceId"),
      room       = rest.screenNames.get(s.screenId),
      format     = s.release.split("/").toList.filter(_.nonEmpty)
    ))

  private def enrichShowtime(st: Showtime, rest: RestData): Showtime =
    st.bookingUrl.flatMap(extractScreeningId).flatMap(rest.screeningsById.get) match {
      case Some(rs) => st.copy(room = rest.screenNames.get(rs.screenId), format = rs.release.split("/").toList.filter(_.nonEmpty))
      case None     => st
    }

  private def extractScreeningId(bookingUrl: String): Option[String] =
    if (bookingUrl.startsWith(s"$BookingBase/"))
      Some(bookingUrl.stripPrefix(s"$BookingBase/").takeWhile(_ != '?'))
    else None

  // The NUXT page sometimes lists the same physical screening under both a generic
  // film title and a more-specific event/promo title (e.g. "Tom i Jerry" and
  // "Tom i Jerry - seanse z konkursami HDD"). Drop the less-specific row when its
  // showtimes overlap in time with a more-specific row.
  /** Collapse entries that are the same film listed twice in one scrape.
   *
   *  Helios surfaces a film under more than one raw title that the cache's row
   *  identity (`MovieService.normalize`) treats as one:
   *    - a `/wydarzenie` event page and a `/filmy` film page differing only in
   *      case ("Drzewo Magii" vs "Drzewo magii"), and
   *    - a REST-only entry whose title carries a stray trailing space
   *      ("Straszny film " vs "Straszny film") — the same untrimmed title also
   *      defeats `enrichFromRest`'s exact-title match, so the film page never
   *      gets its year and the REST row leaks through as a separate entry.
   *
   *  Both spellings hash to the same cache key, so the cache writes a single
   *  slot but every raw entry overwrites it, and `recordCinemaScrape` flags each
   *  one `isNew` on every scrape — the recurring "(N new)" on Helios. Group by
   *  the cache's normalised title and merge each group into one entry: keep the
   *  canonical `/filmy` film as the base, fill any field it lacks from the
   *  siblings, and union their showtimes. */
  private[cinemas] def mergeDuplicateFilms(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    def rank(m: CinemaMovie): Int =
      (if (m.filmUrl.exists(_.contains("/filmy/"))) 4 else 0) +
        (if (m.filmUrl.isDefined) 2 else 0) +
        (if (m.movie.releaseYear.isDefined) 1 else 0)
    movies.groupBy(m => MovieService.normalize(m.movie.title)).values.toSeq.map { group =>
      group.reduce { (a, b) =>
        val (primary, other) = if (rank(a) >= rank(b)) (a, b) else (b, a)
        primary.copy(
          movie = primary.movie.copy(
            title          = primary.movie.title.trim,
            releaseYear    = primary.movie.releaseYear.orElse(other.movie.releaseYear),
            runtimeMinutes = primary.movie.runtimeMinutes.orElse(other.movie.runtimeMinutes),
            countries      = if (primary.movie.countries.nonEmpty) primary.movie.countries else other.movie.countries,
            genres         = if (primary.movie.genres.nonEmpty) primary.movie.genres else other.movie.genres
          ),
          posterUrl  = primary.posterUrl.orElse(other.posterUrl),
          filmUrl    = primary.filmUrl.orElse(other.filmUrl),
          synopsis   = primary.synopsis.orElse(other.synopsis),
          cast       = if (primary.cast.nonEmpty) primary.cast else other.cast,
          director   = if (primary.director.nonEmpty) primary.director else other.director,
          trailerUrl = primary.trailerUrl.orElse(other.trailerUrl),
          showtimes  = (primary.showtimes ++ other.showtimes).distinct.sortBy(_.dateTime)
        )
      }
    }
  }

  private def removeLessSpecificOverlaps(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val timesByTitle = movies.map(m => m.movie.title -> m.showtimes.map(_.dateTime).toSet).toMap
    val norm         = (s: String) => s.toLowerCase.replaceAll("\\s+", " ").trim
    movies.filterNot { m =>
      val times = timesByTitle(m.movie.title)
      movies.exists(other =>
        other.movie.title != m.movie.title &&
          norm(other.movie.title).startsWith(norm(m.movie.title) + " ") &&
          timesByTitle(other.movie.title).intersect(times).nonEmpty)
    }
  }
}

