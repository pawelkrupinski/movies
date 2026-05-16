package services.cinemas

import services.cinemas.HeliosNuxt.{BaseUrl, BookingBase, CinemaSourceId, cleanTitle}
import models.{Cinema, CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._
import tools.{HeliosFetch, HttpFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import scala.util.Try

class HeliosClient(http: HttpFetch = HeliosFetch) extends CinemaScraper {

  val cinema: Cinema = Helios


  private val PageUrl    = "https://helios.pl/poznan/kino-helios/repertuar"
  private val ApiBase    = "https://restapi.helios.pl/api"
  private val WarsawZone = ZoneId.of("Europe/Warsaw")
  private val OffsetDtf  = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  // ── Fetch ─────────────────────────────────────────────────────────────────

  def fetch(): Seq[CinemaMovie] = {
    val rest     = fetchRestData()
    val enriched = enrichFromRest(HeliosNuxt.buildMovies(http.get(PageUrl)), rest)
    removeLessSpecificOverlaps(enriched ++ restOnlyMovies(enriched, rest)).sortBy(_.movie.title)
  }

  private case class RestData(
    screeningsById: Map[String, ApiScreening],
    movieDetails:   Map[String, ApiMovieInfo],
    screenNames:    Map[String, String]
  )

  private def fetchRestData(): RestData = {
    val today = LocalDate.now(WarsawZone)
    val in6   = today.plusDays(6)
    val screeningsUrl =
      s"$ApiBase/cinema/$CinemaSourceId/screening?dateTimeFrom=${today}T00:00:00&dateTimeTo=${in6}T23:59:59"

    val screeningsById = parseApiScreenings(Try(http.getAsync(screeningsUrl).join()).getOrElse("[]"))
    val movieBodies    = fetchBodies(screeningsById.values.map(_.movieId).toSeq.distinct)(id => s"$ApiBase/movie/$id")
    val screenBodies   = fetchBodies(screeningsById.values.map(_.screenId).toSeq.distinct)(id =>
      s"$ApiBase/cinema/$CinemaSourceId/screen/$id")

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

  private case class ApiMovieInfo(
    title:         Option[String],
    duration:      Option[Int],
    description:   Option[String],
    cast:          Option[String],
    director:      Option[String],
    year:          Option[Int],
    premierePl:    Option[java.time.LocalDate],
    premiereWorld: Option[java.time.LocalDate],
    posterUrl:     Option[String],
    slug:          Option[String],
    country:       Option[String]
  )

  private def parseApiMovieBody(body: String): Option[ApiMovieInfo] =
    Try(Json.parse(body)).toOption.flatMap { js =>
      (js \ "title").asOpt[String].filter(_.nonEmpty).map { title =>
        ApiMovieInfo(
          title         = Some(title),
          duration      = (js \ "duration").asOpt[Int],
          description   = (js \ "description").asOpt[String].filter(_.nonEmpty),
          cast          = (js \ "filmCast").asOpt[String].filter(_.nonEmpty),
          director      = (js \ "director").asOpt[String].filter(_.nonEmpty),
          year          = (js \ "yearOfProduction").asOpt[String].flatMap(s => Try(s.take(4).toInt).toOption),
          premierePl    = (js \ "premiereDate").asOpt[String]
                            .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption),
          premiereWorld = (js \ "worldPremiereDate").asOpt[String]
                            .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption),
          posterUrl     = (js \ "posters").asOpt[JsArray]
                            .flatMap(_.value.headOption)
                            .flatMap(_.asOpt[String])
                            .filter(_.startsWith("http")),
          slug          = (js \ "slug").asOpt[String].filter(_.nonEmpty),
          country       = (js \ "country").asOpt[String].map(_.trim).filter(_.nonEmpty)
        )
      }
    }

  private def fetchBodies(ids: Seq[String])(urlFor: String => String): Map[String, String] =
    ids
      .map(id => id -> http.getAsync(urlFor(id)))
      .flatMap { case (id, f) => Try(id -> f.join()).toOption }
      .toMap

  // ── REST enrichment of NUXT movies ────────────────────────────────────────
  //
  // NUXT is the primary source — it lists every screening on the repertoire page,
  // including events and screenings outside the 6-day REST window. REST adds
  // room name, format (2D/NAP/ATMOS…), and richer movie metadata for the subset
  // of screenings it covers. We match REST screenings to NUXT showtimes by the
  // shared screening UUID (NUXT sourceId == REST screening id), then look up the
  // REST movie body via screeningsById to enrich each NUXT movie.

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
          premierePl     = restInfo.flatMap(_.premierePl),
          premiereWorld  = restInfo.flatMap(_.premiereWorld),
          country        = restInfo.flatMap(_.country)
        ),
        posterUrl = restInfo.flatMap(_.posterUrl).orElse(cm.posterUrl),
        synopsis  = restInfo.flatMap(_.description),
        cast      = restInfo.flatMap(_.cast),
        director  = restInfo.flatMap(_.director),
        showtimes = cm.showtimes.map(enrichShowtime(_, rest))
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
              premierePl     = info.premierePl,
              premiereWorld  = info.premiereWorld,
              country        = info.country
            ),
            cinema    = Helios,
            posterUrl = info.posterUrl,
            filmUrl   = info.slug.map(s => s"$BaseUrl/filmy/$s"),
            synopsis  = info.description,
            cast      = info.cast,
            director  = info.director,
            showtimes = screenings.toSeq.flatMap(restShowtime(_, rest)).distinct.sortBy(_.dateTime)
          ))
      }
      .filter(_.showtimes.nonEmpty)
  }

  private def restShowtime(s: ApiScreening, rest: RestData): Option[Showtime] =
    s.dateTime.map(dt => Showtime(
      dateTime   = dt,
      bookingUrl = Some(s"$BookingBase/${s.id}?cinemaId=$CinemaSourceId"),
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

