package clients

import models.{CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._
import tools.{HeliosFetch, HttpFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import scala.util.Try

class HeliosClient(http: HttpFetch = HeliosFetch) {

  private val PageUrl        = "https://helios.pl/poznan/kino-helios/repertuar"
  private val CinemaSourceId = "815face9-2a1d-4c62-9b2f-a361574b79a2"
  private val BaseUrl        = "https://helios.pl/poznan/kino-helios"
  private val BookingBase    = "https://bilety.helios.pl/screen"
  private val ApiBase        = "https://restapi.helios.pl/api"
  private val WarsawZone     = ZoneId.of("Europe/Warsaw")
  private val OffsetDtf      = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  def fetch(): Seq[CinemaMovie] = {
    val today = LocalDate.now(WarsawZone)
    val in6   = today.plusDays(6)

    val screeningsUrl =
      s"$ApiBase/cinema/$CinemaSourceId/screening" +
        s"?dateTimeFrom=${today}T00:00:00&dateTimeTo=${in6}T23:59:59"

    val nuxtHtml = http.get(PageUrl)
    val screeningsById =
      parseApiScreenings(Try(http.getAsync(screeningsUrl).join()).getOrElse("[]"))

    val movieBodies =
      fetchBodies(screeningsById.values.map(_.movieId).toSeq.distinct)(id => s"$ApiBase/movie/$id")

    val screenBodies =
      fetchBodies(screeningsById.values.map(_.screenId).toSeq.distinct)(id =>
        s"$ApiBase/cinema/$CinemaSourceId/screen/$id"
      )

    val restMovies = buildCinemaMovies(screeningsById, movieBodies, screenBodies)
    val nuxtMovies = buildCinemaMoviesFromNuxt(nuxtHtml)

    uniquePosterUrls(
      mergeMoviesByTitle(
        mergeLessSpecificSameShowtimeRows(restMovies ++ nuxtMovies)
      )
    )
  }

  private def mergeLessSpecificSameShowtimeRows(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val enriched =
      movies.map { movie =>
        val title = movie.movie.title
        val times = movie.showtimes.map(_.dateTime).toSet

        val lessSpecificRows =
          movies.filter { other =>
            val otherTitle = other.movie.title

            title != otherTitle &&
              isLessSpecificTitle(otherTitle, title) &&
              other.showtimes.exists(showtime => times.contains(showtime.dateTime))
          }

        if (lessSpecificRows.isEmpty) movie
        else {
          movie.copy(
            movie = movie.movie.copy(
              runtimeMinutes = movie.movie.runtimeMinutes.orElse(lessSpecificRows.flatMap(_.movie.runtimeMinutes).headOption),
              releaseYear    = movie.movie.releaseYear.orElse(lessSpecificRows.flatMap(_.movie.releaseYear).headOption),
              premierePl     = movie.movie.premierePl.orElse(lessSpecificRows.flatMap(_.movie.premierePl).headOption),
              premiereWorld  = movie.movie.premiereWorld.orElse(lessSpecificRows.flatMap(_.movie.premiereWorld).headOption)
            ),
            posterUrl = movie.posterUrl.orElse(lessSpecificRows.flatMap(_.posterUrl).headOption),
            filmUrl   = movie.filmUrl.orElse(lessSpecificRows.flatMap(_.filmUrl).headOption),
            synopsis  = movie.synopsis.orElse(lessSpecificRows.flatMap(_.synopsis).headOption),
            cast      = movie.cast.orElse(lessSpecificRows.flatMap(_.cast).headOption),
            director  = movie.director.orElse(lessSpecificRows.flatMap(_.director).headOption)
          )
        }
      }

    enriched.filterNot { movie =>
      val title = movie.movie.title
      val times = movie.showtimes.map(_.dateTime).toSet

      enriched.exists { other =>
        val otherTitle = other.movie.title

        title != otherTitle &&
          isLessSpecificTitle(title, otherTitle) &&
          other.showtimes.exists(showtime => times.contains(showtime.dateTime))
      }
    }
  }

  private def isLessSpecificTitle(title: String, otherTitle: String): Boolean =
    normalizeTitle(otherTitle).startsWith(normalizeTitle(title) + " ")

  private def normalizeTitle(title: String): String =
    title.toLowerCase.replaceAll("\\s+", " ").trim

  private def uniquePosterUrls(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val seen = scala.collection.mutable.Set.empty[String]

    movies.map { movie =>
      movie.posterUrl match {
        case Some(url) if seen(url) =>
          movie.copy(posterUrl = None)

        case Some(url) =>
          seen += url
          movie

        case None =>
          movie
      }
    }
  }

  private def mergeMoviesByTitle(movies: Seq[CinemaMovie]): Seq[CinemaMovie] =
    movies
      .groupBy(_.movie.title)
      .values
      .map { rows =>
        val best =
          rows.maxBy(row =>
            Seq(
              row.posterUrl,
              row.movie.runtimeMinutes,
              row.filmUrl,
              row.synopsis,
              row.cast,
              row.director
            ).count(_.nonEmpty)
          )

        best.copy(
          movie = best.movie.copy(
            runtimeMinutes = rows.flatMap(_.movie.runtimeMinutes).headOption,
            releaseYear    = rows.flatMap(_.movie.releaseYear).headOption,
            premierePl     = rows.flatMap(_.movie.premierePl).headOption,
            premiereWorld  = rows.flatMap(_.movie.premiereWorld).headOption
          ),
          posterUrl = rows.flatMap(_.posterUrl).headOption,
          filmUrl   = rows.flatMap(_.filmUrl).headOption,
          synopsis  = rows.flatMap(_.synopsis).headOption,
          cast      = rows.flatMap(_.cast).headOption,
          director  = rows.flatMap(_.director).headOption,
          showtimes = rows
            .flatMap(_.showtimes)
            .groupBy(showtime => showtime.bookingUrl.getOrElse(showtime.dateTime.toString))
            .values
            .map { sameScreening =>
              sameScreening.maxBy(showtime =>
                Seq(showtime.room, showtime.format).count(_.nonEmpty)
              )
            }
            .toSeq
            .sortBy(_.dateTime)
        )
      }
      .toSeq
      .sortBy(_.movie.title)

  private def fetchBodies(ids: Seq[String])(urlFor: String => String): Map[String, String] =
    ids
      .map(id => id -> http.getAsync(urlFor(id)))
      .flatMap { case (id, future) => Try(id -> future.join()).toOption }
      .toMap

  private def buildCinemaMovies(
                                 screeningsById: Map[String, ApiScreening],
                                 movieBodies: Map[String, String],
                                 screenBodies: Map[String, String]
                               ): Seq[CinemaMovie] = {
    val movieDetails =
      movieBodies.flatMap { case (id, body) =>
        parseApiMovieBody(body).map(id -> _)
      }

    val screenNames =
      screenBodies.flatMap { case (id, body) =>
        json(body).flatMap(js => firstString(js, "name")).map(id -> _)
      }

    screeningsById.values.toSeq
      .groupBy(_.movieId)
      .toSeq
      .flatMap { case (movieId, screenings) =>
        val movie =
          movieDetails.get(movieId).orElse(screenings.flatMap(_.movie).headOption)

        movie.map { movie =>
          CinemaMovie(
            movie = Movie(
              title          = cleanTitle(movie.title.getOrElse(movieId)),
              runtimeMinutes = movie.duration,
              releaseYear    = movie.year,
              premierePl     = movie.premierePl,
              premiereWorld  = movie.premiereWorld
            ),
            cinema    = Helios,
            posterUrl = movie.posterUrl,
            filmUrl   = movie.slug.map(slug => s"$BaseUrl/filmy/$slug"),
            synopsis  = movie.description,
            cast      = movie.cast,
            director  = movie.director,
            showtimes = screenings
              .flatMap(screening => toShowtime(screening, screenNames))
              .distinct
              .sortBy(_.dateTime)
          )
        }
      }
      .filter(_.showtimes.nonEmpty)
      .sortBy(_.movie.title)
  }

  private def toShowtime(screening: ApiScreening, screenNames: Map[String, String]): Option[Showtime] =
    screening.dateTime.map { dateTime =>
      Showtime(
        dateTime   = dateTime,
        bookingUrl = Some(s"$BookingBase/${screening.id}?cinemaId=$CinemaSourceId"),
        room       = screenNames.get(screening.screenId),
        format     = Some(screening.release).filter(_.nonEmpty)
      )
    }

  private case class ApiScreening(
                                   id: String,
                                   movieId: String,
                                   screenId: String,
                                   release: String,
                                   dateTime: Option[LocalDateTime],
                                   movie: Option[ApiMovieInfo]
                                 )

  private def parseApiScreenings(body: String): Map[String, ApiScreening] =
    json(body).map { root =>
      jsonArray(root).flatMap { s =>
        for {
          id       <- firstString(s, "id", "sourceId", "screeningId")
          movieId  <- firstString(s, "movieId", "movieSourceId").orElse(nestedString(s, "movie", "id")).orElse(nestedString(s, "movie", "sourceId"))
          screenId <- firstString(s, "screenId", "screenSourceId").orElse(nestedString(s, "screen", "id")).orElse(nestedString(s, "screen", "sourceId"))
        } yield {
          val release =
            firstString(s, "release", "moviePrint", "format").getOrElse("")

          val embeddedMovie =
            (s \ "movie").toOption.flatMap(parseApiMovieJson)
              .orElse((s \ "screeningMovie" \ "movie").toOption.flatMap(parseApiMovieJson))
              .orElse((s \ "screeningMovie").toOption.flatMap(parseApiMovieJson))
              .orElse(
                (s \ "screeningMovies").asOpt[JsArray]
                  .flatMap(_.value.headOption)
                  .flatMap(x => (x \ "movie").toOption)
                  .flatMap(parseApiMovieJson)
              )
              .orElse(
                (s \ "screeningMovies").asOpt[JsArray]
                  .flatMap(_.value.headOption)
                  .flatMap(parseApiMovieJson)
              )
              .orElse(parseApiMovieJson(s))

          id -> ApiScreening(
            id       = id,
            movieId  = movieId,
            screenId = screenId,
            release  = release,
            dateTime = parseAnyDateTime(s),
            movie    = embeddedMovie
          )
        }
      }.toMap
    }.getOrElse(Map.empty)

  private def jsonArray(root: JsValue): Seq[JsValue] =
    root.asOpt[JsArray]
      .orElse((root \ "data").asOpt[JsArray])
      .orElse((root \ "items").asOpt[JsArray])
      .orElse((root \ "screenings").asOpt[JsArray])
      .orElse((root \ "result").asOpt[JsArray])
      .map(_.value.toSeq)
      .getOrElse(Seq.empty)

  private def parseAnyDateTime(js: JsValue): Option[LocalDateTime] = {
    val preferred =
      Seq(
        "screeningDateTimeFrom",
        "dateTimeFrom",
        "timeFrom",
        "startTime",
        "startsAt",
        "dateTime",
        "startDateTime",
        "from"
      ).flatMap(name => (js \ name).asOpt[String])

    val allStrings =
      collectStrings(js)

    (preferred ++ allStrings)
      .flatMap(parseApiDateTime)
      .headOption
  }

  private def collectStrings(js: JsValue): Seq[String] =
    js match {
      case JsString(value) => Seq(value)
      case JsArray(values) => values.toSeq.flatMap(collectStrings)
      case JsObject(fields) => fields.values.toSeq.flatMap(collectStrings)
      case _ => Seq.empty
    }

  private def parseApiDateTime(value: String): Option[LocalDateTime] = {
    val normalized = value.trim.replace(' ', 'T')

    val looksLikeDateTime =
      normalized.matches(""".*\d{4}-\d{2}-\d{2}T\d{2}:\d{2}.*""")

    if (!looksLikeDateTime) None
    else {
      Try(LocalDateTime.parse(normalized)).toOption
        .orElse(
          Try(
            ZonedDateTime
              .parse(normalized, OffsetDtf)
              .withZoneSameInstant(WarsawZone)
              .toLocalDateTime
          ).toOption
        )
    }
  }

  private case class ApiMovieInfo(
                                   duration: Option[Int],
                                   description: Option[String],
                                   cast: Option[String],
                                   director: Option[String],
                                   year: Option[Int],
                                   premierePl: Option[java.time.LocalDate],
                                   premiereWorld: Option[java.time.LocalDate],
                                   posterUrl: Option[String],
                                   title: Option[String],
                                   slug: Option[String]
                                 )

  private def parseApiMovieBody(body: String): Option[ApiMovieInfo] =
    json(body).flatMap { root =>
      parseApiMovieJson(
        firstObject(root, "data", "item", "movie", "result").getOrElse(root)
      )
    }

  private def parseApiMovieJson(js: JsValue): Option[ApiMovieInfo] =
    firstString(
      js,
      "title",
      "name",
      "movieTitle",
      "movieName",
      "eventTitle",
      "eventName",
      "displayTitle",
      "displayName"
    )
      .orElse(nestedString(js, "movie", "title"))
      .orElse(nestedString(js, "movie", "name"))
      .orElse(nestedString(js, "event", "title"))
      .orElse(nestedString(js, "event", "name"))
      .orElse(nestedString(js, "film", "title"))
      .orElse(nestedString(js, "film", "name"))
      .map { title =>
        ApiMovieInfo(
          duration =
            (js \ "duration").asOpt[Int]
              .orElse((js \ "duration").asOpt[String].flatMap(parseInt))
              .orElse(nestedString(js, "movie", "duration").flatMap(parseInt)),
          description =
            firstString(js, "description").filter(_.nonEmpty),
          cast =
            firstString(js, "filmCast", "cast").filter(_.nonEmpty),
          director =
            firstString(js, "director").filter(_.nonEmpty),
          year =
            firstString(js, "yearOfProduction", "productionYear").flatMap(s => parseInt(s.take(4))),
          premierePl =
            firstString(js, "premiereDate")
              .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption),
          premiereWorld =
            firstString(js, "worldPremiereDate")
              .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption),
          posterUrl = posterUrlFromJson(js),
          title     = Some(title),
          slug =
            firstString(
              js,
              "slug",
              "movieSlug",
              "eventSlug",
              "filmSlug"
            )
              .orElse(nestedString(js, "movie", "slug"))
              .orElse(nestedString(js, "event", "slug"))
              .orElse(nestedString(js, "film", "slug"))
        )
      }

  private def posterUrlFromJson(js: JsValue): Option[String] =
    firstString(
      js,
      "poster",
      "posterUrl",
      "posterURL",
      "image",
      "imageUrl",
      "imageURL",
      "photo",
      "photoUrl",
      "cover",
      "coverUrl"
    )
      .orElse(nestedString(js, "posterPhoto", "url"))
      .orElse(nestedString(js, "poster", "url"))
      .orElse(nestedString(js, "image", "url"))
      .orElse(
        (js \ "posters").asOpt[JsArray]
          .flatMap(_.value.headOption)
          .flatMap {
            case JsString(url) => Some(url)
            case obj: JsObject => firstString(obj, "url", "fileUrl", "path")
            case _             => None
          }
      )
      .filter(_.startsWith("http"))

  private def firstObject(root: JsValue, names: String*): Option[JsValue] =
    names.iterator
      .flatMap(name => (root \ name).toOption)
      .collectFirst { case obj: JsObject => obj }

  private def firstString(js: JsValue, names: String*): Option[String] =
    names.iterator
      .flatMap(name => (js \ name).asOpt[String])
      .find(_.nonEmpty)

  private def nestedString(js: JsValue, path: String*): Option[String] =
    path
      .foldLeft[Option[JsValue]](Some(js)) { case (current, field) =>
        current.flatMap(value => (value \ field).toOption)
      }
      .flatMap(_.asOpt[String])
      .filter(_.nonEmpty)

  private def cleanTitle(title: String): String =
    Seq(
      " w Helios RePlay",
      " w Helios Anime",
      " w Helios na Scenie",
      " - Salon Kultury Helios",
      " - KNTJ",
      " - KNT"
    ).foldLeft(title)((t, suffix) => t.stripSuffix(suffix))

  private def json(body: String): Option[JsValue] =
    Try(Json.parse(body)).toOption

  private def parseInt(value: String): Option[Int] =
    Try(value.trim.toInt).toOption

  // ── NUXT fallback parser for events/RePlay rows missing from REST ─────────

  private def buildCinemaMoviesFromNuxt(html: String): Seq[CinemaMovie] = {
    val parsed = parseNuxtPage(html)

    val nuxtRows =
      parsed.showtimesByMovie.toSeq.flatMap { case (movieId, slots) =>
        parsed.movies.get(movieId).map(movie => movie -> slots)
      }

    val embeddedRows =
      parsed.embeddedRows.map { row =>
        row.movie -> Seq(row.dateTime -> row.screeningId)
      }

    (nuxtRows ++ embeddedRows ++ parseHtmlRepertoireRows(html))
      .groupBy(_._1.title)
      .values
      .map { rows =>
        val movies = rows.map(_._1)
        val title =
          cleanTitle(movies.head.title)

        val movie =
          movies
            .filter(m => cleanTitle(m.title) == title)
            .find(_.runtimeMinutes.nonEmpty)
            .orElse(movies.find(_.runtimeMinutes.nonEmpty))
            .getOrElse(movies.head)

        val slots =
          rows
            .flatMap(_._2)
            .distinct
            .sortBy(_._1)

        CinemaMovie(
          movie = Movie(
            title          = title,
            runtimeMinutes = movie.runtimeMinutes,
            releaseYear    = None,
            premierePl     = None,
            premiereWorld  = None
          ),
          cinema    = Helios,
          posterUrl = movies.flatMap(_.posterUrl).headOption,
          filmUrl   = movies.flatMap(_.filmUrl).headOption,
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = slots
            .map { case (dateTime, screeningId) =>
              Showtime(
                dateTime   = dateTime,
                bookingUrl = Some(s"$BookingBase/$screeningId?cinemaId=$CinemaSourceId").filter(_ => screeningId.nonEmpty),
                room       = None,
                format     = None
              )
            }
            .distinct
        )
      }
      .filter(_.showtimes.nonEmpty)
      .toSeq
  }

  private def parseHtmlRepertoireRows(html: String): Seq[(NuxtMovie, Seq[(LocalDateTime, String)])] = {
    val groupPattern =
      """(?s)<div aria-labelledby="repertoire-screening-movie-\d+" role="group".*?<h2[^>]*><a href="([^"]+)">([^<]+)</a></h2>(.*?)(?=<div aria-labelledby="repertoire-screening-movie-\d+" role="group"|</section>)""".r

    val showtimePattern =
      """(?s)href="https://bilety\.helios\.pl/screen/([^"?]+)[^"]*".*?<time datetime="([^"]+)"""".r

    groupPattern.findAllMatchIn(html).flatMap { m =>
      val href  = m.group(1)
      val title = htmlText(m.group(2))
      val body  = m.group(3)

      val slots =
        showtimePattern.findAllMatchIn(body).flatMap { sm =>
          parseHtmlDateTime(sm.group(2)).map(_ -> sm.group(1))
        }.toSeq

      Option.when(title.nonEmpty && slots.nonEmpty) {
        NuxtMovie(
          title          = title,
          slug           = href.split("/").lastOption.getOrElse(title),
          posterUrl      = None,
          filmUrl        = Some(absoluteHeliosUrl(href)),
          runtimeMinutes = None
        ) -> slots
      }
    }.toSeq
  }

  private def absoluteHeliosUrl(href: String): String =
    if (href.startsWith("http")) href
    else "https://helios.pl" + href

  private def htmlText(value: String): String =
    value
      .replaceAll("<[^>]+>", "\n")
      .replace("&amp;", "&")
      .split("\\s*\\R\\s*")
      .filter(_.nonEmpty)
      .lastOption
      .getOrElse("")
      .trim

  private def parseHtmlDateTime(value: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(value.take(19).replace(' ', 'T'))).toOption

  private case class NuxtPage(
                               movies: Map[String, NuxtMovie],
                               showtimesByMovie: Map[String, Seq[(LocalDateTime, String)]],
                               embeddedRows: Seq[NuxtMovieRow]
                             )

  private case class NuxtMovieRow(
                                   movieId: String,
                                   movie: NuxtMovie,
                                   dateTime: LocalDateTime,
                                   screeningId: String
                                 )

  private case class NuxtMovie(
                                title: String,
                                slug: String,
                                posterUrl: Option[String],
                                filmUrl: Option[String],
                                runtimeMinutes: Option[Int]
                              )

  private def parseNuxtPage(html: String): NuxtPage = {
    val nuxtIndex = html.lastIndexOf("window.__NUXT__")
    if (nuxtIndex < 0) return NuxtPage(Map.empty, Map.empty, Seq.empty)

    val script = html.substring(nuxtIndex)

    val paramsStart = script.indexOf("(function(") + "(function(".length
    val paramsEnd   = script.indexOf("){", paramsStart)
    val bodyEnd     = script.indexOf("}(")

    if (paramsStart < "(function(".length || paramsEnd < 0 || bodyEnd < 0)
      return NuxtPage(Map.empty, Map.empty, Seq.empty)

    val paramNames = script.substring(paramsStart, paramsEnd).split(",").toSeq
    val valuesRaw  = script.substring(bodyEnd + 2)
    val valuesEnd  = valuesRaw.lastIndexOf("))")

    val paramMap: Map[String, JsValue] =
      Try {
        val raw = valuesRaw.substring(0, valuesEnd)
        val clean =
          raw
            .replaceAll("""Array\(\d+\)""", "null")
            .replace("undefined", "null")

        val valuesArray = Json.parse("[" + clean + "]").as[JsArray]
        paramNames.zip(valuesArray.value).toMap
      }.getOrElse(Map.empty)

    def resolve(token: String): Option[String] = {
      val trimmed = token.trim

      val value =
        if (trimmed.startsWith("\""))
          Some(trimmed.stripPrefix("\"").stripSuffix("\""))
        else
          paramMap.get(trimmed).flatMap {
            case JsString(s) => Some(s)
            case n: JsNumber => n.value.toBigIntExact.map(_.toString)
            case _           => None
          }

      value.map(_.replace("\\u002F", "/"))
    }

    val body = script.substring(paramsEnd + 2, bodyEnd)

    val screeningsMarker =
      """screenings:\{"\d{4}-\d{2}-\d{2}"""".r.findFirstMatchIn(body)

    screeningsMarker match {
      case None =>
        NuxtPage(Map.empty, Map.empty, Seq.empty)

      case Some(marker) =>
        val movieBody      = body.substring(0, marker.start)
        val screeningsBody = body.substring(marker.start + "screenings:".length)

        NuxtPage(
          movies           = parseNuxtMovies(movieBody, screeningsBody, resolve),
          showtimesByMovie = parseNuxtShowtimes(screeningsBody, resolve).groupMap(_._1)(_._2),
          embeddedRows     = parseEmbeddedNuxtRows(screeningsBody, resolve)
        )
    }
  }

  private def parseNuxtMovies(
                               movieBody: String,
                               screeningsBody: String,
                               resolve: String => Option[String]
                             ): Map[String, NuxtMovie] = {
    val normal =
      parseNormalNuxtMovies(movieBody, resolve)

    val embedded =
      parseEmbeddedScreeningNuxtMovies(screeningsBody, resolve)

    val events =
      parseEventNuxtMovies(movieBody, screeningsBody, resolve)

    normal ++ embedded ++ enrichEventNuxtMovies(events, embedded)
  }

  private def enrichEventNuxtMovies(
                                     events: Map[String, NuxtMovie],
                                     embedded: Map[String, NuxtMovie]
                                   ): Map[String, NuxtMovie] =
    events.map { case (id, eventMovie) =>
      val enriched =
        embedded.get(id).map { embeddedMovie =>
          eventMovie.copy(
            posterUrl      = eventMovie.posterUrl.orElse(embeddedMovie.posterUrl),
            runtimeMinutes = eventMovie.runtimeMinutes.orElse(embeddedMovie.runtimeMinutes)
          )
        }.getOrElse(eventMovie)

      id -> enriched
    }

  private def parseEmbeddedScreeningNuxtMovies(
      screeningsBody: String,
      resolve: String => Option[String]
  ): Map[String, NuxtMovie] = {
    val groupPattern =
      """([em]\d+):\{screenings:\[""".r

    groupPattern.findAllMatchIn(screeningsBody).flatMap { groupMatch =>
      val movieId = groupMatch.group(1)
      val screeningsArray =
        bracketedArrayContent(screeningsBody, groupMatch.end)

      """movie:\{(.*?)\},moviePrint:""".r
        .findFirstMatchIn(screeningsArray)
        .flatMap(m => parseNuxtEmbeddedMovieBlock(m.group(1), resolve).map(movieId -> _))
    }.toMap
  }

  private def parseNuxtEmbeddedMovieBlock(
      movie: String,
      resolve: String => Option[String]
  ): Option[NuxtMovie] = {
    val title =
      """title:(?:"([^"]+)"|(\w+))""".r
        .findFirstMatchIn(movie)
        .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))

    val slug =
      """slug:(?:"([^"]+)"|(\w+))""".r
        .findFirstMatchIn(movie)
        .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))

    val numericId =
      """id:(\d{3,}|\w+)""".r
        .findFirstMatchIn(movie)
        .flatMap { m =>
          val raw = m.group(1)
          if (raw.forall(_.isDigit)) Some(raw)
          else resolve(raw).filter(_.forall(_.isDigit))
        }

    val poster =
      """posterPhoto:\{.*?url:(?:"([^"]+)"|(\w+))""".r
        .findFirstMatchIn(movie)
        .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))
        .map(_.replace("\\u002F", "/"))

    val runtime =
      """duration:([^,}]+)""".r
        .findFirstMatchIn(movie)
        .flatMap(m => parseInt(m.group(1)).orElse(resolve(m.group(1)).flatMap(parseInt)))

    for {
      titleStr <- title
      slugStr  <- slug
    } yield NuxtMovie(
      title          = titleStr,
      slug           = slugStr,
      posterUrl      = poster,
      filmUrl        = numericId
        .map(id => s"$BaseUrl/filmy/$slugStr-$id")
        .orElse(Some(s"$BaseUrl/filmy/$slugStr")),
      runtimeMinutes = runtime
    )
  }

  private def parseNormalNuxtMovies(
                                     movieBody: String,
                                     resolve: String => Option[String]
                                   ): Map[String, NuxtMovie] = {
    val moviePattern =
      """,id:(\d{3,}|\w+),sourceId:(?:"([^"]+)"|(\w+)),title:(?:"([^"]+)"|(\w+)),titleOriginal:(?:"[^"]*"|\w+),slug:(?:"([^"]+)"|(\w+))""".r

    moviePattern.findAllMatchIn(movieBody).flatMap { m =>
      val rawId = m.group(1)

      val numericId =
        if (rawId.forall(_.isDigit)) rawId
        else resolve(rawId).filter(_.forall(_.isDigit)).getOrElse("")

      val title = Option(m.group(4)).orElse(resolve(m.group(5)))
      val slug  = Option(m.group(6)).orElse(resolve(m.group(7)))

      if (numericId.isEmpty) None
      else {
        val nearby =
          movieBody.substring(m.start, math.min(m.start + 1500, movieBody.length))

        val poster =
          """posterPhoto:\{filePath:(?:"[^"]+"|[^,{]+),url:(?:"([^"]+)"|(\w+))""".r
            .findFirstMatchIn(nearby)
            .flatMap(pm => Option(pm.group(1)).orElse(resolve(pm.group(2))))
            .map(_.replace("\\u002F", "/"))

        val runtime =
          """duration:([^,}]+)""".r
            .findFirstMatchIn(nearby)
            .flatMap(m => parseInt(m.group(1)).orElse(resolve(m.group(1)).flatMap(parseInt)))

        for {
          titleStr <- title
          slugStr  <- slug
        } yield s"m$numericId" -> NuxtMovie(
          title          = titleStr,
          slug           = slugStr,
          posterUrl      = poster,
          filmUrl        = Some(s"$BaseUrl/filmy/$slugStr-$numericId"),
          runtimeMinutes = runtime
        )
      }
    }.toMap
  }

  private def parseEventNuxtMovies(
                                    movieBody: String,
                                    screeningsBody: String,
                                    resolve: String => Option[String]
                                  ): Map[String, NuxtMovie] = {
    val eventIds =
      """([e]\d+):\{screenings:\[""".r
        .findAllMatchIn(screeningsBody)
        .map(_.group(1))
        .toSet

    eventIds.flatMap { eventId =>
      val numericId = eventId.stripPrefix("e")

      val eventMeta =
        findEventMetaInMovieBody(eventId, movieBody, resolve)
          .orElse(findEventMetaInScreeningsBody(eventId, screeningsBody, resolve))

      eventMeta.map { movie =>
        eventId -> movie.copy(
          filmUrl = movie.filmUrl.orElse(Some(s"$BaseUrl/wydarzenie/${movie.slug}-$numericId"))
        )
      }
    }.toMap
  }

  private def parseEmbeddedNuxtRows(
                                     screeningsBody: String,
                                     resolve: String => Option[String]
                                   ): Seq[NuxtMovieRow] = {
    val dayPattern   = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
    val groupPattern = """([em]\d+):\{screenings:\[""".r
    val timePattern  = """timeFrom:("[\d :.-]+"|[^,{]+)""".r
    val sourcePattern = """sourceId:("[\w-]+"|\w+)""".r
    val moviePattern = """movie:\{(.*?)\},moviePrint:""".r

    val dayMatches = dayPattern.findAllMatchIn(screeningsBody).toSeq

    dayMatches.zipWithIndex.flatMap { case (dayMatch, dayIndex) =>
      val dayBlockEnd =
        if (dayIndex + 1 < dayMatches.length) dayMatches(dayIndex + 1).start
        else screeningsBody.length

      val dayBlock =
        screeningsBody.substring(dayMatch.end, dayBlockEnd)

      groupPattern.findAllMatchIn(dayBlock).flatMap { groupMatch =>
        val movieId = groupMatch.group(1)
        val screeningsArray =
          bracketedArrayContent(dayBlock, groupMatch.end)

        splitNuxtScreeningObjects(screeningsArray).flatMap { screening =>
          val time =
            timePattern
              .findFirstMatchIn(screening)
              .flatMap(m => resolve(m.group(1)))

          val sourceId =
            sourcePattern.findFirstMatchIn(screening).flatMap { m =>
              val raw = m.group(1)
              if (raw.startsWith("\"")) Some(raw.stripPrefix("\"").stripSuffix("\""))
              else resolve(raw)
            }

          val movie =
            moviePattern
              .findFirstMatchIn(screening)
              .flatMap(m => parseNuxtEmbeddedMovieBlock(m.group(1), resolve))

          for {
            timeStr <- time if timeStr.length == 19
            sid     <- sourceId
            m       <- movie
          } yield NuxtMovieRow(
            movieId     = movieId,
            movie       = m,
            dateTime    = LocalDateTime.parse(timeStr.replace(' ', 'T')),
            screeningId = sid
          )
        }
      }
    }
  }

  private def findEventMetaInMovieBody(
                                        eventId: String,
                                        movieBody: String,
                                        resolve: String => Option[String]
                                      ): Option[NuxtMovie] = {
    val eventIdPattern = s"""_id:"$eventId"""".r

    eventIdPattern.findFirstMatchIn(movieBody).flatMap { m =>
      val before =
        movieBody.substring(math.max(0, m.start - 1000), m.start)

      """,name:(\w+),slug:(\w+),""".r
        .findAllMatchIn(before)
        .toSeq
        .lastOption
        .flatMap { nameMatch =>
          for {
            title <- resolve(nameMatch.group(1))
            slug  <- resolve(nameMatch.group(2))
          } yield NuxtMovie(
            title          = title,
            slug           = slug,
            posterUrl      = None,
            filmUrl        = None,
            runtimeMinutes = None
          )
        }
    }
  }

  private def findEventMetaInScreeningsBody(
                                             eventId: String,
                                             screeningsBody: String,
                                             resolve: String => Option[String]
                                           ): Option[NuxtMovie] = {
    val eventStart = screeningsBody.indexOf(s"$eventId:{")
    if (eventStart < 0) return None

    val eventBlock =
      screeningsBody.substring(eventStart, math.min(eventStart + 4000, screeningsBody.length))

    val movieBlock =
      """screeningMovies:\[\{.*?movie:\{(.*?)\},moviePrint:""".r
        .findFirstMatchIn(eventBlock)
        .map(_.group(1))

    movieBlock.flatMap { movie =>
      val title =
        """title:(?:"([^"]+)"|(\w+))""".r
          .findFirstMatchIn(movie)
          .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))

      val slug =
        """slug:(?:"([^"]+)"|(\w+))""".r
          .findFirstMatchIn(movie)
          .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))

      val poster =
        """posterPhoto:\{.*?url:(?:"([^"]+)"|(\w+))""".r
          .findFirstMatchIn(movie)
          .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))
          .map(_.replace("\\u002F", "/"))

      val runtime =
        """duration:([^,}]+)""".r
          .findFirstMatchIn(movie)
          .flatMap(m => parseInt(m.group(1)).orElse(resolve(m.group(1)).flatMap(parseInt)))

      for {
        titleStr <- title
        slugStr  <- slug
      } yield NuxtMovie(
        title          = titleStr,
        slug           = slugStr,
        posterUrl      = poster,
        filmUrl        = Some(s"$BaseUrl/filmy/$slugStr"),
        runtimeMinutes = runtime
      )
    }
  }

  private def parseNuxtShowtimes(
                                  screeningsBody: String,
                                  resolve: String => Option[String]
                                ): Seq[(String, (LocalDateTime, String))] = {
    val dayPattern       = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
    val groupPattern     = """([em]\d+):\{screenings:\[""".r
    val screeningPattern = """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r

    val dayMatches = dayPattern.findAllMatchIn(screeningsBody).toSeq

    dayMatches.zipWithIndex.flatMap { case (dayMatch, dayIndex) =>
      val dayBlockEnd =
        if (dayIndex + 1 < dayMatches.length) dayMatches(dayIndex + 1).start
        else screeningsBody.length

      val dayBlock =
        screeningsBody.substring(dayMatch.end, dayBlockEnd)

      groupPattern.findAllMatchIn(dayBlock).flatMap { groupMatch =>
        val movieId = groupMatch.group(1)

        val screeningsArray =
          bracketedArrayContent(dayBlock, groupMatch.end)

        screeningPattern.findAllMatchIn(screeningsArray).flatMap { screeningMatch =>
          val time =
            resolve(screeningMatch.group(1))

          val rawSourceId =
            screeningMatch.group(2)

          val sourceId =
            if (rawSourceId.startsWith("\""))
              Some(rawSourceId.stripPrefix("\"").stripSuffix("\""))
            else
              resolve(rawSourceId)

          for {
            timeStr <- time if timeStr.length == 19
            sid     <- sourceId
          } yield movieId -> (LocalDateTime.parse(timeStr.replace(' ', 'T')), sid)
        }
      }
    }
  }

  private def splitNuxtScreeningObjects(screeningsArray: String): Seq[String] = {
    val starts =
      """\{timeFrom:""".r
        .findAllMatchIn(screeningsArray)
        .map(_.start)
        .toSeq

    starts.zipWithIndex.map { case (start, index) =>
      val end =
        if (index + 1 < starts.length) starts(index + 1)
        else screeningsArray.length

      screeningsArray.substring(start, end)
    }
  }

  private def bracketedArrayContent(text: String, arrayStart: Int): String = {
    var depth = 1
    var pos   = arrayStart

    while (pos < text.length && depth > 0) {
      if (text(pos) == '[') depth += 1
      else if (text(pos) == ']') depth -= 1
      pos += 1
    }

    text.substring(arrayStart, math.max(arrayStart, pos - 1))
  }
}