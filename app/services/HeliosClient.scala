package clients

import models.{CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._
import tools.{HeliosFetch, HttpFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.mutable
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

    val screeningsFuture = http.getAsync(screeningsUrl)

    val nuxtHtml       = http.get(PageUrl)
    val screeningsBody = Try(screeningsFuture.join()).getOrElse("[]")

    val apiScreeningsById = parseApiScreenings(screeningsBody)

    val movieBodies =
      fetchBodies(apiScreeningsById.values.map(_.movieId).toSeq.distinct)(id => s"$ApiBase/movie/$id")

    val screenBodies =
      fetchBodies(apiScreeningsById.values.map(_.screenId).toSeq.distinct)(id =>
        s"$ApiBase/cinema/$CinemaSourceId/screen/$id"
      )

    buildCinemaMovies(nuxtHtml, apiScreeningsById, movieBodies, screenBodies)
  }

  private def fetchBodies(ids: Seq[String])(urlFor: String => String): Map[String, String] =
    ids
      .map(id => id -> http.getAsync(urlFor(id)))
      .flatMap { case (id, future) => Try(id -> future.join()).toOption }
      .toMap

  private def buildCinemaMovies(
                                 nuxtHtml: String,
                                 apiScreeningsById: Map[String, ApiScreening],
                                 movieBodies: Map[String, String],
                                 screenBodies: Map[String, String]
                               ): Seq[CinemaMovie] = {
    val (movieInfoMap, showtimesByMovie) = parseRepertoirePage(nuxtHtml)

    val movieDetails: Map[String, ApiMovieInfo] = movieBodies.flatMap { case (id, body) =>
      parseApiMovieBody(body).map(id -> _)
    }

    val screenNames: Map[String, String] = screenBodies.flatMap { case (id, body) =>
      json(body).flatMap(js => (js \ "name").asOpt[String]).map(id -> _)
    }

    val moviesWithKeys: Seq[(String, CinemaMovie)] =
      showtimesByMovie.toSeq.flatMap { case (nuxtMovieId, slots) =>
        movieInfoMap.get(nuxtMovieId).map { info =>
          val apiMovieId = apiMovieIdFor(info, slots, apiScreeningsById)
          val apiMovie   = apiMovieId.flatMap(movieDetails.get)
          val movieKey   = apiMovieId.getOrElse(nuxtMovieId)

          movieKey -> toCinemaMovie(
            info              = info,
            apiMovie          = apiMovie,
            slots             = slots,
            apiScreeningsById = apiScreeningsById,
            screenNames       = screenNames
          )
        }
      }

    mergeDuplicateMovies(moviesWithKeys)
  }

  private def mergeDuplicateMovies(moviesWithKeys: Seq[(String, CinemaMovie)]): Seq[CinemaMovie] =
    moviesWithKeys
      .groupMap(_._1)(_._2)
      .values
      .map { sameMovieRows =>
        val first = sameMovieRows.head
        first.copy(
          posterUrl = sameMovieRows.flatMap(_.posterUrl).headOption,
          filmUrl = sameMovieRows.flatMap(_.filmUrl).headOption,
          synopsis = sameMovieRows.flatMap(_.synopsis).headOption,
          cast = sameMovieRows.flatMap(_.cast).headOption,
          director = sameMovieRows.flatMap(_.director).headOption,
          showtimes = sameMovieRows.flatMap(_.showtimes).distinct.sortBy(_.dateTime)
        )
      }
      .toSeq
      .sortBy(_.movie.title)

  private def apiMovieIdFor(
                             info: MovieMeta,
                             slots: Seq[(LocalDateTime, Option[String])],
                             apiScreeningsById: Map[String, ApiScreening]
                           ): Option[String] =
    slots
      .flatMap(_._2)
      .flatMap(screeningIdFromUrl)
      .flatMap(apiScreeningsById.get)
      .map(_.movieId)
      .headOption
      .orElse(info.apiMovieId)

  private def toCinemaMovie(
                             info: MovieMeta,
                             apiMovie: Option[ApiMovieInfo],
                             slots: Seq[(LocalDateTime, Option[String])],
                             apiScreeningsById: Map[String, ApiScreening],
                             screenNames: Map[String, String]
                           ): CinemaMovie =
    CinemaMovie(
      movie = Movie(
        title          = cleanTitle(info.title),
        runtimeMinutes = apiMovie.flatMap(_.duration).orElse(info.runtimeMinutes),
        releaseYear    = apiMovie.flatMap(_.year),
        premierePl     = apiMovie.flatMap(_.premierePl),
        premiereWorld  = apiMovie.flatMap(_.premiereWorld)
      ),
      cinema    = Helios,
      posterUrl = apiMovie.flatMap(_.posterUrl).orElse(info.posterUrl),
      filmUrl   = info.filmUrl,
      synopsis  = apiMovie.flatMap(_.description),
      cast      = apiMovie.flatMap(_.cast),
      director  = apiMovie.flatMap(_.director),
      showtimes = slots.sortBy(_._1).map { case (dateTime, bookingUrl) =>
        toShowtime(dateTime, bookingUrl, apiScreeningsById, screenNames)
      }
    )

  private def toShowtime(
                          dateTime: LocalDateTime,
                          bookingUrl: Option[String],
                          apiScreeningsById: Map[String, ApiScreening],
                          screenNames: Map[String, String]
                        ): Showtime = {
    val screening =
      bookingUrl
        .flatMap(screeningIdFromUrl)
        .flatMap(apiScreeningsById.get)

    Showtime(
      dateTime   = dateTime,
      bookingUrl = bookingUrl,
      room       = screening.map(_.screenId).flatMap(screenNames.get),
      format     = screening.map(_.release).filter(_.nonEmpty)
    )
  }

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

  private def resolveInt(token: String, resolve: String => Option[String]): Option[Int] =
    parseInt(token).orElse(resolve(token).flatMap(parseInt))

  private case class ApiScreening(movieId: String, screenId: String, release: String)

  private def parseApiScreenings(body: String): Map[String, ApiScreening] =
    json(body).map {
      case JsArray(values) =>
        values.flatMap { s =>
          for {
            id       <- (s \ "id").asOpt[String]
            movieId  <- (s \ "movieId").asOpt[String]
            screenId <- (s \ "screenId").asOpt[String]
          } yield {
            val release = (s \ "release").asOpt[String].getOrElse("")
            id -> ApiScreening(movieId, screenId, release)
          }
        }.toMap
      case _ => Map.empty[String, ApiScreening]
    }.getOrElse(Map.empty)

  private def screeningIdFromUrl(url: String): Option[String] = {
    val prefix = BookingBase + "/"
    if (url.startsWith(prefix)) Some(url.stripPrefix(prefix).takeWhile(_ != '?'))
    else None
  }

  private case class ApiMovieInfo(
                                   duration: Option[Int],
                                   description: Option[String],
                                   cast: Option[String],
                                   director: Option[String],
                                   year: Option[Int],
                                   premierePl: Option[java.time.LocalDate],
                                   premiereWorld: Option[java.time.LocalDate],
                                   posterUrl: Option[String]
                                 )

  private def parseApiMovieBody(body: String): Option[ApiMovieInfo] =
    json(body).map { js =>
      val premierePl = (js \ "premiereDate").asOpt[String]
        .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption)
      val premiereWorld = (js \ "worldPremiereDate").asOpt[String]
        .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption)
      val poster = (js \ "posters").asOpt[JsArray]
        .flatMap(_.value.headOption)
        .flatMap {
          case JsString(url) => Some(url)
          case obj: JsObject => (obj \ "url").asOpt[String]
          case _             => None
        }
        .filter(_.startsWith("http"))
      val duration =
        (js \ "duration").asOpt[Int]
          .orElse((js \ "duration").asOpt[String].flatMap(parseInt))

      ApiMovieInfo(
        duration      = duration,
        description   = (js \ "description").asOpt[String].filter(_.nonEmpty),
        cast          = (js \ "filmCast").asOpt[String].filter(_.nonEmpty),
        director      = (js \ "director").asOpt[String].filter(_.nonEmpty),
        year          = (js \ "yearOfProduction").asOpt[String].flatMap(s => parseInt(s.take(4))),
        premierePl    = premierePl,
        premiereWorld = premiereWorld,
        posterUrl     = poster
      )
    }

  private val ScreeningsSectionPat = """screenings:\{"\d{4}-\d{2}-\d{2}"""".r

  private def parseRepertoirePage(
                                   html: String
                                 ): (Map[String, MovieMeta], Map[String, Seq[(LocalDateTime, Option[String])]]) = {
    val nuxtIndex = html.lastIndexOf("window.__NUXT__")
    if (nuxtIndex < 0) return (Map.empty, Map.empty)

    val nuxtScript = html.substring(nuxtIndex)
    val paramsStart = nuxtScript.indexOf("(function(") + "(function(".length
    val paramsEnd   = nuxtScript.indexOf("){", paramsStart)
    val bodyEnd     = nuxtScript.indexOf("}(")
    if (paramsEnd < 0 || bodyEnd < 0) return (Map.empty, Map.empty)

    val paramNames = nuxtScript.substring(paramsStart, paramsEnd).split(",").toSeq
    val valuesRaw  = nuxtScript.substring(bodyEnd + 2)
    val valuesEnd  = valuesRaw.lastIndexOf("))")

    val paramMap: Map[String, JsValue] = Try {
      val raw   = valuesRaw.substring(0, valuesEnd)
      val clean = raw.replaceAll("""Array\(\d+\)""", "null").replace("undefined", "null")
      val valuesArray = Json.parse("[" + clean + "]").as[JsArray]
      paramNames.zip(valuesArray.value).toMap
    }.getOrElse(return (Map.empty, Map.empty))

    def resolve(token: String): Option[String] = {
      val trimmed = token.trim
      val raw =
        if (trimmed.startsWith("\"")) Some(trimmed.stripPrefix("\"").stripSuffix("\""))
        else paramMap.get(trimmed).flatMap {
          case JsString(s) => Some(s)
          case n: JsNumber => n.value.toBigIntExact.map(_.toString)
          case _           => None
        }
      raw.map(_.replace("\\u002F", "/"))
    }

    val body = nuxtScript.substring(paramsEnd + 2, bodyEnd)
    val screeningsMatch =
      ScreeningsSectionPat.findFirstMatchIn(body).getOrElse(return (Map.empty, Map.empty))

    val movieMetaBody  = body.substring(0, screeningsMatch.start)
    val screeningsBody = body.substring(screeningsMatch.start + "screenings:".length)

    val movieInfoMap =
      parseMovieInfo(movieMetaBody, resolve) ++ parseNestedScreeningMovies(screeningsBody, resolve)

    val showtimesByMovie =
      parseScreenings(screeningsBody, resolve).groupMap(_._1)(_._2)

    (movieInfoMap, showtimesByMovie)
  }

  private case class MovieMeta(
                                title: String,
                                slug: String,
                                posterUrl: Option[String],
                                filmUrl: Option[String],
                                apiMovieId: Option[String],
                                runtimeMinutes: Option[Int] = None
                              )

  private def parseNestedScreeningMovies(
                                          screeningsBody: String,
                                          resolve: String => Option[String]
                                        ): Map[String, MovieMeta] = {
    val eventBlockPattern =
      """(e\d+):\{screenings:\[.*?screeningMovies:\[\{.*?movie:\{(.*?)\},moviePrint:""".r

    val sourceIdPattern = """sourceId:(?:"([^"]+)"|(\w+))""".r
    val titlePattern    = """title:(?:"([^"]+)"|(\w+))""".r
    val slugPattern     = """slug:(?:"([^"]+)"|(\w+))""".r
    val durationPattern = """duration:([^,}]+)""".r
    val posterPattern   = """posterPhoto:\{.*?url:(?:"([^"]+)"|(\w+))""".r

    eventBlockPattern.findAllMatchIn(screeningsBody).flatMap { m =>
      val eventId = m.group(1)
      val movie   = m.group(2)

      val apiMovieId = sourceIdPattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2))))
      val title      = titlePattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2))))
      val slug       = slugPattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2))))
      val duration   = durationPattern.findFirstMatchIn(movie).flatMap(x => resolveInt(x.group(1), resolve))
      val posterUrl  = posterPattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2)))).map(_.replace("\\u002F", "/"))

      for {
        titleStr <- title
        slugStr  <- slug
      } yield eventId -> MovieMeta(
        title          = titleStr,
        slug           = slugStr,
        posterUrl      = posterUrl,
        filmUrl        = Some(s"$BaseUrl/filmy/$slugStr"),
        apiMovieId     = apiMovieId,
        runtimeMinutes = duration
      )
    }.toMap
  }

  private def parseMovieInfo(movieMetaBody: String, resolve: String => Option[String]): Map[String, MovieMeta] = {
    val moviePattern =
      """,id:(\d{3,}|\w+),sourceId:(?:"([^"]+)"|(\w+)),title:(?:"([^"]+)"|(\w+)),titleOriginal:(?:"[^"]+"|(?:\w+)),slug:(?:"([^"]+)"|(\w+))""".r

    val movies = moviePattern.findAllMatchIn(movieMetaBody).flatMap { movieMatch =>
      val rawId = movieMatch.group(1)
      val numericId =
        if (rawId.forall(_.isDigit)) rawId
        else resolve(rawId).filter(_.forall(_.isDigit)).getOrElse("")

      val apiMovieId = Option(movieMatch.group(2)).orElse(resolve(movieMatch.group(3))).getOrElse("")
      val title      = Option(movieMatch.group(4)).orElse(resolve(movieMatch.group(5)))
      val slug       = Option(movieMatch.group(6)).orElse(resolve(movieMatch.group(7))).getOrElse("")

      if (numericId.nonEmpty && apiMovieId.nonEmpty && slug.nonEmpty) {
        val nearby = movieMetaBody.substring(movieMatch.start, math.min(movieMatch.start + 1000, movieMetaBody.length))

        val posterUrl =
          """posterPhoto:\{filePath:(?:"[^"]+"|[^,{]+),url:(?:"([^"]+)"|(\w+))""".r
            .findFirstMatchIn(nearby)
            .flatMap(pm => Option(pm.group(1)).map(_.replace("\\u002F", "/")).orElse(resolve(pm.group(2))))

        val duration =
          """duration:([^,}]+)""".r
            .findFirstMatchIn(nearby)
            .flatMap(m => resolveInt(m.group(1), resolve))

        title.map { titleStr =>
          s"m$numericId" -> MovieMeta(
            title          = titleStr,
            slug           = slug,
            posterUrl      = posterUrl,
            filmUrl        = Some(s"$BaseUrl/filmy/$slug-$numericId"),
            apiMovieId     = Some(apiMovieId),
            runtimeMinutes = duration
          )
        }
      } else None
    }

    val eventIdPattern = """_id:"(e\d+)"""".r
    val events = eventIdPattern.findAllMatchIn(movieMetaBody).flatMap { eventMatch =>
      val eventId   = eventMatch.group(1)
      val numericId = eventId.substring(1)
      val before    = movieMetaBody.substring(math.max(0, eventMatch.start - 500), eventMatch.start)

      """,name:(\w+),slug:(\w+),""".r.findAllMatchIn(before).toSeq.lastOption.flatMap { nameMatch =>
        for {
          titleStr <- resolve(nameMatch.group(1))
          slugStr  <- resolve(nameMatch.group(2))
        } yield eventId -> MovieMeta(
          title      = titleStr,
          slug       = slugStr,
          posterUrl  = None,
          filmUrl    = Some(s"$BaseUrl/wydarzenie/$slugStr-$numericId"),
          apiMovieId = None
        )
      }
    }

    (movies ++ events).toMap
  }

  private def parseScreenings(
                               screeningsBody: String,
                               resolve: String => Option[String]
                             ): Seq[(String, (LocalDateTime, Option[String]))] = {
    val result     = mutable.ListBuffer[(String, (LocalDateTime, Option[String]))]()
    val dayPattern = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
    val dayMatches = dayPattern.findAllMatchIn(screeningsBody).toSeq

    for ((dayMatch, dayIndex) <- dayMatches.zipWithIndex) {
      val dayBlockEnd =
        if (dayIndex + 1 < dayMatches.length) dayMatches(dayIndex + 1).start
        else screeningsBody.length

      val dayBlock = screeningsBody.substring(dayMatch.end, dayBlockEnd)

      for (groupMatch <- """([em]\d+):\{screenings:\[""".r.findAllMatchIn(dayBlock)) {
        val movieId = groupMatch.group(1)

        var depth = 1
        var pos   = groupMatch.end

        while (pos < dayBlock.length && depth > 0) {
          if (dayBlock(pos) == '[') depth += 1
          else if (dayBlock(pos) == ']') depth -= 1
          pos += 1
        }

        val screeningsArray = dayBlock.substring(groupMatch.end, pos - 1)
        val screeningPattern =
          """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r

        for (screeningMatch <- screeningPattern.findAllMatchIn(screeningsArray)) {
          val timeValue   = resolve(screeningMatch.group(1))
          val rawSourceId = screeningMatch.group(2)
          val sourceId =
            if (rawSourceId.startsWith("\"")) Some(rawSourceId.stripPrefix("\"").stripSuffix("\""))
            else resolve(rawSourceId)

          for {
            timeStr <- timeValue if timeStr.length == 19
            dateTime = LocalDateTime.parse(timeStr.replace(' ', 'T'))
            bookingUrl = sourceId.map(sid => s"$BookingBase/$sid?cinemaId=$CinemaSourceId")
          } result += movieId -> (dateTime, bookingUrl)
        }
      }
    }

    result.toSeq
  }
}