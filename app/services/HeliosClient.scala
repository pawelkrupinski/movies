package clients

import models.{CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._
import tools.{HeliosFetch, HttpFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.mutable
import scala.util.Try

class HeliosClient(http: HttpFetch = HeliosFetch) {

  private val PageUrl = "https://helios.pl/poznan/kino-helios/repertuar"
  private val CinemaSourceId = "815face9-2a1d-4c62-9b2f-a361574b79a2"
  private val BaseUrl = "https://helios.pl/poznan/kino-helios"
  private val BookingBase = "https://bilety.helios.pl/screen"
  private val ApiBase = "https://restapi.helios.pl/api"
  private val WarsawZone = ZoneId.of("Europe/Warsaw")
  private val OffsetDtf = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  // ── Fetch ─────────────────────────────────────────────────────────────────

  def fetch(): Seq[CinemaMovie] = {
    val today = LocalDate.now(WarsawZone)
    val in6 = today.plusDays(6)

    val screeningsUrl = s"$ApiBase/cinema/$CinemaSourceId/screening" +
      s"?dateTimeFrom=${today}T00:00:00&dateTimeTo=${in6}T23:59:59"
    val apiScreeningsFuture = http.getAsync(screeningsUrl)

    val nuxtHtml = http.get(PageUrl)
    val screeningsBody = Try(apiScreeningsFuture.join()).getOrElse("[]")

    val apiScreeningsById = parseApiScreenings(screeningsBody)
    val uniqueMovieIds = apiScreeningsById.values.map(_.movieId).toSeq.distinct
    val uniqueScreenIds = apiScreeningsById.values.map(_.screenId).toSeq.distinct

    val pendingMovies = uniqueMovieIds.map(id => id -> http.getAsync(s"$ApiBase/movie/$id"))
    val pendingScreens = uniqueScreenIds.map(id => id -> http.getAsync(s"$ApiBase/cinema/$CinemaSourceId/screen/$id"))

    val movieBodies: Map[String, String] = pendingMovies.flatMap { case (id, f) =>
      Try(id -> f.join()).toOption
    }.toMap

    val screenBodies: Map[String, String] = pendingScreens.flatMap { case (id, f) =>
      Try(id -> f.join()).toOption
    }.toMap

    buildCinemaMovies(nuxtHtml, screeningsBody, movieBodies, screenBodies)
  }

  // ── Pure assembly ─────────────────────────────────────────────────────────

  private def buildCinemaMovies(
                                 nuxtHtml: String,
                                 screeningsBody: String,
                                 movieBodies: Map[String, String],
                                 screenBodies: Map[String, String]
                               ): Seq[CinemaMovie] = {
    val (movieInfoMap, showtimesByMovie) = parseRepertoirePage(nuxtHtml)
    val apiScreeningsById = parseApiScreenings(screeningsBody)

    val movieDetails: Map[String, ApiMovieInfo] = movieBodies.flatMap { case (id, body) =>
      parseApiMovieBody(body).map(id -> _)
    }

    val screenNames: Map[String, String] = screenBodies.flatMap { case (id, body) =>
      Try((Json.parse(body) \ "name").asOpt[String]).toOption.flatten.map(id -> _)
    }

    showtimesByMovie.toSeq.flatMap { case (nuxtMovieId, slots) =>
      movieInfoMap.get(nuxtMovieId).map { info =>
        val apiMovieIdFromScreening: Option[String] =
          slots
            .flatMap(_._2)
            .flatMap(screeningIdFromUrl)
            .flatMap(apiScreeningsById.get)
            .map(_.movieId)
            .headOption

        val apiMovie =
          apiMovieIdFromScreening
            .orElse(info.apiMovieId)
            .flatMap(movieDetails.get)

        val cleanTitle = info.title
          .stripSuffix(" w Helios RePlay")
          .stripSuffix(" w Helios Anime")
          .stripSuffix(" w Helios na Scenie")
          .stripSuffix(" - Salon Kultury Helios")
          .stripSuffix(" - KNTJ")
          .stripSuffix(" - KNT")

        CinemaMovie(
          movie = Movie(
            title = cleanTitle,
            runtimeMinutes = apiMovie.flatMap(_.duration).orElse(info.runtimeMinutes),
            releaseYear = apiMovie.flatMap(_.year),
            premierePl = apiMovie.flatMap(_.premierePl),
            premiereWorld = apiMovie.flatMap(_.premiereWorld)
          ),
          cinema = Helios,
          posterUrl = apiMovie.flatMap(_.posterUrl).orElse(info.posterUrl),
          filmUrl = info.filmUrl,
          synopsis = apiMovie.flatMap(_.description),
          cast = apiMovie.flatMap(_.cast),
          director = apiMovie.flatMap(_.director),
          showtimes = slots.sortBy(_._1).map { case (dateTime, bookingUrl) =>
            val sc = bookingUrl.flatMap(screeningIdFromUrl).flatMap(apiScreeningsById.get)
            Showtime(
              dateTime = dateTime,
              bookingUrl = bookingUrl,
              room = sc.map(_.screenId).flatMap(screenNames.get),
              format = sc.map(_.release).filter(_.nonEmpty)
            )
          }
        )
      }
    }
  }

  // ── Billing-API types and parsers ─────────────────────────────────────────

  private case class ApiScreening(movieId: String, screenId: String, release: String)

  private def parseApiScreenings(body: String): Map[String, ApiScreening] =
    Try(Json.parse(body).as[JsArray]).map { arr =>
      arr.value.flatMap { s =>
        for {
          id <- (s \ "id").asOpt[String]
          movieId <- (s \ "movieId").asOpt[String]
          screenId <- (s \ "screenId").asOpt[String]
        } yield {
          val release = (s \ "release").asOpt[String].getOrElse("")
          id -> ApiScreening(movieId, screenId, release)
        }
      }.toMap
    }.getOrElse(Map.empty)

  // Extracts the screening UUID from https://bilety.helios.pl/screen/<UUID>?cinemaId=...
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
    Try(Json.parse(body)).toOption.map { js =>
      val premierePl = (js \ "premiereDate").asOpt[String]
        .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption)
      val premiereWorld = (js \ "worldPremiereDate").asOpt[String]
        .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption)
      val poster = (js \ "posters").asOpt[JsArray]
        .flatMap(_.value.headOption)
        .flatMap(_.asOpt[String])
        .filter(_.startsWith("http"))
      val duration =
        (js \ "duration").asOpt[Int]
          .orElse((js \ "duration").asOpt[String].flatMap(s => Try(s.toInt).toOption))
      ApiMovieInfo(
        duration = duration,
        description = (js \ "description").asOpt[String].filter(_.nonEmpty),
        cast = (js \ "filmCast").asOpt[String].filter(_.nonEmpty),
        director = (js \ "director").asOpt[String].filter(_.nonEmpty),
        year = (js \ "yearOfProduction").asOpt[String].flatMap(s => Try(s.take(4).toInt).toOption),
        premierePl = premierePl,
        premiereWorld = premiereWorld,
        posterUrl = poster
      )
    }

  // ── Repertoire page ───────────────────────────────────────────────────────

  private val ScreeningsSectionPat = """screenings:\{"\d{4}-\d{2}-\d{2}"""".r

  private def parseRepertoirePage(html: String): (Map[String, MovieMeta], Map[String, Seq[(LocalDateTime, Option[String])]]) = {
    val nuxtIndex = html.lastIndexOf("window.__NUXT__")
    if (nuxtIndex < 0) return (Map.empty, Map.empty)
    val nuxtScript = html.substring(nuxtIndex)

    val paramsStart = nuxtScript.indexOf("(function(") + "(function(".length
    val paramsEnd = nuxtScript.indexOf("){", paramsStart)
    val bodyEnd = nuxtScript.indexOf("}(")
    if (paramsEnd < 0 || bodyEnd < 0) return (Map.empty, Map.empty)

    val paramNames = nuxtScript.substring(paramsStart, paramsEnd).split(",").toSeq
    val valuesRaw = nuxtScript.substring(bodyEnd + 2)
    val valuesEnd = valuesRaw.lastIndexOf("))")

    val paramMap: Map[String, JsValue] = Try {
      val raw = valuesRaw.substring(0, valuesEnd)
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
          case _ => None
        }
      raw.map(_.replace("\\u002F", "/"))
    }

    val body = nuxtScript.substring(paramsEnd + 2, bodyEnd)

    val screeningsMatch = ScreeningsSectionPat.findFirstMatchIn(body).getOrElse(return (Map.empty, Map.empty))
    val movieMetaBody = body.substring(0, screeningsMatch.start)
    val screeningsBody = body.substring(screeningsMatch.start + "screenings:".length)

    val movieInfoMap =

      parseMovieInfo(movieMetaBody, resolve) ++ parseNestedScreeningMovies(screeningsBody, resolve)
    val showtimesByMovie = parseScreenings(screeningsBody, resolve).groupMap(_._1)(_._2)

    (movieInfoMap, showtimesByMovie)
  }

  private def parseNestedScreeningMovies(
                                          screeningsBody: String,
                                          resolve: String => Option[String]
                                        ): Map[String, MovieMeta] = {
    val result = mutable.Map[String, MovieMeta]()

    def resolveInt(token: String): Option[Int] = {
      val t = token.trim
      if (t.forall(_.isDigit)) Some(t.toInt)
      else resolve(t).flatMap(s => Try(s.toInt).toOption)
    }

    val eventBlockPattern =
      """(e\d+):\{screenings:\[.*?screeningMovies:\[\{.*?movie:\{(.*?)\},moviePrint:""".r

    val sourceIdPattern = """sourceId:(?:"([^"]+)"|(\w+))""".r
    val titlePattern = """title:(?:"([^"]+)"|(\w+))""".r
    val slugPattern = """slug:(?:"([^"]+)"|(\w+))""".r
    val durationPattern = """duration:([^,}]+)""".r
    val posterPattern = """posterPhoto:\{.*?url:(?:"([^"]+)"|(\w+))""".r

    for (m <- eventBlockPattern.findAllMatchIn(screeningsBody)) {
      val eventId = m.group(1)
      val movie = m.group(2)

      val apiMovieId = sourceIdPattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2))))
      val title = titlePattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2))))
      val slug = slugPattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2))))
      val duration = durationPattern.findFirstMatchIn(movie).flatMap(x => resolveInt(x.group(1)))
      val posterUrl = posterPattern.findFirstMatchIn(movie).flatMap(x => Option(x.group(1)).orElse(resolve(x.group(2)))).map(_.replace("\\u002F", "/"))

      for {
        titleStr <- title
        slugStr <- slug
      } result(eventId) = MovieMeta(
        title = titleStr,
        slug = slugStr,
        posterUrl = posterUrl,
        filmUrl = Some(s"$BaseUrl/filmy/$slugStr"),
        apiMovieId = apiMovieId,
        runtimeMinutes = duration
      )
    }

    result.toMap
  }

  // ── Movie metadata from repertoire NUXT ──────────────────────────────────

  private case class MovieMeta(title: String,
                               slug: String,
                               posterUrl: Option[String],
                               filmUrl: Option[String],
                               apiMovieId: Option[String],
                               runtimeMinutes: Option[Int] = None
                              )

  private def parseMovieInfo(movieMetaBody: String, resolve: String => Option[String]): Map[String, MovieMeta] = {
    val result = mutable.Map[String, MovieMeta]()
    def resolveInt(token: String): Option[Int] = {
      val t = token.trim
      if (t.forall(_.isDigit)) Some(t.toInt)
      else resolve(t).flatMap(s => Try(s.toInt).toOption)
    }

    val moviePattern = """,id:(\d{3,}|\w+),sourceId:(?:"([^"]+)"|(\w+)),title:(?:"([^"]+)"|(\w+)),titleOriginal:(?:"[^"]+"|(?:\w+)),slug:(?:"([^"]+)"|(\w+))""".r
    for (movieMatch <- moviePattern.findAllMatchIn(movieMetaBody)) {
      val rawId = movieMatch.group(1)
      val numericId = if (rawId.forall(_.isDigit)) rawId
      else resolve(rawId).filter(_.forall(_.isDigit)).getOrElse("")
      val apiMovieId = Option(movieMatch.group(2)).orElse(resolve(movieMatch.group(3))).getOrElse("")
      val title = Option(movieMatch.group(4)).orElse(resolve(movieMatch.group(5)))
      val slug = Option(movieMatch.group(6)).orElse(resolve(movieMatch.group(7))).getOrElse("")
      if (numericId.nonEmpty && apiMovieId.nonEmpty && slug.nonEmpty) {
        val nearby = movieMetaBody.substring(movieMatch.start, math.min(movieMatch.start + 1000, movieMetaBody.length))
        val posterUrl =
          """posterPhoto:\{filePath:(?:"[^"]+"|[^,{]+),url:(?:"([^"]+)"|(\w+))""".r
            .findFirstMatchIn(nearby).flatMap { pm =>
              Option(pm.group(1)).map(_.replace("\\u002F", "/")).orElse(resolve(pm.group(2)))
            }
        val duration =
          """duration:([^,}]+)""".r
            .findFirstMatchIn(nearby)
            .flatMap(m => resolveInt(m.group(1)))

        title.foreach { titleStr =>
          result(s"m$numericId") = MovieMeta(
            title          = titleStr,
            slug           = slug,
            posterUrl      = posterUrl,
            filmUrl        = Some(s"$BaseUrl/filmy/$slug-$numericId"),
            apiMovieId     = Some(apiMovieId),
            runtimeMinutes = duration
          )
        }
      }
    }

    val eventIdPattern = """_id:"(e\d+)"""".r
    for (eventMatch <- eventIdPattern.findAllMatchIn(movieMetaBody)) {
      val eventId = eventMatch.group(1)
      val numericId = eventId.substring(1)
      val before = movieMetaBody.substring(math.max(0, eventMatch.start - 500), eventMatch.start)

      for (nameMatch <- """,name:(\w+),slug:(\w+),""".r.findAllMatchIn(before).toSeq.lastOption) {
        val title = resolve(nameMatch.group(1))
        val slug = resolve(nameMatch.group(2))

        for {
          titleStr <- title
          slugStr <- slug
        } result(eventId) = MovieMeta(
          title = titleStr,
          slug = slugStr,
          posterUrl = None,
          filmUrl = Some(s"$BaseUrl/wydarzenie/$slugStr-$numericId"),
          apiMovieId = None
        )
      }
    }

    result.toMap
  }

  // ── Screenings from repertoire NUXT ──────────────────────────────────────

  private def parseScreenings(screeningsBody: String, resolve: String => Option[String]): Seq[(String, (LocalDateTime, Option[String]))] = {
    val result = mutable.ListBuffer[(String, (LocalDateTime, Option[String]))]()
    val dayPattern = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
    val dayMatches = dayPattern.findAllMatchIn(screeningsBody).toSeq

    for ((dayMatch, dayIndex) <- dayMatches.zipWithIndex) {
      val dayBlockEnd = if (dayIndex + 1 < dayMatches.length) dayMatches(dayIndex + 1).start else screeningsBody.length
      val dayBlock = screeningsBody.substring(dayMatch.end, dayBlockEnd)

      for (groupMatch <- """([em]\d+):\{screenings:\[""".r.findAllMatchIn(dayBlock)) {
        val movieId = groupMatch.group(1)

        var depth = 1;
        var pos = groupMatch.end
        while (pos < dayBlock.length && depth > 0) {
          if (dayBlock(pos) == '[') depth += 1
          else if (dayBlock(pos) == ']') depth -= 1
          pos += 1
        }
        val screeningsArray = dayBlock.substring(groupMatch.end, pos - 1)

        val screeningPattern = """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r
        for (screeningMatch <- screeningPattern.findAllMatchIn(screeningsArray)) {
          val timeValue = resolve(screeningMatch.group(1))
          val rawSourceId = screeningMatch.group(2)
          val sourceId = if (rawSourceId.startsWith("\"")) Some(rawSourceId.stripPrefix("\"").stripSuffix("\""))
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
