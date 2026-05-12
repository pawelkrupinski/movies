package clients

import models.{CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.util.Try

object HeliosClient {

  private val PageUrl        = "https://helios.pl/poznan/kino-helios/repertuar"
  private val CinemaSourceId = "815face9-2a1d-4c62-9b2f-a361574b79a2"
  private val BaseUrl        = "https://helios.pl/poznan/kino-helios"
  private val BookingBase    = "https://bilety.helios.pl/screen"
  private val ApiBase        = "https://restapi.helios.pl/api"
  private val WarsawZone     = ZoneId.of("Europe/Warsaw")
  private val OffsetDtf      = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  private val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  private def buildRequest(url: String): HttpRequest =
    HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      .GET()
      .build()

  private def buildApiRequest(url: String): HttpRequest =
    HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "application/json, text/plain, */*")
      .header("Origin", "https://bilety.helios.pl")
      .GET()
      .build()

  // ── Fetch ─────────────────────────────────────────────────────────────────

  def fetch(): Seq[CinemaMovie] = {
    val today = LocalDate.now(WarsawZone)
    val in6   = today.plusDays(6)

    // Phase 1: NUXT page + billing-API screenings in parallel
    val nuxtFuture = httpClient.sendAsync(buildRequest(PageUrl), HttpResponse.BodyHandlers.ofString())
    val screeningsUrl = s"$ApiBase/cinema/$CinemaSourceId/screening" +
      s"?dateTimeFrom=${today}T00:00:00&dateTimeTo=${in6}T23:59:59"
    val apiScreeningsFuture = httpClient.sendAsync(buildApiRequest(screeningsUrl), HttpResponse.BodyHandlers.ofString())

    val nuxtResp = nuxtFuture.join()
    if (nuxtResp.statusCode() != 200)
      throw new RuntimeException(s"helios.pl returned ${nuxtResp.statusCode()}")
    val (movieInfoMap, showtimesByMovie) = parseRepertoirePage(nuxtResp.body())

    val apiScreenings: Map[LocalDateTime, ApiScreening] = Try {
      val r = apiScreeningsFuture.join()
      if (r.statusCode() == 200) parseApiScreenings(r.body()) else Map.empty[LocalDateTime, ApiScreening]
    }.getOrElse(Map.empty)

    // Phase 2: movie details + screen names in parallel (all sendAsync at once)
    val uniqueMovieIds  = apiScreenings.values.map(_.movieId).toSeq.distinct
    val uniqueScreenIds = apiScreenings.values.map(_.screenId).toSeq.distinct

    val pendingMovies = uniqueMovieIds.map { id =>
      id -> httpClient.sendAsync(buildApiRequest(s"$ApiBase/movie/$id"), HttpResponse.BodyHandlers.ofString())
    }
    val pendingScreens = uniqueScreenIds.map { id =>
      id -> httpClient.sendAsync(buildApiRequest(s"$ApiBase/cinema/$CinemaSourceId/screen/$id"), HttpResponse.BodyHandlers.ofString())
    }

    val movieDetails: Map[String, ApiMovieInfo] = pendingMovies.flatMap { case (id, f) =>
      Try(parseApiMovie(f.join())).toOption.flatten.map(id -> _)
    }.toMap

    val screenNames: Map[String, String] = pendingScreens.flatMap { case (id, f) =>
      Try {
        val r = f.join()
        if (r.statusCode() == 200) (Json.parse(r.body()) \ "name").asOpt[String].map(id -> _)
        else None
      }.toOption.flatten
    }.toMap

    // Build CinemaMovies
    showtimesByMovie.toSeq.flatMap { case (nuxtMovieId, slots) =>
      movieInfoMap.get(nuxtMovieId).map { info =>
        // Find the API movieId via the first showtime that matched a billing-API screening
        val apiMovieId = slots.sortBy(_._1).iterator
          .flatMap { case (dt, _) => apiScreenings.get(dt) }
          .nextOption()
          .map(_.movieId)
        val apiMovie = apiMovieId.flatMap(movieDetails.get)

        val cleanTitle = info.title
          .stripSuffix(" w Helios RePlay")
          .stripSuffix(" w Helios Anime")
          .stripSuffix(" w Helios na Scenie")
          .stripSuffix(" - Salon Kultury Helios")

        CinemaMovie(
          movie = Movie(
            title          = cleanTitle,
            runtimeMinutes = apiMovie.flatMap(_.duration),
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
            val sc = apiScreenings.get(dateTime)
            Showtime(
              dateTime   = dateTime,
              bookingUrl = bookingUrl,
              room       = sc.map(_.screenId).flatMap(screenNames.get),
              format     = sc.map(_.release).filter(_.nonEmpty)
            )
          }
        )
      }
    }
  }

  // ── Billing-API types and parsers ─────────────────────────────────────────

  private case class ApiScreening(movieId: String, screenId: String, release: String)

  private def parseApiScreenings(body: String): Map[LocalDateTime, ApiScreening] =
    Try(Json.parse(body).as[JsArray]).map { arr =>
      arr.value.flatMap { s =>
        for {
          timeStr  <- (s \ "screeningTimeFrom").asOpt[String]
          movieId  <- (s \ "movieId").asOpt[String]
          screenId <- (s \ "screenId").asOpt[String]
        } yield {
          val localTime = ZonedDateTime.parse(timeStr, OffsetDtf)
            .withZoneSameInstant(WarsawZone).toLocalDateTime
          val release = (s \ "release").asOpt[String].getOrElse("")
          localTime -> ApiScreening(movieId, screenId, release)
        }
      }.toMap
    }.getOrElse(Map.empty)

  private case class ApiMovieInfo(
    duration:     Option[Int],
    description:  Option[String],
    cast:         Option[String],
    director:     Option[String],
    year:         Option[Int],
    premierePl:   Option[java.time.LocalDate],
    premiereWorld: Option[java.time.LocalDate],
    posterUrl:    Option[String]
  )

  private def parseApiMovie(resp: HttpResponse[String]): Option[ApiMovieInfo] = {
    if (resp.statusCode() != 200) return None
    Try(Json.parse(resp.body())).toOption.map { js =>
      val premierePl = (js \ "premiereDate").asOpt[String]
        .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption)
      val premiereWorld = (js \ "worldPremiereDate").asOpt[String]
        .flatMap(s => Try(ZonedDateTime.parse(s, OffsetDtf).toLocalDate).toOption)
      val poster = (js \ "posters").asOpt[JsArray]
        .flatMap(_.value.headOption)
        .flatMap(_.asOpt[String])
        .filter(_.startsWith("http"))
      ApiMovieInfo(
        duration     = (js \ "duration").asOpt[Int],
        description  = (js \ "description").asOpt[String].filter(_.nonEmpty),
        cast         = (js \ "filmCast").asOpt[String].filter(_.nonEmpty),
        director     = (js \ "director").asOpt[String].filter(_.nonEmpty),
        year         = (js \ "yearOfProduction").asOpt[String].flatMap(s => Try(s.take(4).toInt).toOption),
        premierePl   = premierePl,
        premiereWorld = premiereWorld,
        posterUrl    = poster
      )
    }
  }

  // ── Repertoire page ───────────────────────────────────────────────────────
  //
  // The page embeds all data in a server-side-rendered IIFE of the form:
  //   window.__NUXT__=(function(a,b,...,nU){ return { ...body... } }(v0,v1,...,vN))
  // String values are deduplicated into the argument list; the body references
  // them by parameter name.  We build a paramMap to resolve those references.

  private val ScreeningsSectionPat = """screenings:\{"\d{4}-\d{2}-\d{2}"""".r

  private def parseRepertoirePage(html: String): (Map[String, MovieMeta], Map[String, Seq[(LocalDateTime, Option[String])]]) = {
    val nuxtIndex = html.lastIndexOf("window.__NUXT__")
    if (nuxtIndex < 0) return (Map.empty, Map.empty)
    val nuxtScript = html.substring(nuxtIndex)

    // Layout: (function(<params>){ <body> }(<values>))
    val paramsStart = nuxtScript.indexOf("(function(") + "(function(".length
    val paramsEnd   = nuxtScript.indexOf("){", paramsStart)
    val bodyEnd     = nuxtScript.indexOf("}(")   // start of invocation arguments
    if (paramsEnd < 0 || bodyEnd < 0) return (Map.empty, Map.empty)

    val paramNames = nuxtScript.substring(paramsStart, paramsEnd).split(",").toSeq
    val valuesRaw  = nuxtScript.substring(bodyEnd + 2)
    val valuesEnd  = valuesRaw.lastIndexOf("))")

    val paramMap: Map[String, JsValue] = Try {
      val valuesArray = Json.parse("[" + valuesRaw.substring(0, valuesEnd) + "]").as[JsArray]
      paramNames.zip(valuesArray.value).toMap
    }.getOrElse(return (Map.empty, Map.empty))

    // Resolve a token that is either a quoted literal or a param name reference.
    // Poster URLs in the body use / for "/" — normalise on the way out.
    def resolve(token: String): Option[String] = {
      val trimmed = token.trim
      val raw =
        if (trimmed.startsWith("\"")) Some(trimmed.stripPrefix("\"").stripSuffix("\""))
        else paramMap.get(trimmed).flatMap(_.asOpt[String])
      raw.map(_.replace("\\u002F", "/"))
    }

    val body = nuxtScript.substring(paramsEnd + 2, bodyEnd)

    val screeningsMatch = ScreeningsSectionPat.findFirstMatchIn(body).getOrElse(return (Map.empty, Map.empty))
    val movieMetaBody  = body.substring(0, screeningsMatch.start)
    val screeningsBody = body.substring(screeningsMatch.start + "screenings:".length)

    val movieInfoMap    = parseMovieInfo(movieMetaBody, resolve)
    val showtimesByMovie = parseScreenings(screeningsBody, resolve).groupMap(_._1)(_._2)

    (movieInfoMap, showtimesByMovie)
  }

  // ── Movie metadata from repertoire NUXT ──────────────────────────────────

  private case class MovieMeta(title: String, slug: String, posterUrl: Option[String], filmUrl: Option[String])

  private def parseMovieInfo(movieMetaBody: String, resolve: String => Option[String]): Map[String, MovieMeta] = {
    val result = mutable.Map[String, MovieMeta]()

    // Regular movies: ,id:NUMBER,sourceId:"UUID",title:VAR_OR_LIT,...,slug:"SLUG"
    val moviePattern = """,id:(\d{3,}),sourceId:"[^"]+",title:("([^"]+)"|(\w+)),titleOriginal:(?:"[^"]+"|(?:\w+)),slug:"([^"]+)"""".r
    for (movieMatch <- moviePattern.findAllMatchIn(movieMetaBody)) {
      val numericId = movieMatch.group(1)
      val title     = Option(movieMatch.group(3)).orElse(resolve(movieMatch.group(4)))
      val slug      = movieMatch.group(5)
      val nearby    = movieMetaBody.substring(movieMatch.start, math.min(movieMatch.start + 1000, movieMetaBody.length))
      val posterUrl = """posterPhoto:\{filePath:"[^"]+",url:"([^"]+)"""".r
        .findFirstMatchIn(nearby).map(_.group(1).replace("\\u002F", "/"))
      title.foreach { titleStr =>
        result(s"m$numericId") = MovieMeta(titleStr, slug, posterUrl, Some(s"$BaseUrl/filmy/$slug-$numericId"))
      }
    }

    // Events: _id:"eNNNN" — metadata appears in the 500 chars before the id token
    val eventIdPattern = """_id:"(e\d+)"""".r
    for (eventMatch <- eventIdPattern.findAllMatchIn(movieMetaBody)) {
      val eventId   = eventMatch.group(1)
      val numericId = eventId.substring(1)
      val before    = movieMetaBody.substring(math.max(0, eventMatch.start - 500), eventMatch.start)
      for (nameMatch <- """,name:(\w+),slug:(\w+),""".r.findAllMatchIn(before).toSeq.lastOption) {
        val title     = resolve(nameMatch.group(1))
        val slug      = resolve(nameMatch.group(2))
        val posterUrl = """posterPhoto:\{filePath:(\w+),url:(\w+)""".r
          .findAllMatchIn(before).toSeq.lastOption.flatMap(posterMatch => resolve(posterMatch.group(2)))
        for (titleStr <- title; slugStr <- slug)
          result(eventId) = MovieMeta(titleStr, slugStr, posterUrl, Some(s"$BaseUrl/wydarzenie/$slugStr-$numericId"))
      }
    }

    result.toMap
  }

  // ── Screenings from repertoire NUXT ──────────────────────────────────────

  private def parseScreenings(screeningsBody: String, resolve: String => Option[String]): Seq[(String, (LocalDateTime, Option[String]))] = {
    val result     = mutable.ListBuffer[(String, (LocalDateTime, Option[String]))]()
    val dayPattern = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
    val dayMatches = dayPattern.findAllMatchIn(screeningsBody).toSeq

    for ((dayMatch, dayIndex) <- dayMatches.zipWithIndex) {
      val dayBlockEnd = if (dayIndex + 1 < dayMatches.length) dayMatches(dayIndex + 1).start else screeningsBody.length
      val dayBlock    = screeningsBody.substring(dayMatch.end, dayBlockEnd)

      for (groupMatch <- """([em]\d+):\{screenings:\[""".r.findAllMatchIn(dayBlock)) {
        val movieId = groupMatch.group(1)

        // Walk brackets to find the closing ] of this movie's screenings array.
        var depth = 1; var pos = groupMatch.end
        while (pos < dayBlock.length && depth > 0) {
          if (dayBlock(pos) == '[') depth += 1
          else if (dayBlock(pos) == ']') depth -= 1
          pos += 1
        }
        val screeningsArray = dayBlock.substring(groupMatch.end, pos - 1)

        val screeningPattern = """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r
        for (screeningMatch <- screeningPattern.findAllMatchIn(screeningsArray)) {
          val timeValue   = resolve(screeningMatch.group(1))
          val rawSourceId = screeningMatch.group(2)
          val sourceId    = if (rawSourceId.startsWith("\"")) Some(rawSourceId.stripPrefix("\"").stripSuffix("\""))
                            else resolve(rawSourceId)

          for {
            timeStr <- timeValue if timeStr.length == 19
            dateTime   = LocalDateTime.parse(timeStr.replace(' ', 'T'))
            bookingUrl = sourceId.map(sid => s"$BookingBase/$sid?cinemaId=$CinemaSourceId")
          } result += movieId -> (dateTime, bookingUrl)
        }
      }
    }

    result.toSeq
  }

}
