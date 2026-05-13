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

  // ── Fetch ─────────────────────────────────────────────────────────────────

  def fetch(): Seq[CinemaMovie] = {
    val today = LocalDate.now(WarsawZone)
    val in6   = today.plusDays(6)

    val screeningsUrl =
      s"$ApiBase/cinema/$CinemaSourceId/screening" +
        s"?dateTimeFrom=${today}T00:00:00&dateTimeTo=${in6}T23:59:59"

    val nuxtHtml    = http.get(PageUrl)
    val screeningsById =
      parseApiScreenings(Try(http.getAsync(screeningsUrl).join()).getOrElse("[]"))

    val movieBodies =
      fetchBodies(screeningsById.values.map(_.movieId).toSeq.distinct)(id => s"$ApiBase/movie/$id")
    val screenBodies =
      fetchBodies(screeningsById.values.map(_.screenId).toSeq.distinct)(id =>
        s"$ApiBase/cinema/$CinemaSourceId/screen/$id")

    val restMovies  = buildCinemaMovies(screeningsById, movieBodies, screenBodies)
    val nuxtMovies  = buildCinemaMoviesFromNuxt(nuxtHtml)

    uniquePosterUrls(mergeMoviesByTitle(mergeLessSpecificSameShowtimeRows(restMovies ++ nuxtMovies)))
  }

  // ── REST assembly ─────────────────────────────────────────────────────────

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
    slug:          Option[String]
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
          slug          = (js \ "slug").asOpt[String].filter(_.nonEmpty)
        )
      }
    }

  private def fetchBodies(ids: Seq[String])(urlFor: String => String): Map[String, String] =
    ids
      .map(id => id -> http.getAsync(urlFor(id)))
      .flatMap { case (id, f) => Try(id -> f.join()).toOption }
      .toMap

  private def buildCinemaMovies(
    screeningsById: Map[String, ApiScreening],
    movieBodies:    Map[String, String],
    screenBodies:   Map[String, String]
  ): Seq[CinemaMovie] = {
    val movieDetails = movieBodies.flatMap { case (id, body) =>
      parseApiMovieBody(body).map(id -> _)
    }
    val screenNames = screenBodies.flatMap { case (id, body) =>
      Try((Json.parse(body) \ "name").asOpt[String]).toOption.flatten.map(id -> _)
    }

    screeningsById.values.toSeq
      .groupBy(_.movieId)
      .toSeq
      .flatMap { case (movieId, screenings) =>
        movieDetails.get(movieId).map { movie =>
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
            showtimes = screenings.flatMap(s => toShowtime(s, screenNames)).distinct.sortBy(_.dateTime)
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

  // ── Merge helpers ─────────────────────────────────────────────────────────

  private def mergeLessSpecificSameShowtimeRows(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val enriched = movies.map { movie =>
      val times = movie.showtimes.map(_.dateTime).toSet
      val moreSpecificRows = movies.filter { other =>
        movie.movie.title != other.movie.title &&
          isLessSpecificTitle(movie.movie.title, other.movie.title) &&
          other.showtimes.exists(st => times.contains(st.dateTime))
      }
      if (moreSpecificRows.isEmpty) movie
      else movie.copy(
        movie     = movie.movie.copy(
          runtimeMinutes = movie.movie.runtimeMinutes.orElse(moreSpecificRows.flatMap(_.movie.runtimeMinutes).headOption),
          releaseYear    = movie.movie.releaseYear.orElse(moreSpecificRows.flatMap(_.movie.releaseYear).headOption),
          premierePl     = movie.movie.premierePl.orElse(moreSpecificRows.flatMap(_.movie.premierePl).headOption),
          premiereWorld  = movie.movie.premiereWorld.orElse(moreSpecificRows.flatMap(_.movie.premiereWorld).headOption)
        ),
        posterUrl = movie.posterUrl.orElse(moreSpecificRows.flatMap(_.posterUrl).headOption),
        filmUrl   = movie.filmUrl.orElse(moreSpecificRows.flatMap(_.filmUrl).headOption),
        synopsis  = movie.synopsis.orElse(moreSpecificRows.flatMap(_.synopsis).headOption),
        cast      = movie.cast.orElse(moreSpecificRows.flatMap(_.cast).headOption),
        director  = movie.director.orElse(moreSpecificRows.flatMap(_.director).headOption)
      )
    }

    enriched.filterNot { movie =>
      val times = movie.showtimes.map(_.dateTime).toSet
      enriched.exists { other =>
        movie.movie.title != other.movie.title &&
          isLessSpecificTitle(movie.movie.title, other.movie.title) &&
          other.showtimes.exists(st => times.contains(st.dateTime))
      }
    }
  }

  private def isLessSpecificTitle(title: String, other: String): Boolean =
    normalizeTitle(other).startsWith(normalizeTitle(title) + " ")

  private def normalizeTitle(title: String): String =
    title.toLowerCase.replaceAll("\\s+", " ").trim

  private def uniquePosterUrls(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val seen = scala.collection.mutable.Set.empty[String]
    movies.map { movie =>
      movie.posterUrl match {
        case Some(url) if seen(url) => movie.copy(posterUrl = None)
        case Some(url)              => seen += url; movie
        case None                   => movie
      }
    }
  }

  private def mergeMoviesByTitle(movies: Seq[CinemaMovie]): Seq[CinemaMovie] =
    movies
      .groupBy(_.movie.title)
      .values
      .map { rows =>
        val best = rows.maxBy(row =>
          Seq(row.posterUrl, row.movie.runtimeMinutes, row.filmUrl, row.synopsis, row.cast, row.director).count(_.nonEmpty)
        )
        best.copy(
          movie     = best.movie.copy(
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
            .groupBy(st => st.bookingUrl.getOrElse(st.dateTime.toString))
            .values
            .map(same => same.maxBy(st => Seq(st.room, st.format).count(_.nonEmpty)))
            .toSeq
            .sortBy(_.dateTime)
        )
      }
      .toSeq
      .sortBy(_.movie.title)

  private def cleanTitle(title: String): String =
    Seq(" w Helios RePlay", " w Helios Anime", " w Helios na Scenie",
        " - Salon Kultury Helios", " - KNTJ", " - KNT")
      .foldLeft(title)((t, suffix) => t.stripSuffix(suffix))

  // ── NUXT fallback (events + movies outside the 6-day REST window) ─────────

  private def buildCinemaMoviesFromNuxt(html: String): Seq[CinemaMovie] = {
    val parsed   = parseNuxtPage(html)
    val nuxtRows = parsed.showtimesByMovie.toSeq.flatMap { case (movieId, slots) =>
      parsed.movies.get(movieId).map(movie => movie -> slots)
    }

    nuxtRows
      .groupBy(_._1.title)
      .values
      .map { rows =>
        val movies = rows.map(_._1)
        val title  = cleanTitle(movies.head.title)
        val movie  = movies.filter(m => cleanTitle(m.title) == title)
          .find(_.runtimeMinutes.nonEmpty)
          .orElse(movies.find(_.runtimeMinutes.nonEmpty))
          .getOrElse(movies.head)
        val slots  = rows.flatMap(_._2).distinct.sortBy(_._1)
        CinemaMovie(
          movie = Movie(title = title, runtimeMinutes = movie.runtimeMinutes,
                        releaseYear = None, premierePl = None, premiereWorld = None),
          cinema    = Helios,
          posterUrl = movies.flatMap(_.posterUrl).headOption,
          filmUrl   = movies.flatMap(_.filmUrl).headOption,
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = slots.map { case (dateTime, screeningId) =>
            Showtime(
              dateTime   = dateTime,
              bookingUrl = Some(s"$BookingBase/$screeningId?cinemaId=$CinemaSourceId").filter(_ => screeningId.nonEmpty),
              room       = None,
              format     = None
            )
          }.distinct
        )
      }
      .filter(_.showtimes.nonEmpty)
      .toSeq
  }

  // ── NUXT IIFE parser ──────────────────────────────────────────────────────

  private case class NuxtPage(
    movies:          Map[String, NuxtMovie],
    showtimesByMovie: Map[String, Seq[(LocalDateTime, String)]]
  )

  private case class NuxtMovie(
    title:          String,
    slug:           String,
    posterUrl:      Option[String],
    filmUrl:        Option[String],
    runtimeMinutes: Option[Int]
  )

  private def parseNuxtPage(html: String): NuxtPage = {
    val nuxtIndex = html.lastIndexOf("window.__NUXT__")
    if (nuxtIndex < 0) return NuxtPage(Map.empty, Map.empty)

    val script     = html.substring(nuxtIndex)
    val paramsStart = script.indexOf("(function(") + "(function(".length
    val paramsEnd   = script.indexOf("){", paramsStart)
    val bodyEnd     = script.indexOf("}(")

    if (paramsStart < "(function(".length || paramsEnd < 0 || bodyEnd < 0)
      return NuxtPage(Map.empty, Map.empty)

    val paramNames = script.substring(paramsStart, paramsEnd).split(",").toSeq
    val valuesRaw  = script.substring(bodyEnd + 2)
    val valuesEnd  = valuesRaw.lastIndexOf("))")

    val paramMap: Map[String, JsValue] =
      Try {
        val clean = valuesRaw.substring(0, valuesEnd)
          .replaceAll("""Array\(\d+\)""", "null")
          .replace("undefined", "null")
        paramNames.zip(Json.parse("[" + clean + "]").as[JsArray].value).toMap
      }.getOrElse(Map.empty)

    def resolve(token: String): Option[String] = {
      val t = token.trim
      val raw =
        if (t.startsWith("\"")) Some(t.stripPrefix("\"").stripSuffix("\""))
        else paramMap.get(t).flatMap {
          case JsString(s) => Some(s)
          case n: JsNumber => n.value.toBigIntExact.map(_.toString)
          case _           => None
        }
      raw.map(_.replace("\\u002F", "/"))
    }

    val body = script.substring(paramsEnd + 2, bodyEnd)
    val screeningsMarker = """screenings:\{"\d{4}-\d{2}-\d{2}"""".r.findFirstMatchIn(body)

    screeningsMarker match {
      case None => NuxtPage(Map.empty, Map.empty)
      case Some(marker) =>
        val movieBody      = body.substring(0, marker.start)
        val screeningsBody = body.substring(marker.start + "screenings:".length)
        NuxtPage(
          movies           = parseNuxtMovies(movieBody, screeningsBody, resolve),
          showtimesByMovie = parseNuxtShowtimes(screeningsBody, resolve).groupMap(_._1)(_._2)
        )
    }
  }

  private def parseNuxtMovies(
    movieBody:      String,
    screeningsBody: String,
    resolve:        String => Option[String]
  ): Map[String, NuxtMovie] = {
    val normal   = parseNormalNuxtMovies(movieBody, resolve)
    val embedded = parseEmbeddedScreeningNuxtMovies(screeningsBody, resolve)
    val events   = parseEventNuxtMovies(movieBody, screeningsBody, resolve)
    normal ++ embedded ++ enrichEventNuxtMovies(events, embedded)
  }

  private def enrichEventNuxtMovies(
    events:   Map[String, NuxtMovie],
    embedded: Map[String, NuxtMovie]
  ): Map[String, NuxtMovie] =
    events.map { case (id, ev) =>
      id -> embedded.get(id).map(em => ev.copy(
        posterUrl      = ev.posterUrl.orElse(em.posterUrl),
        runtimeMinutes = ev.runtimeMinutes.orElse(em.runtimeMinutes)
      )).getOrElse(ev)
    }

  private def parseEmbeddedScreeningNuxtMovies(
    screeningsBody: String,
    resolve:        String => Option[String]
  ): Map[String, NuxtMovie] =
    """([em]\d+):\{screenings:\[""".r.findAllMatchIn(screeningsBody).flatMap { gm =>
      val movieId = gm.group(1)
      val arr     = bracketedArrayContent(screeningsBody, gm.end)
      """movie:\{(.*?)\},moviePrint:""".r
        .findFirstMatchIn(arr)
        .flatMap(m => parseNuxtEmbeddedMovieBlock(m.group(1), resolve).map(movieId -> _))
    }.toMap

  private def parseNuxtEmbeddedMovieBlock(movie: String, resolve: String => Option[String]): Option[NuxtMovie] = {
    val title     = """title:(?:"([^"]+)"|(\w+))""".r.findFirstMatchIn(movie)
                      .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))
    val slug      = """slug:(?:"([^"]+)"|(\w+))""".r.findFirstMatchIn(movie)
                      .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))
    val numericId = """id:(\d{3,}|\w+)""".r.findFirstMatchIn(movie)
                      .flatMap { m =>
                        val raw = m.group(1)
                        if (raw.forall(_.isDigit)) Some(raw) else resolve(raw).filter(_.forall(_.isDigit))
                      }
    val poster    = """posterPhoto:\{.*?url:(?:"([^"]+)"|(\w+))""".r.findFirstMatchIn(movie)
                      .flatMap(m => Option(m.group(1)).orElse(resolve(m.group(2))))
                      .map(_.replace("\\u002F", "/"))
    val runtime   = """duration:([^,}]+)""".r.findFirstMatchIn(movie)
                      .flatMap(m => parseInt(m.group(1)).orElse(resolve(m.group(1)).flatMap(parseInt)))
    for {
      titleStr <- title
      slugStr  <- slug
    } yield NuxtMovie(
      title          = titleStr,
      slug           = slugStr,
      posterUrl      = poster,
      filmUrl        = numericId.map(id => s"$BaseUrl/filmy/$slugStr-$id").orElse(Some(s"$BaseUrl/filmy/$slugStr")),
      runtimeMinutes = runtime
    )
  }

  private def parseNormalNuxtMovies(movieBody: String, resolve: String => Option[String]): Map[String, NuxtMovie] = {
    val moviePat = """,id:(\d{3,}|\w+),sourceId:(?:"([^"]+)"|(\w+)),title:(?:"([^"]+)"|(\w+)),titleOriginal:(?:"[^"]*"|\w+),slug:(?:"([^"]+)"|(\w+))""".r
    moviePat.findAllMatchIn(movieBody).flatMap { m =>
      val rawId     = m.group(1)
      val numericId = if (rawId.forall(_.isDigit)) rawId
                      else resolve(rawId).filter(_.forall(_.isDigit)).getOrElse("")
      val title     = Option(m.group(4)).orElse(resolve(m.group(5)))
      val slug      = Option(m.group(6)).orElse(resolve(m.group(7)))
      if (numericId.isEmpty) None
      else {
        val nearby  = movieBody.substring(m.start, math.min(m.start + 1500, movieBody.length))
        val poster  = """posterPhoto:\{filePath:(?:"[^"]+"|[^,{]+),url:(?:"([^"]+)"|(\w+))""".r
                        .findFirstMatchIn(nearby)
                        .flatMap(pm => Option(pm.group(1)).orElse(resolve(pm.group(2))))
                        .map(_.replace("\\u002F", "/"))
        val runtime = """duration:([^,}]+)""".r.findFirstMatchIn(nearby)
                        .flatMap(mm => parseInt(mm.group(1)).orElse(resolve(mm.group(1)).flatMap(parseInt)))
        for {
          titleStr <- title
          slugStr  <- slug
        } yield s"m$numericId" -> NuxtMovie(titleStr, slugStr, poster, Some(s"$BaseUrl/filmy/$slugStr-$numericId"), runtime)
      }
    }.toMap
  }

  private def parseEventNuxtMovies(
    movieBody:      String,
    screeningsBody: String,
    resolve:        String => Option[String]
  ): Map[String, NuxtMovie] = {
    val eventIds = """([e]\d+):\{screenings:\[""".r.findAllMatchIn(screeningsBody).map(_.group(1)).toSet
    eventIds.flatMap { eventId =>
      val numericId = eventId.stripPrefix("e")
      val meta = findEventMetaInMovieBody(eventId, movieBody, resolve)
                   .orElse(findEventMetaInScreeningsBody(eventId, screeningsBody, resolve))
      meta.map(movie => eventId -> movie.copy(filmUrl = movie.filmUrl.orElse(Some(s"$BaseUrl/wydarzenie/${movie.slug}-$numericId"))))
    }.toMap
  }

  private def findEventMetaInMovieBody(eventId: String, movieBody: String, resolve: String => Option[String]): Option[NuxtMovie] =
    s"""_id:"$eventId"""".r.findFirstMatchIn(movieBody).flatMap { m =>
      val before = movieBody.substring(math.max(0, m.start - 1000), m.start)
      """,name:([\w$]+),slug:([\w$]+),""".r.findAllMatchIn(before).toSeq.lastOption.flatMap { nm =>
        for {
          title <- resolve(nm.group(1))
          slug  <- resolve(nm.group(2))
        } yield NuxtMovie(title, slug, posterUrl = None, filmUrl = None, runtimeMinutes = None)
      }
    }

  private def findEventMetaInScreeningsBody(eventId: String, screeningsBody: String, resolve: String => Option[String]): Option[NuxtMovie] = {
    val start = screeningsBody.indexOf(s"$eventId:{")
    if (start < 0) return None
    val block = screeningsBody.substring(start, math.min(start + 4000, screeningsBody.length))
    """screeningMovies:\[\{.*?movie:\{(.*?)\},moviePrint:""".r.findFirstMatchIn(block).flatMap { m =>
      val movie   = m.group(1)
      val title   = """title:(?:"([^"]+)"|(\w+))""".r.findFirstMatchIn(movie)
                      .flatMap(mm => Option(mm.group(1)).orElse(resolve(mm.group(2))))
      val slug    = """slug:(?:"([^"]+)"|(\w+))""".r.findFirstMatchIn(movie)
                      .flatMap(mm => Option(mm.group(1)).orElse(resolve(mm.group(2))))
      val poster  = """posterPhoto:\{.*?url:(?:"([^"]+)"|(\w+))""".r.findFirstMatchIn(movie)
                      .flatMap(mm => Option(mm.group(1)).orElse(resolve(mm.group(2))))
                      .map(_.replace("\\u002F", "/"))
      val runtime = """duration:([^,}]+)""".r.findFirstMatchIn(movie)
                      .flatMap(mm => parseInt(mm.group(1)).orElse(resolve(mm.group(1)).flatMap(parseInt)))
      for {
        titleStr <- title
        slugStr  <- slug
      } yield NuxtMovie(titleStr, slugStr, poster, Some(s"$BaseUrl/filmy/$slugStr"), runtime)
    }
  }

  private def parseNuxtShowtimes(screeningsBody: String, resolve: String => Option[String]): Seq[(String, (LocalDateTime, String))] = {
    val dayPat       = """"(\d{4}-\d{2}-\d{2})"\s*:\s*\{""".r
    val groupPat     = """([em]\d+):\{screenings:\[""".r
    val screeningPat = """\{timeFrom:("[\d :.-]+"|[^,{]+),saleTimeTo:[^,]+,sourceId:("[\w-]+"|\w+),""".r

    val dayMatches = dayPat.findAllMatchIn(screeningsBody).toSeq
    dayMatches.zipWithIndex.flatMap { case (dayMatch, i) =>
      val end      = if (i + 1 < dayMatches.length) dayMatches(i + 1).start else screeningsBody.length
      val dayBlock = screeningsBody.substring(dayMatch.end, end)
      groupPat.findAllMatchIn(dayBlock).flatMap { gm =>
        val movieId = gm.group(1)
        val arr     = bracketedArrayContent(dayBlock, gm.end)
        screeningPat.findAllMatchIn(arr).flatMap { sm =>
          val time = resolve(sm.group(1))
          val rawSid = sm.group(2)
          val sid  = if (rawSid.startsWith("\"")) Some(rawSid.stripPrefix("\"").stripSuffix("\""))
                     else resolve(rawSid)
          for {
            timeStr <- time if timeStr.length == 19
            s       <- sid
          } yield movieId -> (LocalDateTime.parse(timeStr.replace(' ', 'T')), s)
        }
      }
    }
  }

  private def bracketedArrayContent(text: String, start: Int): String = {
    var depth = 1; var pos = start
    while (pos < text.length && depth > 0) {
      if (text(pos) == '[') depth += 1 else if (text(pos) == ']') depth -= 1
      pos += 1
    }
    text.substring(start, math.max(start, pos - 1))
  }

  private def parseInt(s: String): Option[Int] = Try(s.trim.toInt).toOption
}

object HeliosClient {
  def fetch(): Seq[CinemaMovie] = new HeliosClient().fetch()
}
