package clients

import models.{CinemaMovie, Helios, Movie, Showtime}
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.LocalDateTime
import scala.collection.mutable
import scala.util.Try

object HeliosClient {

  private val PageUrl        = "https://helios.pl/poznan/kino-helios/repertuar"
  private val CinemaSourceId = "815face9-2a1d-4c62-9b2f-a361574b79a2"
  private val BaseUrl        = "https://helios.pl/poznan/kino-helios"
  private val BookingBase    = "https://bilety.helios.pl/screen"

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

  def fetch(): Seq[CinemaMovie] = {
    val repertoireResponse = httpClient.send(buildRequest(PageUrl), HttpResponse.BodyHandlers.ofString())
    if (repertoireResponse.statusCode() != 200)
      throw new RuntimeException(s"helios.pl returned ${repertoireResponse.statusCode()}")

    val (movieInfoMap, showtimesByMovie) = parseRepertoirePage(repertoireResponse.body())

    // Fire all individual movie-page requests concurrently
    val pendingPages = movieInfoMap.values
      .flatMap(_.filmUrl)
      .toSeq.distinct
      .map(url => url -> httpClient.sendAsync(buildRequest(url), HttpResponse.BodyHandlers.ofString()))

    val pageMetaByUrl: Map[String, MoviePageMeta] = pendingPages.flatMap { case (url, future) =>
      Try(parseMoviePage(future.join().body())).toOption.map(url -> _)
    }.toMap

    showtimesByMovie.toSeq.flatMap { case (movieId, slots) =>
      movieInfoMap.get(movieId).map { info =>
        val pageMeta = info.filmUrl.flatMap(pageMetaByUrl.get)
        CinemaMovie(
          movie     = Movie(info.title
            .stripSuffix(" w Helios RePlay")
            .stripSuffix(" w Helios Anime")
            .stripSuffix(" w Helios na Scenie")
            .stripSuffix(" - Salon Kultury Helios")),
          cinema    = Helios,
          posterUrl = info.posterUrl,
          filmUrl   = info.filmUrl,
          synopsis  = pageMeta.flatMap(_.synopsis),
          cast      = pageMeta.flatMap(_.cast),
          director  = pageMeta.flatMap(_.director),
          showtimes = slots.sortBy(_._1).map { case (dateTime, bookingUrl) => Showtime(dateTime, bookingUrl) }
        )
      }
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

  // ── Individual movie page ─────────────────────────────────────────────────
  //
  // Synopsis comes from JSON-LD (always the full text, already plain).
  // Director and cast are literal strings in the NUXT metadata body
  // (not variable references), so no paramMap is needed there.

  private case class MoviePageMeta(synopsis: Option[String], cast: Option[String], director: Option[String])

  private val LiteralFieldPat  = (field: String) => raw"""$field:"((?:[^"\\]|\\.)*)"""".r
  private val JsonLdScriptPat  = """<script[^>]+application/ld\+json[^>]*>(.*?)</script>""".r

  private def parseMoviePage(html: String): MoviePageMeta = {
    val synopsis = parseJsonLdDescription(html)

    val nuxtIndex = html.lastIndexOf("window.__NUXT__")
    if (nuxtIndex < 0) return MoviePageMeta(synopsis, None, None)
    val nuxtScript = html.substring(nuxtIndex)

    val paramsEnd = nuxtScript.indexOf("){")
    val bodyEnd   = nuxtScript.indexOf("}(")
    if (paramsEnd < 0 || bodyEnd < 0) return MoviePageMeta(synopsis, None, None)

    val fullBody = nuxtScript.substring(paramsEnd + 2, bodyEnd)
    // Only search in the movie-metadata section (before screenings) to avoid
    // matching Helios corporate boilerplate that appears later in the page.
    val metaBody = ScreeningsSectionPat.findFirstMatchIn(fullBody)
      .map(m => fullBody.substring(0, m.start))
      .getOrElse(fullBody)

    def extract(field: String): Option[String] =
      LiteralFieldPat(field).findFirstMatchIn(metaBody)
        .map(_.group(1))
        .filter(_.nonEmpty)

    MoviePageMeta(
      synopsis = synopsis,
      cast     = extract("cast"),
      director = extract("director")
    )
  }

  // The JSON-LD Movie object always carries the full plain-text description.
  private def parseJsonLdDescription(html: String): Option[String] =
    JsonLdScriptPat.findAllMatchIn(html)
      .flatMap(m => scala.util.Try(play.api.libs.json.Json.parse(m.group(1))).toOption)
      .collectFirst { case json if (json \ "description").isDefined =>
        (json \ "description").as[String].trim
      }
      .filter(_.nonEmpty)

}
