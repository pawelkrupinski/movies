package clients

import models.{CinemaMovie, Movie, Multikino, Showtime}
import play.api.Logging
import play.api.libs.json._

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{CookieManager, CookiePolicy, URI}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

// ── Multikino client ───────────────────────────────────────────────────────

object MultikinoClient extends Logging {

  private val HomeUrl        = "https://www.multikino.pl/"
  private val ApiUrl         = "https://www.multikino.pl/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"
  private val ScrapingAntUrl = "https://api.scrapingant.com/v2/general"

  private val scrapingAntKey: Option[String] = Option(System.getenv("SCRAPINGANT_KEY")).filter(_.nonEmpty)

  private val cookieManager = new CookieManager(null, CookiePolicy.ACCEPT_ALL)

  private val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .cookieHandler(cookieManager)
    .build()

  private def scrapingAntRequest(key: String): HttpRequest = {
    val encoded = URLEncoder.encode(ApiUrl, StandardCharsets.UTF_8)
    HttpRequest.newBuilder()
      .uri(URI.create(s"$ScrapingAntUrl?url=$encoded&browser=true&proxy_country=pl&return_page_source=true"))
      .header("x-api-key", key)
      .header("Accept", "application/json, text/plain, */*")
      .GET()
      .build()
  }

  private def apiRequest(): HttpRequest =
    HttpRequest.newBuilder()
      .uri(URI.create(ApiUrl))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "application/json, text/plain, */*")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      .header("Referer", "https://www.multikino.pl/repertuar/poznan-stary-browar/teraz-gramy")
      .GET()
      .build()

  private def refreshSession(): Unit =
    httpClient.send(
      HttpRequest.newBuilder()
        .uri(URI.create(HomeUrl))
        .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
        .header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
        .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
        .GET()
        .build(),
      HttpResponse.BodyHandlers.discarding()
    )

  def fetch(): Seq[CinemaMovie] = {
    val body = scrapingAntKey match {
      case Some(key) =>
        val response = httpClient.send(scrapingAntRequest(key), HttpResponse.BodyHandlers.ofString())
        val status   = response.statusCode()
        if (status != 200)
          throw new RuntimeException(s"ScrapingAnt returned $status: ${response.body().take(500)}")
        response.body()
      case None =>
        val response = httpClient.send(apiRequest(), HttpResponse.BodyHandlers.ofString())
        response.statusCode() match {
          case 200 => response.body()
          case 401 | 403 =>
            refreshSession()
            val retryResponse = httpClient.send(apiRequest(), HttpResponse.BodyHandlers.ofString())
            if (retryResponse.statusCode() != 200)
              throw new RuntimeException(s"API returned ${retryResponse.statusCode()} after session refresh")
            retryResponse.body()
          case code =>
            throw new RuntimeException(s"API returned $code")
        }
    }

    parseJson(body)
  }

  // ── JSON parsing ───────────────────────────────────────────────────────────

  private def parseJson(json: String): Seq[CinemaMovie] = {
    val films = (Json.parse(json) \ "result").as[JsArray].value

    films.map { film =>
      val rawTitle       = (film \ "filmTitle").as[String]
      val title          = rawTitle.replaceFirst("^Kino na obcasach:\\s*", "")
      val runtimeMinutes = (film \ "runningTime").asOpt[Int]
      val releaseYear    = (film \ "releaseDate").asOpt[String]
                             .flatMap(d => scala.util.Try(java.time.LocalDate.parse(d.take(10)).getYear).toOption)
      val multikinoId    = (film \ "filmId").asOpt[String].filter(_.nonEmpty)
      val mxcId          = (film \ "movieXchangeCode").asOpt[String].filter(_.nonEmpty)
      val movie          = Movie(title, runtimeMinutes, releaseYear)
      val posterUrl      = (film \ "posterImageSrc").asOpt[String].filter(_.nonEmpty)
      val filmUrl   = (film \ "filmUrl").asOpt[String].filter(_.nonEmpty)
                        .map(url => if (url.startsWith("http")) url else s"https://www.multikino.pl$url")
      val synopsis  = (film \ "synopsisShort").asOpt[String].filter(_.nonEmpty)
      val cast      = (film \ "cast").asOpt[String].filter(_.nonEmpty)
      val director  = (film \ "director").asOpt[String].filter(_.nonEmpty)
      val groups    = (film \ "showingGroups").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)

      val showtimes = groups.flatMap { group =>
        (group \ "sessions").as[JsArray].value.flatMap { session =>
          (session \ "startTime").asOpt[String].map { startTime =>
            val bookingUrl = (session \ "bookingUrl").asOpt[String]
                              .map(url => if (url.startsWith("http")) url else s"https://www.multikino.pl$url")
            val room = (session \ "screenName").asOpt[String].filter(_.nonEmpty)
            Showtime(dateTime = LocalDateTime.parse(startTime), bookingUrl = bookingUrl, room = room)
          }
        }
      }.toSeq

      CinemaMovie(
        movie       = movie,
        cinema      = Multikino,
        posterUrl   = posterUrl,
        filmUrl     = filmUrl,
        synopsis    = synopsis,
        cast        = cast,
        director    = director,
        showtimes   = showtimes,
        externalIds = (multikinoId.map("mk" -> _) ++ mxcId.map("mxc" -> _)).toMap
      )
    }.toSeq
  }
}
