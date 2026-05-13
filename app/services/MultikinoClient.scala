package clients

import models.{CinemaMovie, Movie, Multikino, Showtime}
import play.api.Logging
import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{CookieManager, CookiePolicy, URI}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

class MultikinoClient(http: HttpFetch = MultikinoClient.DefaultFetch) {

  import MultikinoClient.ApiUrl

  def fetch(): Seq[CinemaMovie] = parseJson(http.get(ApiUrl))

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
            val room   = (session \ "screenName").asOpt[String].filter(_.nonEmpty)
            val attrs  = (session \ "attributes").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                          .flatMap(a => (a \ "name").asOpt[String])
                          .toSet
            // Multikino tags Language attributes as DUBBING / NAPISY / JĘZYK ORYGINALNY.
            // Normalise to the same DUB/NAP tokens other clients use; original-language
            // screenings get no token (matches Cinema City's behaviour).
            val format = List(
              attrs.find(n => n == "2D" || n == "3D"),
              if (attrs.contains("DUBBING"))     Some("DUB")
              else if (attrs.contains("NAPISY")) Some("NAP")
              else None
            ).flatten
            Showtime(dateTime = LocalDateTime.parse(startTime), bookingUrl = bookingUrl, room = room, format = format)
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

object MultikinoClient extends Logging {

  val ApiUrl = "https://www.multikino.pl/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"

  private val HomeUrl        = "https://www.multikino.pl/"
  private val ScrapingAntUrl = "https://api.scrapingant.com/v2/general"

  object DefaultFetch extends HttpFetch {
    private val scrapingAntKey: Option[String] = Option(System.getenv("SCRAPINGANT_KEY")).filter(_.nonEmpty)

    private val cookieManager = new CookieManager(null, CookiePolicy.ACCEPT_ALL)

    private val httpClient = HttpClient.newBuilder()
      .version(HttpClient.Version.HTTP_1_1)
      .followRedirects(HttpClient.Redirect.NORMAL)
      .cookieHandler(cookieManager)
      .build()

    override def get(url: String): String = scrapingAntKey match {
      case Some(key) =>
        val encoded  = URLEncoder.encode(url, StandardCharsets.UTF_8)
        val request  = HttpRequest.newBuilder()
          .uri(URI.create(s"$ScrapingAntUrl?url=$encoded&browser=true&proxy_country=pl&return_page_source=true"))
          .header("x-api-key", key)
          .header("Accept", "application/json, text/plain, */*")
          .GET()
          .build()
        val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
        val status   = response.statusCode()
        if (status != 200)
          throw new RuntimeException(s"ScrapingAnt returned $status: ${response.body().take(500)}")
        response.body()

      case None =>
        def apiRequest() = HttpRequest.newBuilder()
          .uri(URI.create(url))
          .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
          .header("Accept", "application/json, text/plain, */*")
          .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
          .header("Referer", "https://www.multikino.pl/repertuar/poznan-stary-browar/teraz-gramy")
          .GET()
          .build()

        val response = httpClient.send(apiRequest(), HttpResponse.BodyHandlers.ofString())
        response.statusCode() match {
          case 200 => response.body()
          case 401 | 403 =>
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
            val retryResponse = httpClient.send(apiRequest(), HttpResponse.BodyHandlers.ofString())
            if (retryResponse.statusCode() != 200)
              throw new RuntimeException(s"API returned ${retryResponse.statusCode()} after session refresh")
            retryResponse.body()
          case code =>
            throw new RuntimeException(s"API returned $code")
        }
    }
  }

  def fetch(): Seq[CinemaMovie] = new MultikinoClient().fetch()
}
