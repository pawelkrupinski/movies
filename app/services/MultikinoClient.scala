package clients

import models.{CinemaMovie, Movie, Multikino, Showtime}
import play.api.Logging
import play.api.libs.json._
import tools.{Env, HttpFetch}

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{CookieManager, CookiePolicy, URI}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

class MultikinoClient(http: HttpFetch = MultikinoClient.DefaultFetch) {

  import MultikinoClient.{ApiUrl, BaseUrl}

  def fetch(): Seq[CinemaMovie] = parseFilms(http.get(ApiUrl))

  private def parseFilms(json: String): Seq[CinemaMovie] =
    (Json.parse(json) \ "result").as[JsArray].value.map(parseFilm).toSeq

  private def parseFilm(film: JsValue): CinemaMovie = {
    val title       = (film \ "filmTitle").as[String].replaceFirst("^Kino na obcasach:\\s*", "")
    val multikinoId = (film \ "filmId").asOpt[String].filter(_.nonEmpty)
    val mxcId       = (film \ "movieXchangeCode").asOpt[String].filter(_.nonEmpty)
    val sessions    = (film \ "showingGroups").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                        .flatMap(g => (g \ "sessions").as[JsArray].value)
    CinemaMovie(
      movie       = Movie(
        title          = title,
        runtimeMinutes = (film \ "runningTime").asOpt[Int],
        releaseYear    = (film \ "releaseDate").asOpt[String].flatMap(parseYear)
      ),
      cinema      = Multikino,
      posterUrl   = (film \ "posterImageSrc").asOpt[String].filter(_.nonEmpty),
      filmUrl     = absoluteUrl((film \ "filmUrl").asOpt[String]),
      synopsis    = (film \ "synopsisShort").asOpt[String].filter(_.nonEmpty),
      cast        = (film \ "cast").asOpt[String].filter(_.nonEmpty),
      director    = (film \ "director").asOpt[String].filter(_.nonEmpty),
      showtimes   = sessions.flatMap(parseSession).toSeq,
      externalIds = (multikinoId.map("mk" -> _) ++ mxcId.map("mxc" -> _)).toMap
    )
  }

  private def parseSession(session: JsValue): Option[Showtime] =
    (session \ "startTime").asOpt[String].map { startTime =>
      val attrs = (session \ "attributes").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                    .flatMap(a => (a \ "name").asOpt[String]).toSet
      Showtime(
        dateTime   = LocalDateTime.parse(startTime),
        bookingUrl = absoluteUrl((session \ "bookingUrl").asOpt[String]),
        room       = (session \ "screenName").asOpt[String].filter(_.nonEmpty),
        format     = parseFormat(attrs)
      )
    }

  // Multikino tags Language attributes as DUBBING / NAPISY / JĘZYK ORYGINALNY.
  // Normalise to DUB / NAP tokens (other clients use the same); original-language
  // screenings get no language tag (matches Cinema City's behaviour).
  private def parseFormat(attrs: Set[String]): List[String] = List(
    attrs.find(n => n == "2D" || n == "3D"),
    if (attrs.contains("DUBBING"))     Some("DUB")
    else if (attrs.contains("NAPISY")) Some("NAP")
    else None
  ).flatten

  private def absoluteUrl(url: Option[String]): Option[String] =
    url.filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else BaseUrl + u)

  private def parseYear(date: String): Option[Int] =
    Try(java.time.LocalDate.parse(date.take(10)).getYear).toOption
}

object MultikinoClient extends Logging {

  val BaseUrl = "https://www.multikino.pl"
  val ApiUrl  = s"$BaseUrl/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"

  private val HomeUrl        = s"$BaseUrl/"
  private val ScrapingAntUrl = "https://api.scrapingant.com/v2/general"
  private val UserAgent      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
  private val AcceptLang     = "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7"

  object DefaultFetch extends HttpFetch {
    private val scrapingAntKey: Option[String] = Env.get("SCRAPINGANT_KEY")

    private val httpClient = HttpClient.newBuilder()
      .version(HttpClient.Version.HTTP_1_1)
      .followRedirects(HttpClient.Redirect.NORMAL)
      .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
      .build()

    override def get(url: String): String =
      scrapingAntKey.fold(directGet(url))(fetchViaScrapingAnt(url, _))

    // ── ScrapingAnt path ──────────────────────────────────────────────────
    //
    // The Multikino API responds 401 without the `microservicesToken` JWT cookie
    // that the homepage hands out. So we two-step: ScrapingAnt → homepage to
    // capture every set-cookie, then ScrapingAnt → API with `cookies=` URL
    // parameter. `browser=false` keeps the response raw — the headless browser
    // otherwise wraps JSON in <pre>, costs more credits, and gets caught by
    // anti-bot detection.

    private def fetchViaScrapingAnt(targetUrl: String, key: String): String = {
      val cookies = readSetCookieHeaders(httpClient.send(
        scrapingAntRequest(HomeUrl, key, ""),
        HttpResponse.BodyHandlers.discarding()
      ))
      val response = httpClient.send(
        scrapingAntRequest(targetUrl, key, s"&cookies=${urlEncode(cookies)}"),
        HttpResponse.BodyHandlers.ofString()
      )
      if (!isUsable(response))
        throw new RuntimeException(
          s"ScrapingAnt API response unusable: status=${response.statusCode()}, " +
          s"body=${response.body().length}B, head='${response.body().take(120).replace('\n', ' ')}'"
        )
      response.body()
    }

    private def scrapingAntRequest(targetUrl: String, key: String, extraParams: String) =
      HttpRequest.newBuilder()
        .uri(URI.create(s"$ScrapingAntUrl?url=${urlEncode(targetUrl)}&proxy_country=pl&browser=false$extraParams"))
        .header("x-api-key", key)
        .header("Accept", "application/json, text/plain, */*")
        .GET()
        .build()

    private def readSetCookieHeaders(response: HttpResponse[_]): String =
      response.headers().allValues("set-cookie").asScala
        .map(_.split(";", 2).head)
        .filter(_.nonEmpty)
        .mkString(";")

    // We only call out to the Multikino JSON API through this path, so a valid
    // response must (a) be 200, (b) have a body, (c) be JSON-shaped. Anti-bot
    // interstitials come back as `<!DOCTYPE html>…` and would otherwise crash
    // downstream in the JSON parser.
    private def isUsable(response: HttpResponse[String]): Boolean = {
      val body = response.body()
      response.statusCode() == 200 && body.nonEmpty && body.dropWhile(_.isWhitespace).startsWith("{")
    }

    // ── Direct path (no ScrapingAnt key, e.g. local dev) ──────────────────
    //
    // The API also responds 401 to direct datacenter requests until you've
    // touched the homepage. The shared CookieManager picks up the cookies
    // automatically, so the retry just sends the same API request again.

    private def directGet(url: String): String = {
      val first = httpClient.send(apiRequest(url), HttpResponse.BodyHandlers.ofString())
      first.statusCode() match {
        case 200       => first.body()
        case 401 | 403 =>
          httpClient.send(homepageRequest(), HttpResponse.BodyHandlers.discarding())
          val retry = httpClient.send(apiRequest(url), HttpResponse.BodyHandlers.ofString())
          if (retry.statusCode() != 200)
            throw new RuntimeException(s"API returned ${retry.statusCode()} after session refresh")
          retry.body()
        case code      => throw new RuntimeException(s"API returned $code")
      }
    }

    private def apiRequest(url: String) = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", UserAgent)
      .header("Accept", "application/json, text/plain, */*")
      .header("Accept-Language", AcceptLang)
      .header("Referer", s"$BaseUrl/repertuar/poznan-stary-browar/teraz-gramy")
      .GET()
      .build()

    private def homepageRequest() = HttpRequest.newBuilder()
      .uri(URI.create(HomeUrl))
      .header("User-Agent", UserAgent)
      .header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      .header("Accept-Language", AcceptLang)
      .GET()
      .build()

    private def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
  }

  def fetch(): Seq[CinemaMovie] = new MultikinoClient().fetch()
}
