package services.cinemas

import models.CinemaMovie
import tools.{Env, HttpFetch}

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{CookieManager, CookiePolicy, URI}

/**
 * Multikino fetches `microservice/showings/cinemas/0011/films` and hands the
 * JSON to [[MultikinoParser]]. Transport is delegated to either
 * [[ScrapingAntClient]] (when `SCRAPINGANT_KEY` is set — required in
 * production, Multikino blocks datacenter IPs) or to a direct `HttpClient`
 * with a one-shot homepage-then-API retry for cookie acquisition.
 */
class MultikinoClient(http: HttpFetch = MultikinoClient.DefaultFetch) {
  def fetch(): Seq[CinemaMovie] = MultikinoParser.parse(http.get(MultikinoClient.ApiUrl))
}

object MultikinoClient {

  val BaseUrl = "https://www.multikino.pl"
  val ApiUrl  = s"$BaseUrl/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"

  private val HomeUrl    = s"$BaseUrl/"
  private val UserAgent  = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
  private val AcceptLang = "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7"

  object DefaultFetch extends HttpFetch {
    // Shared `HttpClient` — its `CookieManager` is needed by the direct path
    // to carry the cookies the homepage sets into the subsequent API call.
    // (`ScrapingAntClient` is stateless across calls; each call fetches fresh
    // cookies.)
    private val httpClient = HttpClient.newBuilder()
      .version(HttpClient.Version.HTTP_1_1)
      .followRedirects(HttpClient.Redirect.NORMAL)
      .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
      .build()

    private val scrapingAnt: Option[ScrapingAntClient] =
      Env.get("SCRAPINGANT_KEY").map(k => new ScrapingAntClient(httpClient, k))

    override def get(url: String): String =
      scrapingAnt.fold(directGet(url))(_.getWithCookies(url, HomeUrl))

    // ── Direct path (no ScrapingAnt key, e.g. local dev) ──────────────────
    //
    // The API responds 401 to direct datacenter requests until you've touched
    // the homepage. The shared CookieManager picks up the cookies; the retry
    // just resends the same API request now that the session is established.
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
  }

  def fetch(): Seq[CinemaMovie] = new MultikinoClient().fetch()
}
