package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import tools.{Env, HttpFetch}

import java.net.http.HttpClient
import java.net.{CookieManager, CookiePolicy}

/**
 * Multikino fetches `microservice/showings/cinemas/0011/films` and hands the
 * JSON to [[MultikinoParser]]. Transport is delegated to either
 * [[ScrapingAntClient]] (when one is injected — required in production,
 * Multikino blocks datacenter IPs) or to the injected [[HttpFetch]] with a
 * one-shot homepage-then-API retry for cookie acquisition. The retry path
 * relies on the injected fetch persisting cookies between calls
 * (`RealHttpFetch` does, via a `CookieManager`).
 *
 * Session-handling lives here, not in a wrapper that callers compose
 * around the fetch, so the rules run identically in production
 * (`RealHttpFetch`) and in fixture tests (`FakeHttpFetch`) — there is no
 * parallel HTTP code path that tests can accidentally bypass.
 *
 * `scrapingAnt` is injected (composition root reads
 * [[MultikinoClient.scrapingAntFromEnv]]) so tests are hermetic — they
 * just pass `None`.
 */
class MultikinoClient(
  http:        HttpFetch,
  scrapingAnt: Option[ScrapingAntClient] = None,
) extends CinemaScraper {
  import MultikinoClient._

  val cinema: Cinema = Multikino

  def fetch(): Seq[CinemaMovie] = MultikinoParser.parse(getApi())

  private def getApi(): String =
    scrapingAnt.fold(directGet())(_.getWithCookies(ApiUrl, HomeUrl))

  // The API responds 401 to direct datacenter requests until you've
  // touched the homepage. The first attempt is optimistic — once the
  // session cookie is established on the underlying client, subsequent
  // ticks succeed without the warm-up. On failure, fetch the homepage
  // (its `Set-Cookie` lands in the underlying fetch's cookie store) and
  // retry the API.
  private def directGet(): String =
    try http.get(ApiUrl)
    catch { case _: Exception =>
      try http.get(HomeUrl) catch { case _: Exception => () }
      http.get(ApiUrl)
    }
}

object MultikinoClient {

  val BaseUrl = "https://www.multikino.pl"
  val ApiUrl  = s"$BaseUrl/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"

  private val HomeUrl = s"$BaseUrl/"

  /** Reads `SCRAPINGANT_KEY` from the environment. Used by the composition
   *  root (`Wiring`) to plug ScrapingAnt routing into production; tests
   *  skip this and pass `None`, keeping `.env.local` out of the spec
   *  execution. */
  def scrapingAntFromEnv: Option[ScrapingAntClient] =
    Env.get("SCRAPINGANT_KEY").map(k => new ScrapingAntClient(scrapingAntHttpClient, k))

  // ScrapingAnt has its own cookie-carryover protocol (read `Set-Cookie`
  // off the homepage response, pass via `&cookies=`) that needs raw
  // response headers — not expressible through the stateless
  // `HttpFetch.get` surface. So it keeps a dedicated client.
  private lazy val scrapingAntHttpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
    .build()
}
