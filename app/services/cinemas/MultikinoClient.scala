package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import tools.{Env, HttpFetch}

import java.net.http.HttpClient
import java.net.{CookieManager, CookiePolicy}

/**
 * Multikino fetches `microservice/showings/cinemas/0011/films` and hands the
 * JSON to [[MultikinoParser]]. Whether the call goes through ScrapingAnt
 * (production, where Multikino blocks datacenter IPs) or directly
 * (`RealHttpFetch` locally, `FakeHttpFetch` in tests) is a decision made
 * once at the composition root — `MultikinoClient.fetchFor(direct)` returns
 * a single `HttpFetch` that already encapsulates the routing. The client
 * itself doesn't know.
 *
 * Session-handling retry stays in the client: optimistic API call,
 * homepage warm-up on failure, retry. With the ScrapingAnt wrapper in
 * place the first call always succeeds (ScrapingAnt does its own cookie
 * carryover) and the retry never fires; with a direct fetch the retry
 * recovers the 401-without-session-cookie response that Multikino's API
 * returns on a cold connection.
 */
class MultikinoClient(http: HttpFetch) extends CinemaScraper {
  import MultikinoClient._

  val cinema: Cinema = Multikino

  def fetch(): Seq[CinemaMovie] = MultikinoParser.parse(getApiWithRetry())

  private def getApiWithRetry(): String =
    try http.get(ApiUrl)
    catch { case _: Exception =>
      try http.get(HomeUrl) catch { case _: Exception => () }
      http.get(ApiUrl)
    }
}

object MultikinoClient {

  val BaseUrl = "https://www.multikino.pl"
  val ApiUrl  = s"$BaseUrl/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"
  val HomeUrl = s"$BaseUrl/"

  /** Build the `HttpFetch` to pass into `MultikinoClient` at the
   *  composition root. Routes through ScrapingAnt when `SCRAPINGANT_KEY`
   *  is set (production); falls back to `direct` otherwise (local dev /
   *  tests). The returned fetch is the *single* injection point — test
   *  wirings override `Wiring.multikinoFetch` with their own (typically
   *  `FakeHttpFetch`) and the routing decision drops out of the test
   *  surface entirely.
   */
  def fetchFor(direct: HttpFetch): HttpFetch =
    Env.get("SCRAPINGANT_KEY")
      .map(k => new ScrapingAntFetch(new ScrapingAntClient(scrapingAntHttpClient, k, proxyCountry = ProxyCountry), HomeUrl))
      .getOrElse(direct)

  // `proxy_country` for the ScrapingAnt route. The PL datacenter pool started
  // returning 423 ("Our browser was detected by target site") on every
  // retry — same IP class flagged on each attempt, so the in-client
  // exponential backoff couldn't recover. Picking a different country picks
  // a different IP pool that hasn't been scored yet. Multikino's API isn't
  // geo-fenced — the response shape is identical regardless of origin
  // country. Revisit if `de` itself starts getting blocked.
  private val ProxyCountry = "de"

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
