package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import tools.{Env, FallbackHttpFetch, HttpFetch}

import java.net.http.HttpClient
import java.net.{CookieManager, CookiePolicy}
import java.time.Duration

/**
 * Multikino fetches `microservice/showings/cinemas/0011/films` and hands the
 * JSON to [[MultikinoParser]]. The HTTP path is wired at the composition
 * root by `MultikinoClient.fetchFor(direct)` as a fallback chain:
 *
 *   Zyte (if `ZYTE_API_KEY` set)
 *     → ScrapingAnt (if `SCRAPINGANT_KEY` set)
 *       → `direct`
 *
 * Each link tries the next on any exception, so one backend going down
 * (Zyte 5xx, ScrapingAnt rate-limited, …) silently rolls over to the
 * next without code changes. The client itself doesn't know which
 * backend served any given request.
 *
 * Session-handling retry stays in the client: optimistic API call,
 * homepage warm-up on failure, retry. With Zyte/ScrapingAnt in front
 * the first call always succeeds (both wrappers do their own cookie
 * carryover) and the retry never fires; with a direct fetch the retry
 * recovers the 401-without-session-cookie response that Multikino's
 * API returns on a cold connection.
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
   *  composition root. Composes a fallback chain of (Zyte, ScrapingAnt,
   *  direct) — each link is only included when its env var is set, so
   *  local dev with neither key reverts to `direct` alone. Tests
   *  override `Wiring.multikinoFetch` directly with `FakeHttpFetch` and
   *  this whole construction sits behind an env check that never fires.
   */
  def fetchFor(direct: HttpFetch): HttpFetch = {
    val zyte = Env.get("ZYTE_API_KEY").map { k =>
      "zyte" -> (new ZyteFetch(new ZyteClient(zyteHttpClient, k), HomeUrl): HttpFetch)
    }
    val scrapingAnt = Env.get("SCRAPINGANT_KEY").map { k =>
      "scrapingAnt" -> (new ScrapingAntFetch(
        new ScrapingAntClient(scrapingAntHttpClient, k, proxyCountry = () => ScrapingAntClient.randomCountry()),
        HomeUrl
      ): HttpFetch)
    }
    val chain = (zyte.toSeq ++ scrapingAnt.toSeq) :+ ("direct" -> direct)
    if (chain.size == 1) chain.head._2 else new FallbackHttpFetch(chain)
  }

  // Both Zyte and ScrapingAnt have their own cookie-carryover protocols
  // that need raw response handling, so each keeps a dedicated underlying
  // `HttpClient`. Zyte does the protocol entirely server-side (we just
  // POST with a session.id); ScrapingAnt requires reading Set-Cookie off
  // the homepage response and forwarding via &cookies= — neither is
  // expressible through the stateless `HttpFetch.get` surface.
  private lazy val zyteHttpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    // Zyte calls take ~3-5s per request (warmup + API ≈ 7-10s end-to-end);
    // a stuck connection would otherwise hang the cinema-refresh tick.
    .connectTimeout(Duration.ofSeconds(15))
    .build()

  private lazy val scrapingAntHttpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
    .build()
}
