package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import tools.HttpFetch

/**
 * Multikino fetches `microservice/showings/cinemas/0011/films` and hands the
 * JSON to [[MultikinoParser]]. The HTTP path is wired at the composition
 * root by `MultikinoClient.fetchFor(direct)` as a fallback chain:
 *
 *   Zyte (if `ZYTE_API_KEY` set) → `direct`
 *
 * Each link tries the next on any exception, so Zyte going down silently
 * rolls over to direct without code changes.
 *
 * Session-handling retry stays in the client: optimistic API call,
 * homepage warm-up on failure, retry. With Zyte in front the first call
 * always succeeds (it does its own cookie carryover) and the retry never
 * fires; with a direct fetch the retry recovers the 401-without-session-
 * cookie response that Multikino's API returns on a cold connection.
 */
class MultikinoClient(
  http:              HttpFetch,
  cinemaId:          String = MultikinoClient.PoznanStaryBrowarId,
  override val cinema: Cinema = Multikino,
) extends CinemaScraper {
  import MultikinoClient._

  private val apiUrl = MultikinoClient.apiUrl(cinemaId)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = MultikinoParser.parse(getApiWithRetry(), cinema)

  private def getApiWithRetry(): String =
    try http.get(apiUrl)
    catch { case _: Exception =>
      try http.get(HomeUrl) catch { case _: Exception => () }
      http.get(apiUrl)
    }
}

object MultikinoClient {

  val BaseUrl = "https://www.multikino.pl"

  /** Multikino's microservice cinema ids are 4-digit zero-padded numbers.
   *  Poznań Stary Browar is `0011`; the other cities pass their own id. */
  val PoznanStaryBrowarId = "0011"

  def apiUrl(cinemaId: String): String =
    s"$BaseUrl/api/microservice/showings/cinemas/$cinemaId/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"

  /** Poznań's API URL — kept as a named constant because the session-handling
   *  spec drives `fetch()` against it directly. */
  val ApiUrl  = apiUrl(PoznanStaryBrowarId)
  val HomeUrl = s"$BaseUrl/"

  /** Build the `HttpFetch` to pass into `MultikinoClient` at the
   *  composition root. A Zyte-primary → direct fallback chain (see
   *  [[ZyteFallback]]); Multikino's API sits behind a session-cookie wall, so
   *  the Zyte leg warms a session from `HomeUrl` first. Tests override
   *  `Wiring.multikinoFetch` directly with `FakeHttpFetch`.
   */
  def fetchFor(direct: HttpFetch): HttpFetch =
    ZyteFallback.fetchFor(direct, cookieSource = Some(HomeUrl))
}
