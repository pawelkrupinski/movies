package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import tools.{Env, FallbackHttpFetch, HttpFetch}

import java.net.http.HttpClient
import java.time.Duration

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
   *  composition root. Composes a fallback chain of (Zyte, direct) —
   *  Zyte is included when its env var is set, so local dev without
   *  the key reverts to `direct` alone. Tests override
   *  `Wiring.multikinoFetch` directly with `FakeHttpFetch`.
   */
  def fetchFor(direct: HttpFetch): HttpFetch = {
    val zyte = Env.get("ZYTE_API_KEY").map { k =>
      "zyte" -> (new ZyteFetch(new ZyteClient(zyteHttpClient, k), HomeUrl): HttpFetch)
    }
    val chain = zyte.toSeq :+ ("direct" -> direct)
    if (chain.size == 1) chain.head._2 else new FallbackHttpFetch(chain)
  }

  private lazy val zyteHttpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .build()
}
