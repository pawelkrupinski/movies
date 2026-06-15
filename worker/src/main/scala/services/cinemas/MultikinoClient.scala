package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import tools.{Env, HttpFetch}

import scala.concurrent.duration._

/**
 * Multikino fetches `microservice/showings/cinemas/0011/films` and hands the
 * JSON to [[MultikinoParser]]. The HTTP path is wired at the composition
 * root by `MultikinoClient.fetchFor(direct)` as a fallback chain:
 *
 *   Zyte (if `ZYTE_API_KEY` set) → `direct`
 *
 * Each link tries the next on any exception, so Zyte going down rolls over to
 * `direct` without code changes. In production Zyte is REQUIRED, not an
 * optimisation: Multikino sits behind Cloudflare and 403s our Fly datacenter IP
 * on the homepage itself — proven 2026-06-15, the `direct` leg never even
 * reaches the API, so it's a last-resort that only succeeds from a
 * residential/dev IP. (See the `reference_multikino_fly_ip_cloudflare_block`
 * memory.)
 *
 * Session-handling retry stays in the client: optimistic API call, homepage
 * warm-up on failure, retry. That recovers the 401-without-session-cookie the
 * API returns on a cold connection — which is the second wall, *behind* the IP
 * block. With Zyte in front the first call already succeeds (it does its own
 * cookie carryover, shared across cinemas — see [[ZyteFallback]]) so the retry
 * rarely fires.
 */
class MultikinoClient(
  http:              HttpFetch,
  cinemaId:          String = MultikinoClient.PoznanStaryBrowarId,
  override val cinema: Cinema = Multikino,
) extends CinemaScraper {
  import MultikinoClient._

  private val apiUrl = MultikinoClient.apiUrl(cinemaId)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def chain: Boolean = true

  // Served through the metered Zyte residential proxy (above), so refresh on a
  // longer cadence than ordinary directly-scraped venues to cut the proxy bill —
  // see CinemaScraper.scrapeFreshness. Tunable via KINOWO_MULTIKINO_FRESHNESS_MINUTES.
  override def scrapeFreshness: FiniteDuration =
    Env.positiveLong(MultikinoClient.FreshnessMinutesKey, 60L).minutes

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

  /** Env knob for the per-source scrape cadence (minutes). Longer than the
   *  ordinary-venue default because every Multikino scrape is a paid Zyte
   *  residential-proxy call — see `scrapeFreshness`. */
  val FreshnessMinutesKey = "KINOWO_MULTIKINO_FRESHNESS_MINUTES"

  /** Build the `HttpFetch` to pass into `MultikinoClient` at the
   *  composition root. A Zyte-primary → direct fallback chain (see
   *  [[ZyteFallback]]); Multikino's API sits behind a session-cookie wall, so
   *  the Zyte leg warms a session from `HomeUrl` — once, then reused across all
   *  the cinema clients sharing this fetch (see [[SharedZyteSession]]). Tests
   *  override `Wiring.multikinoFetch` directly with `FakeHttpFetch`.
   */
  def fetchFor(direct: HttpFetch): HttpFetch =
    ZyteFallback.fetchFor(direct, cookieSource = Some(HomeUrl))
}
