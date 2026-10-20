package tools

import play.api.Logging

import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * ONE RATE BUDGET PER SERVICE FOR EVERY JOB ON A SHARED EGRESS ADDRESS. Off github.com every CI job leaves through
 * ci-runner's one public IP, and TMDB, IMDb & co. limit per source IP: five recording legs each pacing only itself
 * are five times the rate the provider sees (six at once drew ~70k TMDB 429s, Forgejo run 322). The runner serves a
 * permit service (bitcashier infra, nix/files/forgejo-rate-permits) and names it in `FORGEJO_RATE_PERMITS`; a
 * recording or fill asks it for a permit for the request's [[ExternalService]] before each attempt on the wire, so
 * the legs share one bucket per service between them.
 *
 * CI-only by construction: this lives in the worker's `fixtures` config, which production code cannot see, and is
 * wired only into the recording's live wire ([[ArchiveReplayWiring]]) and the fill's (`scripts.FillMissingFixtures`).
 * With the variable unset (github.com, a laptop) [[RatePermits.over]] hands the wire back unchanged.
 */
trait RatePermits {

  /** Returns once a request to `service` may go out. */
  def acquire(service: String): Unit
}

object RatePermits {

  /** The permit service the process's configuration names, if it names one. */
  def from(service: Option[settings.RatePermitService]): Option[RatePermits] = service.map(s => new HttpRatePermits(s.value))

  /** `wire`, asking `permits` before every request when there are any; `wire` itself otherwise. */
  def over(wire: HttpFetch, permits: Option[RatePermits]): HttpFetch = permits.fold(wire)(new RatePermittedHttpFetch(wire, _))
}

/**
 * Asks for a permit before each request to a third-party API, then makes it. Wired at the LEAF, under the 429 gate
 * and the breaker, so every attempt -- each 429 retry included -- takes a permit of its own, and a breaker's fast-fail
 * (which never reaches the wire) takes none. A cinema site or any host no [[ExternalService]] names is not asked
 * for: those are paced per host already, and no provider limits them as one.
 */
final class RatePermittedHttpFetch(delegate: HttpFetch, permits: RatePermits) extends ForwardingHttpFetch {

  protected def around[T](url: String)(call: HttpFetch => T): T = {
    ExternalService.ofUrl(url).filter(ExternalService.apis.contains).foreach(permits.acquire)
    call(delegate)
  }
}

/**
 * The permit service over HTTP: `GET <base>/permit?service=<name>` blocks until a permit is granted (200). A 429 is
 * the service saying the next permit is further off than it will hold a request open for, so it is asked again after
 * `busyPause`. Anything else -- the service down, an unexpected status -- lets the request go out unpermitted (said
 * once): the provider's own 429 and [[ThrottledHttpFetch]] remain behind it, and a recording must not stop because
 * its pacer did.
 */
final class HttpRatePermits(
  base:      String,
  client:    HttpClient     = HttpRatePermits.client,
  busyPause: FiniteDuration = 1.second,
  sleep:     Long => Unit   = Thread.sleep
) extends RatePermits with Logging {

  private val warned = new AtomicBoolean(false)

  /** Longer than the service's own longest hold (30 s), so a granted permit is never cut off on its way back. */
  private val requestTimeout = java.time.Duration.ofSeconds(90)

  def acquire(service: String): Unit = {
    val request = HttpRequest
      .newBuilder(java.net.URI.create(s"${base.stripSuffix("/")}/permit?service=${URLEncoder.encode(service, StandardCharsets.UTF_8)}"))
      .timeout(requestTimeout)
      .GET()
      .build()
    @tailrec def ask(): Unit = Try(client.send(request, HttpResponse.BodyHandlers.discarding()).statusCode()) match {
      case Success(200) => ()
      case Success(429) =>
        sleep(busyPause.toMillis)
        ask()
      case Success(status) => unpermitted(service, s"HTTP $status")
      case Failure(e)      => unpermitted(service, e.toString)
    }
    ask()
  }

  private def unpermitted(service: String, why: String): Unit =
    if (warned.compareAndSet(false, true))
      logger.warn(s"rate permits at $base unavailable ($why) for $service -- requests go out unpermitted; said once.")
}

object HttpRatePermits {

  /** One client for every wiring a process builds (a convergence leg builds one per pass), so one selector thread. */
  lazy val client: HttpClient = HttpClient.newBuilder().connectTimeout(java.time.Duration.ofSeconds(5)).build()
}
