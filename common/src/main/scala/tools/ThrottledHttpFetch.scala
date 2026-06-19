package tools

import play.api.Logging

import java.net.URI
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration._

/**
 * A shared, per-host rate-limit gate wrapping any `HttpFetch`.
 *
 * On HTTP 429, a per-request retry alone makes things WORSE under concurrency:
 * every in-flight request independently backs off and retries, then re-converges
 * into the same burst (thundering herd) and trips the limit again. This decorator
 * coordinates instead — a 429 from a host sets a single shared `pausedUntil` for
 * THAT host, and every request to it parks on that gate before firing, so the
 * whole caller fleet (the ~12 concurrent TMDB callers across the enrichment
 * budget + the worker pool) stands down together and resumes once, honoring the
 * server's `Retry-After`. Per-host, so a TMDB pause never stalls Filmweb / cinema
 * fetches.
 *
 * Wire it CLOSEST to the wire (inside `MonitoringHttpFetch`) so its waits are
 * invisible to uptime. Only 429 is retried — every other status/exception
 * propagates immediately (404 is a real "not found" signal for several clients,
 * not something to retry). The wait between attempts IS the gate: the next
 * attempt parks on the freshly-set `pausedUntil`, so `Retry-After` is what paces
 * the retry. Persistent 429s past `maxAttempts` propagate the `HttpStatusException`
 * unchanged, so the resolve path still degrades to a transient miss (and the
 * staging give-up budget concludes it).
 *
 * The async path delegates directly — every TMDB call is synchronous
 * (`TmdbClient` uses `get`); `getAsync` is the cinema-scrape fan-out, not a
 * rate-limited path. `now`/`sleep`/`jitterMillis` are injectable for tests.
 */
class ThrottledHttpFetch(
  delegate:     HttpFetch,
  maxAttempts:  Int            = 4,
  defaultPause: FiniteDuration = 5.seconds,
  maxPause:     FiniteDuration = 60.seconds,
  jitterMillis: () => Long     = () => (scala.util.Random.nextDouble() * 250).toLong,
  now:          () => Instant  = () => Instant.now(),
  sleep:        Long => Unit   = Thread.sleep
) extends HttpFetch with Logging {

  private val pausedUntil = new ConcurrentHashMap[String, Instant]()

  private def hostOf(url: String): Option[String] =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase)

  /** Park until this host's gate elapses, plus a little jitter so the fleet
   *  doesn't resume in lockstep and immediately re-trip the limit. */
  private def awaitGate(host: String): Unit = Option(pausedUntil.get(host)).foreach { until =>
    val waitMs = java.time.Duration.between(now(), until).toMillis + jitterMillis()
    if (waitMs > 0) sleep(waitMs)
  }

  private def throttled[T](url: String)(block: => T): T = hostOf(url) match {
    case None       => block                              // unparseable host — nothing to gate on
    case Some(host) =>
      def attempt(n: Int): T = {
        awaitGate(host)
        try block
        catch {
          case e: HttpStatusException if e.code == 429 =>
            val pause = (e.retryAfter.getOrElse(defaultPause)).min(maxPause)
            pausedUntil.put(host, now().plusMillis(pause.toMillis))
            if (n >= maxAttempts) {
              logger.warn(s"HTTP 429 from $host — exhausted $maxAttempts attempts; propagating.")
              throw e
            }
            logger.warn(s"HTTP 429 from $host — pausing all calls to it for ${pause.toMillis}ms " +
              s"(attempt $n/$maxAttempts, Retry-After=${e.retryAfter.map(d => s"${d.toMillis}ms").getOrElse("absent")}).")
            attempt(n + 1)
        }
      }
      attempt(1)
  }

  override def get(url: String): String = throttled(url)(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = throttled(url)(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = throttled(url)(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String =
    throttled(url)(delegate.post(url, body, contentType))

  // Async = cinema-scrape fan-out, not a rate-limited TMDB path — bypass the gate.
  override def getAsync(url: String): java.util.concurrent.CompletableFuture[String] = delegate.getAsync(url)
}
