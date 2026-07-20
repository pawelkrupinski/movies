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
  // The ceiling on how long a single 429 parks a host. This pause is SHARED per
  // host ("pausing all calls to it"), so it is NOT just the offending request's
  // backoff — it freezes the whole fleet's calls to that host for its duration.
  // A brief raise to 10min (to "honor" Flicks' Retry-After: 300-600s) backfired
  // badly: with the 200ms pace keeping Flicks ~99% clean, a 429 is transient noise,
  // yet each one froze ALL flicks.co.uk scraping for a full 10min, and a fresh 429
  // landed within 1-3min of every resume — so UK scraping stalled on an ~11-14min
  // cadence (ScrapeChunk task-duration pegged the 120s histogram ceiling, throughput
  // collapsed). Since obeying the long ask neither prevents the next 429 nor is
  // worth freezing everything for, cap at 60s: a real backoff that keeps the shared
  // freeze short. TMDB and other small-Retry-After hosts sit well under it, unchanged.
  maxPause:     FiniteDuration = 60.seconds,
  jitterMillis: () => Long     = () => (scala.util.Random.nextDouble() * 250).toLong,
  now:          () => Instant  = () => Instant.now(),
  sleep:        Long => Unit   = Thread.sleep,
  summaryInterval: FiniteDuration = 5.minutes,
  // Where the pace-report goes. Defaults to this class's logger; injected in
  // tests so the summary can be asserted on as data instead of scraped out of
  // captured log output.
  report:       Option[String => Unit] = None
) extends HttpFetch with Logging {

  private val pausedUntil = new ConcurrentHashMap[String, Instant]()

  // Per-host {requests, 429s} since that host's last summary. Individual 429s
  // are already logged below, but a raw count has no denominator — 3,000 of them
  // reads as catastrophic or negligible depending on a total the log never
  // carried. Tuning a pace empirically needs the RATE, so accumulate both and
  // emit one summary per host per `summaryInterval`.
  private val stats = new ConcurrentHashMap[String, HostCallStats]()

  private def hostOf(url: String): Option[String] =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase)

  /** Count one call (and whether it drew a 429) and, once per `summaryInterval`,
   *  log that host's clean-rate. Summarising on the request path rather than from
   *  a timer keeps this decorator thread-free; a host that stops being called
   *  simply stops reporting, which is the honest signal anyway. */
  private def record(host: String, throttled: Boolean): Unit = {
    val s = stats.computeIfAbsent(host, _ => new HostCallStats(now().toEpochMilli))
    // Flush the elapsed window BEFORE counting this call, so each summary covers
    // exactly its own interval and the call that trips the boundary opens the
    // next one rather than being double-counted at the edge.
    val nowMs = now().toEpochMilli
    val since = s.lastSummaryMs.get()
    if (nowMs - since >= summaryInterval.toMillis && s.lastSummaryMs.compareAndSet(since, nowMs)) {
      val total = s.requests.getAndSet(0)
      val t429  = s.throttled.getAndSet(0)
      val clean = if (total == 0) 100.0 else 100.0 * (total - t429) / total
      val pace  = RateLimitedHttpFetch.configuredInterval(s"https://$host/")
        .map(d => s"${d.toMillis}ms").getOrElse("unpaced")
      val msg = f"pace-report $host: $total%d requests, $t429%d throttled (429), $clean%.1f%% clean, pace=$pace"
      report.fold(logger.info(msg))(_(msg))
    }
    s.requests.incrementAndGet()
    if (throttled) s.throttled.incrementAndGet()
    ()
  }

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
        try {
          val result = block
          record(host, throttled = false)
          result
        }
        catch {
          case e: HttpStatusException if e.code == 429 =>
            record(host, throttled = true)
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

/** One host's call tally between summaries. Mutable and thread-safe by design —
 *  every paced request touches it from the worker pool. */
private final class HostCallStats(startedMs: Long) {
  val requests      = new java.util.concurrent.atomic.AtomicLong(0)
  val throttled     = new java.util.concurrent.atomic.AtomicLong(0)
  val lastSummaryMs = new java.util.concurrent.atomic.AtomicLong(startedMs)
}
