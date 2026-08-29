package tools

import play.api.Logging

import scala.collection.mutable

/**
 * Tries each backend in order, returning the first successful body.
 * A backend that fails is recorded at DEBUG and the chain falls through
 * to the next one; only when every backend has failed does it warn, and
 * throw a single composite exception naming each failure.
 *
 * Used by Multikino's composition: Zyte primary → direct fetch as
 * last resort. None of the backends know about each other — each is
 * just an `HttpFetch`.
 *
 * Single-backend lists are a degenerate case: prefer just using that
 * backend directly. Empty lists are a wiring bug — throws at
 * construction so misconfigured chains don't fail silently in
 * production on the first request.
 */
class FallbackHttpFetch(
  backends:  Seq[(String, HttpFetch)],
  // Fired per backend attempt with (name, error): None on success, Some(message)
  // on failure. Lets a caller meter which leg served vs fell through — the worker
  // records the "proxy" leg's outcome to the UptimeMonitor so /uptime shows when
  // the residential proxy served vs fell back to Zyte. Must not throw (guarded).
  onOutcome: (String, Option[String]) => Unit = FallbackHttpFetch.NoOutcome
) extends HttpFetch with Logging {
  require(backends.nonEmpty, "FallbackHttpFetch needs at least one backend")

  override def get(url: String): String = tryEach("get", url, _.get(url))

  // Headers must ride the chain — must NOT inherit the base default
  // (`get(url, headers) = get(url)`), which silently DROPS them. Odeon
  // authenticates its Vista ocapi with an `Authorization: Bearer`, so inheriting
  // the default sent every proxied Odeon call unauthenticated: the origin answered
  // 401, the JVM's proxy Authenticator surfaced that as `IOException:
  // WWW-Authenticate header missing`, the chain rolled to the direct leg, and that
  // leg 403'd on the Cloudflare block the proxy existed to clear (2026-08-29). The
  // gap was invisible until then because every other proxied source authenticates
  // with a cookie or nothing at all.
  override def get(url: String, headers: Map[String, String]): String =
    tryEach("get", url, _.get(url, headers))

  // Raw bytes through the same fallback chain — must NOT inherit the lossy base
  // default (`get(url).getBytes(UTF_8)`), which would mojibake a legacy
  // single-byte page fetched through this chain.
  override def getBytes(url: String): Array[Byte] = tryEach("getBytes", url, _.getBytes(url))

  override def post(url: String, body: String, contentType: String): String =
    tryEach("post", url, _.post(url, body, contentType))

  private def tryEach[T](verb: String, url: String, call: HttpFetch => T): T = {
    val failures        = mutable.ListBuffer.empty[String]
    var lastFailure     = Option.empty[Throwable]
    var result: Option[T] = None
    val it              = backends.iterator
    while (result.isEmpty && it.hasNext) {
      val (name, backend) = it.next()
      try {
        result = Some(call(backend))
        safeOutcome(name, None)
      } catch {
        case t: Throwable =>
          lastFailure = Some(t)
          val message = s"$name: ${t.getClass.getSimpleName}: ${t.getMessage}"
          safeOutcome(name, Some(message))
          // DEBUG, not WARN: falling through is what a fallback chain is FOR, and a
          // chain that then answers has nothing wrong with it. At warning volume the
          // convergence legs emitted thousands of nine-line misses per run — every
          // film that never resolves misses the fixture backend by construction, since
          // a 404 leaves no response to record — on runs making no network calls at
          // all. It read as a broken cache, was reported as one, and drowned the
          // warnings that meant something. The detail is kept for whoever is actually
          // diagnosing a fall-through, and every failure is still named in the warning
          // below if the chain runs out of backends.
          logger.debug(s"FallbackHttpFetch $verb $url — $message; trying next backend")
          failures += message
      }
    }
    result.getOrElse {
      val detail = s"All ${backends.size} backends failed for $verb $url:\n  " + failures.mkString("\n  ")
      // A definitive "not found" from the LAST backend is an ANSWER, and it has to
      // reach the caller as one. Wrapping it in a composite `RuntimeException` hid
      // it twice over: the message now begins "All N backends failed", so
      // `EnrichmentRead`'s `^HTTP <code>` test cannot see the 404 and books it as a
      // failed read. That is fatal to a slug-probe ladder — Metacritic and Rotten
      // Tomatoes try ~20 candidates of which at most one exists, so treating the
      // first losing probe as an outage aborts the whole ladder. A convergence leg
      // measured it exactly: Metacritic 17 and RT 73 against production's 308 and
      // 354, while IMDb (keyed by id, no ladder) was untouched.
      //
      // Only the LAST backend's verdict qualifies. An earlier leg missing (no
      // fixture recorded) says nothing about the resource; the leg that actually
      // reached the upstream is the one whose answer this is.
      lastFailure.filter(EnrichmentRead.isAbsent).foreach { absent =>
        logger.debug(s"FallbackHttpFetch $verb $url — every backend failed and the last says NOT FOUND; " +
                     s"propagating that rather than a composite failure")
        throw absent
      }
      // NOW it is a warning: nothing answered. Logged as well as thrown because
      // callers routinely catch this to degrade gracefully, and a swallowed
      // exception is how a chain fails silently.
      logger.warn(s"FallbackHttpFetch $detail")
      throw new RuntimeException(detail)
    }
  }

  // A metering callback must never break the fetch it's observing.
  private def safeOutcome(name: String, error: Option[String]): Unit =
    try onOutcome(name, error) catch { case _: Throwable => () }
}

object FallbackHttpFetch {
  val NoOutcome: (String, Option[String]) => Unit = (_, _) => ()
}
