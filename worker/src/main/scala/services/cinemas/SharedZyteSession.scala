package services.cinemas

import tools.GetOnlyHttpFetch

import java.time.Clock
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/**
 * Reuses ONE warmed Zyte session across every fetch through it — the shared
 * `HttpFetch` Multikino's ~30 cinema clients all hold (one `mkFetch`, see
 * [[ZyteFallback]] / `MultikinoClient.fetchFor`).
 *
 * Multikino's films API sits behind a session-cookie wall, so each fetch needs
 * a session warmed from the homepage — and warming is a *second* paid Zyte
 * request. But the session cookie is domain-scoped: one warmed `multikino.pl`
 * session authorises every cinema id. So we warm once and reuse it for `ttl`,
 * turning the ~2 requests/scrape (warm + API) into ~1 across the fleet — the
 * cost lever in the `reference_multikino_fly_ip_cloudflare_block` memory.
 *
 * Safe by construction, so a wrong `ttl` guess can't break Multikino, only cost
 * a wasted call:
 *   - A fetch on a session Zyte/upstream has since expired throws (the API
 *     answers 401 → [[ZyteClient.fetchWithSession]] throws). We drop the cached
 *     session, re-warm, and retry the fetch ONCE.
 *   - A warm failure (the homepage itself blocked — e.g. served from our
 *     datacenter IP rather than Zyte's residential one) propagates, so the
 *     surrounding `FallbackHttpFetch` rolls to the `direct` leg exactly as the
 *     old per-call path did.
 *
 * Thread-safe: the warm runs under a lock so a burst of concurrent cinemas
 * warms once, not once each; the fetches themselves run outside the lock.
 */
class SharedZyteSession(
  client:          ZyteClient,
  cookieSourceUrl: String,
  ttl:             FiniteDuration,
  clock:           Clock = Clock.systemUTC()
) extends GetOnlyHttpFetch {

  // (sessionId, warmedAtMillis) — None until the first warm / after invalidation.
  private var current: Option[(String, Long)] = None
  private val lock = new Object

  override def get(targetUrl: String): String =
    try client.fetchWithSession(targetUrl, sessionId())
    catch {
      case _: Exception =>
        // The session likely died upstream (401) — drop it, re-warm, retry once.
        // A second failure propagates to the FallbackHttpFetch → direct leg.
        invalidate()
        client.fetchWithSession(targetUrl, sessionId())
    }

  /** The current live session id — reused while inside `ttl`, otherwise a fresh
   *  warm. Synchronised so concurrent callers warm once and then share it. */
  private def sessionId(): String = lock.synchronized {
    val now = clock.millis()
    current match {
      case Some((id, warmedAt)) if now - warmedAt < ttl.toMillis => id
      case _ =>
        val id = UUID.randomUUID().toString
        client.warm(cookieSourceUrl, id)
        current = Some((id, now))
        id
    }
  }

  private def invalidate(): Unit = lock.synchronized { current = None }
}
