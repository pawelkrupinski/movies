package tools

/**
 * Wraps a fetch so a failed GET first warms a session by hitting `warmUrl`, then
 * retries the original GET once. For Multikino through the residential proxy:
 * several locations' films API returns 401 on a cold call (and the JVM's proxy
 * Authenticator surfaces that as `IOException: WWW-Authenticate header missing`),
 * but 200 once the homepage has set a session cookie on that egress IP — verified
 * 2026-06-16. Warming inside the proxy leg lets it serve via the proxy instead of
 * the surrounding FallbackHttpFetch rolling to Zyte.
 *
 * Two requirements on `delegate`, both met by a proxied [[RealHttpFetch]] pinned
 * to one IP: it keeps a cookie jar across calls (so the warm's cookie rides the
 * retry), and it uses a STICKY egress (Multikino's session cookie is IP-bound, so
 * a rotating proxy would warm on one IP and retry on another → still 401).
 */
class SessionWarmingHttpFetch(delegate: HttpFetch, warmUrl: String) extends GetOnlyHttpFetch {

  override def get(url: String): String = warmAndRetry(delegate.get(url))

  override def getBytes(url: String): Array[Byte] = warmAndRetry(delegate.getBytes(url))

  private def warmAndRetry[T](attempt: => T): T =
    try attempt
    catch {
      case _: Exception =>
        try delegate.get(warmUrl) catch { case _: Exception => () } // best-effort warm
        attempt
    }
}
