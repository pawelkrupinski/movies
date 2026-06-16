package tools

import java.util.concurrent.{CompletableFuture, Semaphore}

/**
 * Bounds the number of concurrent calls through `delegate` with a shared
 * semaphore: a call BLOCKS until a permit frees, then proceeds. It never fails
 * or falls back on contention — it WAITS.
 *
 * This caps concurrent Decodo proxy authentications at the account's
 * concurrent-auth sub-limit ("too many authentication attempts. Limit: 3",
 * 2026-06-16). Spreading venues across IPs ([[StickyShardHttpFetch]]) didn't help
 * because the cap is on simultaneous authentications, not per-IP load — so the
 * burst of new proxy tunnels a scrape tick opens has to be throttled, not just
 * distributed.
 *
 * Crucially the wait happens INSIDE the proxy leg, before the surrounding
 * FallbackHttpFetch would reach Zyte: contention queues on the proxy instead of
 * spilling to the paid Zyte egress. Zyte stays reserved for genuine proxy
 * failures (a burned IP, a 401/407). The semaphore is shared across every
 * proxied client so the cap is account-wide. Fair acquisition avoids starving
 * any one waiter.
 */
class ConcurrencyLimitedHttpFetch(delegate: HttpFetch, permits: Semaphore) extends HttpFetch {

  private def limited[T](body: => T): T = {
    permits.acquire()
    try body
    finally permits.release()
  }

  override def get(url: String): String = limited(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = limited(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = limited(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String =
    limited(delegate.post(url, body, contentType))

  // Route async through the same blocking gate so an async caller is bounded too;
  // the proxied clients call `get` synchronously, so this is the rare path.
  override def getAsync(url: String): CompletableFuture[String] =
    CompletableFuture.supplyAsync(() => limited(delegate.get(url)))
}
