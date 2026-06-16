package tools

import java.util.concurrent.atomic.AtomicInteger

/**
 * Hands each proxied client its OWN sticky egress IP from the Decodo pool,
 * round-robin across the pool's ports.
 *
 * Spreading is the point. Funnelling every proxied client (Multikino + all the
 * biletyna venues, ~50 fetches per 15-min tick) through a SINGLE egress IP
 * tripped Decodo's concurrent-auth cap — `too many authentication attempts.
 * Limit: 3` — and rolled all proxied traffic to the Zyte fallback (2026-06-16).
 * One pinned IP per client keeps each STICKY (Multikino's session cookie is
 * IP-bound, so its homepage-warm → API retry must share an egress) while
 * distributing the auth load across the pool.
 *
 * Each proxied client calls [[nextEgress]] exactly once — it's wired behind a
 * `lazy val` in `WorkerWiring` — and reuses the pinned config for its lifetime,
 * so the assignment is sticky per process. The in-memory cookie jar re-warms on
 * restart, so cross-restart assignment order doesn't matter; only that distinct
 * clients land on distinct IPs.
 */
class StickyProxyPool(pool: RealHttpFetch.ProxyConfig) {
  private val cursor = new AtomicInteger(0)

  /** The next sticky egress: a config pinned to the next pool port, round-robin
   *  (wraps once the pool is exhausted). */
  def nextEgress(): RealHttpFetch.ProxyConfig =
    pool.pinnedTo(pool.ports(Math.floorMod(cursor.getAndIncrement(), pool.ports.size)))
}
