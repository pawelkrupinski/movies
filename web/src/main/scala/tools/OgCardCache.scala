package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.TimeUnit

/**
 * Shared memoisation for the rendered Open Graph PNGs (the film card and the
 * per-city page card). Holds a 12h, size-bounded Caffeine cache plus the
 * "render once, but only cache a *complete* card" rule both card services rely
 * on: a render whose poster failed to load is still returned, but NOT frozen,
 * so the next share retries instead of serving a permanently poster-less card.
 */
private[tools] class OgCardCache(maxSize: Long) {
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder().maximumSize(maxSize).expireAfterWrite(12, TimeUnit.HOURS).build()

  /** Return the cached PNG for `key`, or run `render`. `render` yields the
   *  bytes paired with whether the render is complete enough to cache. */
  def getOrRender(key: String)(render: => (Array[Byte], Boolean)): Array[Byte] =
    Option(cache.getIfPresent(key)).getOrElse {
      val (bytes, cacheable) = render
      if (cacheable) cache.put(key, bytes)
      bytes
    }
}

private[tools] object OgCard {
  /** Bound on how many candidate poster URLs a card fetches before giving up on
   *  a text-only/empty render. The fallback list can hold 25+ URLs; the first
   *  reachable origin almost always wins, but the cap keeps a pathological
   *  all-dead chain from stacking dozens of slow connect timeouts on one
   *  request. (A film's *primary* poster is often a Multikino origin whose
   *  Cloudflare 403s our Fly datacenter IP, so the fallbacks carry the load.) */
  val MaxPosterCandidates = 6
}
