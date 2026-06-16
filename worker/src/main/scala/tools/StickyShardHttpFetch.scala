package tools

import java.net.URI
import java.util.concurrent.CompletableFuture

/**
 * Routes each request to one of `shards` by a stable key derived from the URL,
 * so a given venue always egresses via the SAME shard — and thus the same proxy
 * IP and its warmed session cookie — while different venues spread across all
 * shards.
 *
 * This is how the Decodo proxy stays sticky-per-venue without funnelling onto
 * one IP. Each shard is a [[RealHttpFetch]] pinned to one pool IP (its own cookie
 * jar); the Multikino shards are each wrapped in their own
 * [[SessionWarmingHttpFetch]], so a cold venue warms the homepage on ITS shard's
 * IP and the warmed session is reused by every venue routed to that IP. Spreading
 * venues across all IPs keeps any one endpoint under Decodo's concurrent-auth cap
 * (`too many authentication attempts. Limit: 3`, which rolled all proxied traffic
 * to Zyte on 2026-06-16). See the `reference_decodo_isp_proxy` memory.
 *
 * The key defaults to host + path with the query stripped ([[StickyShardHttpFetch.hostAndPath]]),
 * which is stable per venue: Multikino's films URL carries the cinemaId in its
 * path, biletyna's the venue, while the query carries volatile session attributes.
 */
class StickyShardHttpFetch(
  shards: IndexedSeq[HttpFetch],
  keyOf:  String => String = StickyShardHttpFetch.hostAndPath
) extends HttpFetch {
  require(shards.nonEmpty, "StickyShardHttpFetch needs at least one shard")

  private def shardFor(url: String): HttpFetch =
    shards(Math.floorMod(keyOf(url).hashCode, shards.size))

  override def get(url: String): String = shardFor(url).get(url)
  override def get(url: String, headers: Map[String, String]): String = shardFor(url).get(url, headers)
  override def getBytes(url: String): Array[Byte] = shardFor(url).getBytes(url)
  override def getAsync(url: String): CompletableFuture[String] = shardFor(url).getAsync(url)
  override def post(url: String, body: String, contentType: String): String =
    shardFor(url).post(url, body, contentType)
}

object StickyShardHttpFetch {
  /** Host + path, query stripped — stable per venue (a Multikino films URL's
   *  cinemaId, a biletyna venue path), unlike the full URL whose query carries
   *  session attributes. Falls back to the raw URL if it won't parse, so a weird
   *  URL still routes deterministically rather than throwing. */
  def hostAndPath(url: String): String =
    scala.util.Try {
      val uri = URI.create(url)
      s"${Option(uri.getHost).getOrElse("")}${Option(uri.getPath).getOrElse("")}"
    }.getOrElse(url)
}
