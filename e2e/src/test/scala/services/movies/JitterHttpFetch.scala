package services.movies

import tools.HttpFetch

import java.util.concurrent.CompletableFuture

/** Sleeps a deterministic-per-(url,seed) 0..maxDelayMillis before each fetch so
 *  the concurrent enrichment stages complete in a perturbed order. Deterministic
 *  in the delay (a fixed function of the URL + seed), so a failing seed replays
 *  the same timing profile; the actual completion *order* still depends on the
 *  pools, which is exactly the nondeterminism under test. Shared by the
 *  determinism specs (`ScrapeOrderDeterminismSpec`, `StagingOrderDeterminismSpec`). */
class JitterHttpFetch(delegate: HttpFetch, seed: Long, maxDelayMillis: Int) extends HttpFetch {
  // Toggled off for serial, single-threaded re-enrichment phases (e.g.
  // `converge()`), where a per-fetch sleep perturbs nothing and only burns
  // wall-clock. Stays on through the concurrent scrape/drain where it matters.
  @volatile var enabled: Boolean = true
  private def jitter(url: String): Unit = {
    if (!enabled) return
    val h = (url.hashCode.toLong * 0x9E3779B97F4A7C15L) ^ seed
    val d = Math.floorMod(h, (maxDelayMillis + 1).toLong)
    if (d > 0) Thread.sleep(d)
  }
  override def get(url: String): String = { jitter(url); delegate.get(url) }
  override def getBytes(url: String): Array[Byte] = { jitter(url); delegate.getBytes(url) }
  override def get(url: String, headers: Map[String, String]): String = { jitter(url); delegate.get(url, headers) }
  override def post(url: String, body: String, contentType: String): String = { jitter(url); delegate.post(url, body, contentType) }
  override def getAsync(url: String): CompletableFuture[String] = CompletableFuture.supplyAsync(() => get(url))
}
