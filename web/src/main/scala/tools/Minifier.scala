package tools


import play.api.Mode

/** Rewrites an HTML fragment's inline `<script>` / `<style>` blocks — what
 *  `views/_minified.scala.html` applies to every block it wraps. Chosen once
 *  at the web composition root ([[Minifier.forMode]]) and handed to the
 *  templates by the controllers. */
trait Minifier {
  def process(html: String): String
}

object Minifier {
  /** Production traffic gets a fresh [[MemoisingMinifier]]; every other mode
   *  gets [[PassThroughMinifier]] so the inline source stays readable in
   *  DevTools while running `sbt run` locally. */
  def forMode(mode: Mode): Minifier =
    if (mode == Mode.Prod) new MemoisingMinifier else PassThroughMinifier
}

/** Leaves the markup untouched — the dev-mode and `/debug/tune` choice. */
object PassThroughMinifier extends Minifier {
  def process(html: String): String = html
}

/** [[Minify]] memoised per input, so the per-request render cost is one cache lookup
 *  after the first hit.
 *
 *  BOUNDED, because the input set is only bounded while no block carries a per-render
 *  value. The listing's script once interpolated its render instant, which made every
 *  render a new ~25 KB block and a new minified script — kept in unbounded maps for the
 *  life of the process. That value now rides outside the block; the bound is what stops
 *  the next one leaking, and costs nothing while the blocks are what they should be:
 *  every distinct template-rendered block (a few per city) is one entry. */
class MemoisingMinifier(maxEntries: Long = MemoisingMinifier.MaxEntries) extends Minifier {
  private def cache(): com.github.benmanes.caffeine.cache.Cache[String, String] =
    com.github.benmanes.caffeine.cache.Caffeine.newBuilder().maximumSize(maxEntries).build[String, String]()
  // Stats on the block cache alone: its hit ratio is what says the blocks are what they
  // should be. A per-render value inside one would show as a ratio near zero and an
  // entry count that only grows (published as `kinowo_web_cache_*{cache="minifier"}`).
  private val blockCache = com.github.benmanes.caffeine.cache.Caffeine.newBuilder()
    .maximumSize(maxEntries).recordStats().build[String, String]()
  private val jsCache    = cache()
  private val cssCache   = cache()

  private val minifyJs:  String => String = src => jsCache.get(src, Minify.minifyJs)
  private val minifyCss: String => String = src => cssCache.get(src, Minify.minifyCss)

  def process(html: String): String =
    blockCache.get(html, Minify.process(_, minifyJs, minifyCss))

  /** How many distinct blocks this instance has memoised. */
  def cachedBlocks: Int = { blockCache.cleanUp(); blockCache.estimatedSize().toInt }

  /** The block cache's size and hit ratio, for `WebCacheMetrics`. */
  def occupancy: services.metrics.CacheOccupancy = {
    blockCache.cleanUp()
    services.metrics.CacheOccupancy.of(blockCache, weighted = false)
  }
}

object MemoisingMinifier {
  /** Every block of every city's pages several times over: a city contributes a handful. */
  val MaxEntries: Long = 4096
}
