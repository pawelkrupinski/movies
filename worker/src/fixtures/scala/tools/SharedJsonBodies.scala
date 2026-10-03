package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import play.api.libs.json.{JsValue, Json}

/**
 * TMDB response bodies parsed once for every pass of an order-independence replay, not once per pass.
 *
 * The passes run side by side in one JVM over the same recorded tree, so each identity take-up asks
 * the same film records at about the same moment, and each parsed every one of them for itself —
 * the cast-and-crew bodies the identity store's normalizer files (`TmdbNormalizer`, through
 * [[JsonBodies]]), ~82k per US pass: 8% of the US take-up's CPU samples, spent three times over (JFR,
 * 2026-10-03). Shared, the first pass to read a body parses it and the others get that tree: 77% of a
 * local US replay's parses were answered so.
 *
 * Keyed by the body's CONTENT — each pass reads its fixture into a string of its own — and a pure
 * function of it: a parsed `JsValue` is immutable, so a pass can neither change what another reads
 * nor leave anything of its own in it, and a body evicted and parsed again is an equal tree. What a
 * pass decides cannot depend on whether its parse was shared.
 *
 * Bounded by the bodies' length ([[SharedJsonBodies.Budget]]): the whole US tree parsed would be
 * gigabytes, and the passes only need what they are reading NOW — they read in lockstep, so a body
 * one pass parsed is asked by the others seconds later. Lives in the replay harness only
 * (`ArchiveReplayWiring`'s `tmdbBodies`); no worker holds it.
 */
final class SharedJsonBodies(budget: Long = SharedJsonBodies.Budget, parser: String => JsValue = Json.parse(_: String))
    extends JsonBodies {
  private val parsed: Cache[String, JsValue] = Caffeine.newBuilder()
    .maximumWeight(budget)
    .weigher[String, JsValue]((body, _) => body.length)
    .executor(_.run())   // evict on the caller's thread: the bound holds when `parse` returns
    .recordStats()
    .build[String, JsValue]()

  override def parse(body: String): JsValue = parsed.get(body, parser(_))

  /** Characters of body held parsed now. */
  private[tools] def held: Long = parsed.policy().eviction().get().weightedSize().getAsLong

  /** How often a pass found its body already parsed — what the sharing saved. */
  def describe: String = {
    val stats = parsed.stats()
    f"shared TMDB bodies: ${stats.hitCount} of ${stats.requestCount} parses shared (${stats.hitRate * 100}%.1f%%), ${stats.evictionCount} evicted"
  }
}

object SharedJsonBodies {
  /** Characters of body held parsed at once: ~4,000 US film records (7.8 KB on average), seconds of
   *  a take-up's reads — wider than the passes drift apart, a fraction of the replay's heap. */
  val Budget: Long = 32L * 1024 * 1024
}
