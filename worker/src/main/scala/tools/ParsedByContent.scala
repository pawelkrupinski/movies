package tools

import com.github.benmanes.caffeine.cache.Cache

import scala.util.hashing.MurmurHash3

/**
 * A PURE parse of a page, done once per distinct content: a search page Metacritic or Rotten Tomatoes answered is
 * read the same way every time it is read, so the second read of the same text is handed the first one's result.
 *
 * Keyed by the content — its length and a 128-bit hash of it, never by the object or the URL — so it holds no page,
 * and what it hands back is what parsing that text gives: an equal body parses to an equal, immutable result. A
 * result evicted is parsed again, alike. Nothing a reader decides can depend on whether its parse was held; dropping
 * the whole cache changes only what the next reads cost.
 *
 * Why: a convergence replay re-reads the same recorded search pages tick after tick (each rating refresh walks its
 * film's probe ladder again), and Jsoup parsing them was 9% of the DE convergence row's CPU (JFR, run 41302893834).
 *
 * `parse` must be a function of the text alone: no client state, no clock, no configuration.
 */
final class ParsedByContent[A <: AnyRef](maxEntries: Long) {
  private val parsed: Cache[ParsedByContent.Key, A] = BoundedCache.ofSize(maxEntries).build[ParsedByContent.Key, A]()

  def apply(body: String)(parse: String => A): A = parsed.get(ParsedByContent.Key.of(body), _ => parse(body))
}

object ParsedByContent {

  /** A body's length and two independent 64-bit hashes of it: a different body with the same key is a 128-bit
   *  collision at the same length. */
  final case class Key(length: Int, high: Long, low: Long)

  object Key {
    def of(body: String): Key = Key(body.length, hash64(body, 0x1b873593, 0x5bd1e995), hash64(body, 0x2f9be6cc, 0x7ed55d16))

    private def hash64(s: String, a: Int, b: Int): Long =
      (MurmurHash3.stringHash(s, a).toLong << 32) ^ (MurmurHash3.stringHash(s, b) & 0xffffffffL)
  }
}
