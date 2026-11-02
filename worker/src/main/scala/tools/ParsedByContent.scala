package tools

import com.github.benmanes.caffeine.cache.Cache

import java.lang.invoke.{MethodHandles, VarHandle}
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets

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

  /** A body's length and a 128-bit hash of it (MurmurHash3 x64_128 of its UTF-8 bytes): a different body with the same
   *  key is a 128-bit collision at the same length. One pass over the bytes, 16 at a time: the four passes of
   *  `MurmurHash3.stringHash` it replaces cost more than the parses they saved (JFR, run 41352844445: 5.5% of the DE
   *  convergence row's CPU, the parses left 4.5%). */
  final case class Key(length: Int, high: Long, low: Long)

  object Key {
    def of(body: String): Key = {
      val (high, low) = murmur3x64x128(body.getBytes(StandardCharsets.UTF_8))
      Key(body.length, high, low)
    }

    private val Longs: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Long]], ByteOrder.LITTLE_ENDIAN)
    private val C1               = 0x87c37b91114253d5L
    private val C2               = 0x4cf5ad432745937fL

    /** MurmurHash3_x64_128 (Austin Appleby's reference, seed 0): `h1`, `h2`. */
    private[tools] def murmur3x64x128(data: Array[Byte]): (Long, Long) = {
      var h1     = 0L
      var h2     = 0L
      val blocks = data.length / 16
      var i      = 0
      while (i < blocks) {
        val k1 = (Longs.get(data, i * 16): Long)
        val k2 = (Longs.get(data, i * 16 + 8): Long)
        h1 ^= mixK1(k1)
        h1 = java.lang.Long.rotateLeft(h1, 27) + h2
        h1 = h1 * 5 + 0x52dce729
        h2 ^= mixK2(k2)
        h2 = java.lang.Long.rotateLeft(h2, 31) + h1
        h2 = h2 * 5 + 0x38495ab5
        i += 1
      }
      var k1   = 0L
      var k2   = 0L
      val tail = blocks * 16
      var j    = data.length - 1
      while (j >= tail) {
        val at   = j - tail
        val byte = data(j) & 0xffL
        if (at >= 8) k2 ^= byte << ((at - 8) * 8) else k1 ^= byte << (at * 8)
        j -= 1
      }
      h2 ^= mixK2(k2)
      h1 ^= mixK1(k1)
      h1 ^= data.length
      h2 ^= data.length
      h1 += h2
      h2 += h1
      h1 = fmix(h1)
      h2 = fmix(h2)
      h1 += h2
      h2 += h1
      (h1, h2)
    }

    private def mixK1(k: Long): Long = java.lang.Long.rotateLeft(k * C1, 31) * C2
    private def mixK2(k: Long): Long = java.lang.Long.rotateLeft(k * C2, 33) * C1

    private def fmix(k0: Long): Long = {
      var k = k0
      k ^= k >>> 33
      k *= 0xff51afd7ed558ccdL
      k ^= k >>> 33
      k *= 0xc4ceb9fe1a85ec53L
      k ^ (k >>> 33)
    }
  }
}
