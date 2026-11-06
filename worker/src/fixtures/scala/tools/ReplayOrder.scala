package tools

/**
 * The one replay world's randomness (docs/design/identity-structural-fixes.md, fix 3): an order of items drawn by the
 * seed and each item's OWN key alone, never by a stream of draws -- so an item more or fewer moves no other's place.
 * A `scala.util.Random(seed).shuffle` re-rolls every later position when one item is added or dropped: the drift
 * replay's shared stream did so to its fills (IdentityDrift.Share.filed, keyed since 80b59d0da8e1), and a slice that
 * gained one listing was walked in a wholly other order. A tool file: stdlib alone, for the old refs it is dropped into.
 */
object ReplayOrder {
  /** `items` ordered by `seed` and each one's `key`: the same seed and keys give the same order, whatever else is there. */
  def of[A](items: Seq[A], seed: Long)(key: A => String): Seq[A] =
    items.map(item => (rank(seed, key(item)), key(item), item)).sortBy(r => (r._1, r._2)).map(_._3)

  /** Where `key` falls under `seed`: 64 bits of two MurmurHash3s, each seeded by the whole seed (mixed, so seeds a few
   *  apart -- 7 and 8 -- give wholly other orders). */
  def rank(seed: Long, key: String): Long = {
    val mixed = seed * 0x9e3779b97f4a7c15L
    val high  = scala.util.hashing.MurmurHash3.stringHash(key, (mixed >>> 32).toInt ^ mixed.toInt)
    val low   = scala.util.hashing.MurmurHash3.stringHash(key, mixed.toInt ^ 0x5bd1e995)
    (high.toLong << 32) | (low.toLong & 0xffffffffL)
  }
}
