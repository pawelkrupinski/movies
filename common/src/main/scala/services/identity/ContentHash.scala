package services.identity

import scala.util.hashing.MurmurHash3

/**
 * A 64-bit hash of a value's content, walked structurally and building nothing: products and
 * sequences in order, sets and maps order-free, strings and numbers by value. Equal values hash
 * equally in every JVM — it is persisted (a stored family's slice digest) — and a change anywhere
 * in the value moves it.
 *
 * What a slice's digest was before: its structural `hashCode` beside a hash of its `toString`,
 * which rendered every candidate record and ranked answer of every family it compared into one
 * string — the model thread's largest single cost in a take-up's profile.
 */
private[identity] object ContentHash {

  def of(value: Any): Long = value match {
    case null                => 0x9e3779b97f4a7c15L
    // The corpus-wide learned decorations every listing carries: the rules version's already
    // (`IncrementalResolver.rulesVersion`), so a change rebuilds every family. Walked per listing,
    // they were 9.5 s of a US take-up's digests.
    case _: TitleDecorations => 0xdL
    case s: String           => (MurmurHash3.stringHash(s, 0x1b873593).toLong << 32) ^ (MurmurHash3.stringHash(s, 0x5bd1e995) & 0xffffffffL)
    case i: Int              => mix(i.toLong ^ 0x1L)
    case l: Long             => mix(l ^ 0x2L)
    case d: Double           => mix(java.lang.Double.doubleToLongBits(d) ^ 0x3L)
    case b: Boolean          => if (b) 0x4L else 0x5L
    case c: Char             => mix(c.toLong ^ 0x6L)
    case None                => 0x7L
    case Some(x)             => mix(of(x) + 0x8L)
    case m: scala.collection.Map[?, ?] =>
      mix(m.foldLeft(m.size.toLong * 0x9L) { case (acc, (k, v)) => acc + mix(of(k) * 31 + of(v)) })
    case s: scala.collection.Set[?] =>
      mix(s.foldLeft(s.size.toLong * 0xaL)((acc, e) => acc + mix(of(e))))
    case s: Iterable[?]      => s.foldLeft(0xbL)((acc, e) => mix(acc * 31 + of(e)))
    case p: Product          => p.productIterator.foldLeft(of(p.productPrefix))((acc, e) => mix(acc * 31 + of(e)))
    case other               => mix(other.hashCode.toLong ^ 0xcL)
  }

  /** SplitMix64's finaliser: every input bit reaches every output bit. */
  private def mix(z0: Long): Long = {
    var z = z0
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
  }
}
