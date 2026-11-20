package tools.couplingfixture.slack

// ExemptionSlackSpec's fixture: Asker counts nine collaborators (each holds state, a `var`), and gives up four of them
// to the suggested cut -- one forwarded, one used only while it is built, one co-used with another, one reached
// through another that hands it out -- while one more is only beside another (a hint, kept).

final class Asker(store: Store, hashing: Hashing, metrics: Metrics, corpus: Corpus, fence: Fence, tally: Tally,
                  decoder: Decoder, seed: Seed) {
  // the decoder is only forwarded into the cursor; the seed only read while the asker is built
  private val cursor = new Cursor(decoder)
  private val start  = seed.take()

  def started: Int = start
  // the store and its hashing: every method using either uses both
  def file(x: Int): Unit = store.put(hashing.of(x))
  def count: Int = store.count + hashing.of(0)
  // the fence only where the corpus is used too, and the corpus hands it out
  def guard(): Int = corpus.add() + fence.hold()
  def ask(line: String): Int = metrics.asked() + corpus.add() + cursor.next(line)
  // the tally only where the metrics are used too, but the metrics do not hand it out
  def skip(): Int = metrics.asked() + tally.bump()
}

final class Store { private var n = 0; def put(x: Int): Unit = n += x; def count: Int = n }
final class Hashing { private var calls = 0; def of(x: Int): Int = { calls += 1; x * 31 + calls } }
final class Metrics { private var seen = 0; def asked(): Int = { seen += 1; seen } }
final class Tally { private var n = 0; def bump(): Int = { n += 1; n } }
final class Fence { private var held = 0; def hold(): Int = { held += 1; held } }
final class Corpus(fence: Fence) { private var rows = 0; def add(): Int = { rows += 1; rows }; def fenceOf: Fence = fence }
final class Decoder { private var n = 0; def decode(s: String): Int = { n += 1; s.length + n } }
final class Cursor(decoder: Decoder) { private var at = 0; def next(s: String): Int = { at += 1; decoder.decode(s) + at } }
final class Seed { private var handed = 0; def take(): Int = { handed += 1; handed } }
