package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger

/** A pure parse, done once per distinct content: an equal text — another object — is handed the first parse's result, a
 *  different text is parsed for itself, and an evicted one is parsed again to an equal result. */
class ParsedByContentSpec extends AnyFlatSpec with Matchers {

  private final class Counted(maxEntries: Long = 16) {
    val parses = new AtomicInteger
    val cache  = new ParsedByContent[Seq[String]](maxEntries)
    def apply(body: String): Seq[String] = cache(body) { b => parses.incrementAndGet(); b.split(',').toSeq }
  }

  "ParsedByContent" should "parse an equal text once, whichever object carries it" in {
    val c     = new Counted
    val first = c(new String("a,b,c"))
    c(new String("a,b,c")) should be theSameInstanceAs first
    c.parses.get shouldBe 1
  }

  it should "parse different texts each for itself, the same length included" in {
    val c = new Counted
    c("a,b") shouldBe Seq("a", "b")
    c("a,c") shouldBe Seq("a", "c")
    c.parses.get shouldBe 2
  }

  it should "hand back what parsing gives after an eviction" in {
    val c = new Counted(maxEntries = 1)
    (1 to 200).foreach(i => c(s"x,$i") shouldBe Seq("x", i.toString))
    c("x,1") shouldBe Seq("x", "1")
  }

  it should "cache nothing for a parse that throws" in {
    val c     = new ParsedByContent[Seq[String]](16)
    val tries = new AtomicInteger
    an[IllegalStateException] should be thrownBy c("boom")(_ => { tries.incrementAndGet(); throw new IllegalStateException("no") })
    c("boom")(_ => { tries.incrementAndGet(); Seq("ok") }) shouldBe Seq("ok")
    tries.get shouldBe 2
  }

  "ParsedByContent.Key" should "be equal for equal texts and differ for a changed one" in {
    ParsedByContent.Key.of(new String("<html>x</html>")) shouldBe ParsedByContent.Key.of("<html>x</html>")
    ParsedByContent.Key.of("<html>x</html>") should not be ParsedByContent.Key.of("<html>y</html>")
  }

  it should "be MurmurHash3 x64_128 of the UTF-8 bytes, as the reference computes it" in {
    // the reference's (and Python mmh3.hash64's) values, seed 0
    ParsedByContent.Key.murmur3x64x128("hello".getBytes("UTF-8")) shouldBe ((0xcbd8a7b341bd9b02L, 0x5b1e906a48ae1d19L))
    ParsedByContent.Key.murmur3x64x128(Array.emptyByteArray) shouldBe ((0L, 0L))
  }

  it should "tell apart texts whose difference lies past the first block, in the tail, or in a non-Latin letter" in {
    val page = "<div>" + ("x" * 37) + "</div>"
    val keys = Seq(page, page.updated(20, 'y'), page.updated(page.length - 1, '!'), page.updated(20, 'ł'), page.updated(20, '’'))
      .map(ParsedByContent.Key.of)
    keys.distinct.size shouldBe keys.size
  }
}
