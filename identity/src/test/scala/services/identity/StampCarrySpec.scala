package services.identity

import kinowo.build.StampCarry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The stamp carry (`project/StampCarry.scala`): a digest proven to decide what an earlier one decides carries that one's
 *  stamp, so the deploy re-decides nothing; any other digest is its own stamp, as before. */
class StampCarrySpec extends AnyFlatSpec with Matchers {

  private def digest(c: Char): String = c.toString * 64
  private def sha(c: Char): String    = c.toString * 40
  private def row(from: Char, to: Char, stamp: String = "identity-rules", proof: String = "pure-move"): String =
    Seq(stamp, digest(from), digest(to), proof, sha('1'), sha('2')).mkString("\t")

  "a stamp" should "be its own digest when no row carries it: an unproven change bumps it" in {
    StampCarry.version("identity-rules", digest('a'), StampCarry.parse(Seq(row('b', 'c')))) shouldBe digest('a')
  }

  it should "be the earlier digest a proven row carries it to: a pure move keeps the stamp" in {
    StampCarry.version("identity-rules", digest('b'), StampCarry.parse(Seq(row('b', 'a')))) shouldBe digest('a')
  }

  it should "follow a chain of proven rows to the stamp its first step carried" in {
    val rows = StampCarry.parse(Seq(row('b', 'a'), row('c', 'b', proof = "what-if")))
    StampCarry.version("identity-rules", digest('c'), rows) shouldBe digest('a')
  }

  it should "read only its own stamp's rows" in {
    StampCarry.version("venue-slot", digest('b'), StampCarry.parse(Seq(row('b', 'a')))) shouldBe digest('b')
  }

  "the table" should "fail the build on a row that loops" in {
    val rows = StampCarry.parse(Seq(row('b', 'a'), row('a', 'b')))
    an[IllegalArgumentException] should be thrownBy StampCarry.version("identity-rules", digest('a'), rows)
  }

  it should "fail the build on one digest carried twice, or on a malformed row" in {
    an[IllegalArgumentException] should be thrownBy StampCarry.parse(Seq(row('b', 'a'), row('b', 'c')))
    an[IllegalArgumentException] should be thrownBy StampCarry.parse(Seq(row('b', 'a', stamp = "nonsense")))
    an[IllegalArgumentException] should be thrownBy StampCarry.parse(Seq(row('b', 'a', proof = "trust-me")))
    an[IllegalArgumentException] should be thrownBy StampCarry.parse(Seq(row('a', 'a')))
    an[IllegalArgumentException] should be thrownBy StampCarry.parse(Seq(row('b', 'a').split("\t").take(5).mkString("\t")))
  }

  it should "skip its comments and blank lines" in {
    StampCarry.parse(Seq("# stamp\tdigest", "", row('b', 'a'))) should have size 1
  }
}
