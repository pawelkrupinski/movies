package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode

class MinifierSpec extends AnyFlatSpec with Matchers {

  private val block = "<script>var x = 1; // c</script><style>.a { color: red; }</style>"

  "MemoisingMinifier" should "minify exactly as the stateless Minify does" in {
    new MemoisingMinifier().process(block) shouldBe Minify.process(block)
  }

  it should "keep its memo to itself, so a fresh instance starts empty" in {
    val warm = new MemoisingMinifier
    warm.process(block)
    warm.process("<style>.b { color: blue; }</style>")
    warm.cachedBlocks shouldBe 2

    new MemoisingMinifier().cachedBlocks shouldBe 0
  }

  "PassThroughMinifier" should "leave the markup untouched" in {
    PassThroughMinifier.process(block) shouldBe block
  }

  "Minifier.forMode" should "memoise in production and pass through elsewhere" in {
    Minifier.forMode(Mode.Prod) shouldBe a [MemoisingMinifier]
    Minifier.forMode(Mode.Dev) shouldBe PassThroughMinifier
    Minifier.forMode(Mode.Test) shouldBe PassThroughMinifier
  }

  it should "hand each production wiring its own caches" in {
    Minifier.forMode(Mode.Prod) should not be theSameInstanceAs (Minifier.forMode(Mode.Prod))
  }
}
