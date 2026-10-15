package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Whitespace.collapsedSpaces

class WhitespaceSpec extends AnyFlatSpec with Matchers {

  "collapsedSpaces" should "make each whitespace run one space, trimmed, a no-break space kept" in {
    "  Digger \n\t (15)  ".collapsedSpaces shouldBe "Digger (15)"
    "Lalka  2D".collapsedSpaces shouldBe "Lalka  2D"
    "".collapsedSpaces shouldBe ""
  }
}
