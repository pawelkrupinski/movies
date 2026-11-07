package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LatinScriptSpec extends AnyFlatSpec with Matchers {
  "a text" should "hold a Latin letter where one of its letters is Latin, accented or not" in {
    LatinScript.hasLetter("Wong Kar Wai") shouldBe true
    LatinScript.hasLetter("Łódź 1999") shouldBe true
    LatinScript.hasLetter("王家衛") shouldBe false
    LatinScript.hasLetter("Москва") shouldBe false
    LatinScript.hasLetter("1999 — !") shouldBe false
  }

  it should "be Latin letters only where no letter of it is of another script, a text with none too" in {
    LatinScript.lettersOnly("Łódź 1999") shouldBe true
    LatinScript.lettersOnly("1999") shouldBe true
    LatinScript.lettersOnly("Kino Москва") shouldBe false
  }
}
