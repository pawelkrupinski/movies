package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The two schemes of the repository share these letters: neither may spell one differently from the other. */
class CyrillicLettersSpec extends AnyFlatSpec with Matchers {
  "CyrillicLetters.Shared" should "hold the 30 letters both schemes spell alike, and none of the seven they spell apart" in {
    CyrillicLetters.Shared should have size 30
    CyrillicLetters.Shared.keySet.intersect("гєёїйюя".toSet) shouldBe empty
  }

  it should "give the letters the search romanization writes" in {
    "щука".map(c => CyrillicLetters.Shared(c)).mkString shouldBe "shchuka"
    TextNormalization.romanizeCyrillic("щука") shouldBe "shchuka"
  }
}
