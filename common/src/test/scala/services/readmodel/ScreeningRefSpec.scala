package services.readmodel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScreeningRefSpec extends AnyFlatSpec with Matchers {

  "A screening ref's city" should "be read off its id, after the film" in {
    ScreeningRef("f1a2b3|poznan|Kino Muza", "f1a2b3").citySlug shouldBe Some("poznan")
  }

  it should "survive a film id holding a `|` (a legacy title|year id) and a variant card" in {
    ScreeningRef("Diuna|2021|zielona-gora|Cinema City", "Diuna|2021").citySlug shouldBe Some("zielona-gora")
    ScreeningRef("f1a2b3~imax|london|BFI IMAX", "f1a2b3").citySlug shouldBe Some("london")
  }

  it should "be None for an id not shaped as the projection writes them" in {
    ScreeningRef("other|poznan|Kino Muza", "f1a2b3").citySlug shouldBe None
    ScreeningRef("f1a2b3|poznan", "f1a2b3").citySlug shouldBe None
  }
}
