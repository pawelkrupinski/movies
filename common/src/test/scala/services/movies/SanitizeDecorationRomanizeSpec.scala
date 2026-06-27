package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Regression: `sanitize` must be INVARIANT to a decoration glued to a trailing
 * numeral. "Toy Story 5- dubbing" (the cinema's raw spelling) and "Toy Story 5"
 * (its decoration-stripped display form) are the same film and must share a key.
 *
 * They didn't: `normalize` (Arabic→Roman) ran BEFORE the `- dubbing` strip, so it
 * saw the glued token "5-" and left it Arabic (`toystory5`), while the stripped
 * "Toy Story 5" romanised the now-standalone "5"→V (`toystoryv`). The two keys
 * disagreed, so the folded `movies` row (keyed off the display form) never matched
 * the next scrape's key and the film re-incubated in staging every scrape.
 * `sanitize` now romanises AFTER `canonical`, so both key on `toystoryv`.
 */
class SanitizeDecorationRomanizeSpec extends AnyFlatSpec with Matchers {

  "sanitize" should "key a glued-decoration title the same as its stripped form (Toy Story 5)" in {
    TitleNormalizer.sanitize("Toy Story 5- dubbing") shouldBe TitleNormalizer.sanitize("Toy Story 5")
    TitleNormalizer.sanitize("Toy Story 5- dubbing") shouldBe "toystoryv"
  }

  it should "still romanise a space-separated decoration's numeral (unchanged)" in {
    TitleNormalizer.sanitize("Toy Story 5 - dubbing") shouldBe "toystoryv"
    TitleNormalizer.sanitize("Dune 2 (dubbing)") shouldBe "duneii"
  }

  it should "NOT romanise a numeral glued to RETAINED punctuation (no decoration stripped)" in {
    // "Avengers 5: Doomsday" keeps its subtitle, so "5:" stays glued and Arabic —
    // the order swap only romanises numerals the strip actually frees.
    TitleNormalizer.sanitize("Avengers 5: Doomsday") shouldBe "avengers5doomsday"
  }

  it should "leave numerals inside a word and 4-digit numbers alone" in {
    TitleNormalizer.sanitize("Se7en")        shouldBe "se7en"
    TitleNormalizer.sanitize("Blade Runner 2049") shouldBe "bladerunner2049"
  }
}
