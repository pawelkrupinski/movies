package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `sanitize` keys numerals in ARABIC (the spelling cinemas + TMDB use), unifying
 * the unambiguous multi-letter Roman form onto it, while staying invariant to a
 * decoration glued to the numeral and never corrupting single-letter title words.
 */
class SanitizeNumeralKeySpec extends AnyFlatSpec with Matchers {
  import TitleNormalizer.sanitize

  "sanitize" should "key numerals in Arabic, not Roman" in {
    sanitize("Toy Story 5")     shouldBe "toystory5"
    sanitize("Avengers 5: Doomsday") shouldBe "avengers5doomsday"
  }

  it should "unify the multi-letter Roman form onto Arabic, case-insensitively" in {
    sanitize("Mortal Kombat II") shouldBe sanitize("Mortal Kombat 2")
    sanitize("Mortal Kombat II") shouldBe "mortalkombat2"
    sanitize("Mortal kombat ii") shouldBe "mortalkombat2"   // a lower-casing cinema must still fold
  }

  // Format stripping moved OUT of the canonical/sanitize rules into the shared
  // `FormatTags`, applied centrally at ingest (MovieCache.recordCinemaScrape) so
  // it also badges the screenings. So the "decoration glued to the numeral"
  // invariance (the re-divert bug) is now provided by `extractFormatTags` BEFORE
  // sanitize — sanitize itself no longer strips a format tag.
  it should "stay invariant to a format decoration glued to the numeral (stripped by extractFormatTags, then keyed)" in {
    def key(s: String) = sanitize(FormatTags.extractFormatTags(s)._1)
    key("Toy Story 5- dubbing")      shouldBe sanitize("Toy Story 5")
    key("Mortal Kombat II- dubbing") shouldBe sanitize("Mortal Kombat II")
    key("Dune 2 (dubbing)")          shouldBe "dune2"
  }

  it should "NOT convert single-letter Roman that collides with real title words" in {
    sanitize("I Am Legend")    shouldBe "iamlegend"     // not "1amlegend"
    sanitize("Malcolm X")      shouldBe "malcolmx"      // not "malcolm10"
    sanitize("V for Vendetta") shouldBe "vforvendetta"  // not "5forvendetta"
  }

  it should "leave numerals inside a word and 4-digit numbers alone" in {
    sanitize("Se7en")             shouldBe "se7en"
    sanitize("Blade Runner 2049") shouldBe "bladerunner2049"
  }
}
