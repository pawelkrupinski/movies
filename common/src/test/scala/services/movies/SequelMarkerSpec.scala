package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pairs come from nine days of prod re-key logs (2026-08-29 → 09-06): the ones
 *  that folded wrongly and the ones that folded rightly and must keep doing so. */
class SequelMarkerSpec extends AnyFlatSpec with Matchers {

  private def toks(s: String): Seq[String] =
    tools.TextNormalization.deburr(s).toLowerCase.split("[^\\p{L}\\p{N}]+").filter(_.nonEmpty).toSeq

  private def anotherEntry(base: String, whole: String) = SequelMarker.namesAnotherEntry(toks(base), toks(whole))

  "SequelMarker" should "refuse a sequel that carries the base title as a prefix" in {
    anotherEntry("The Hunger Games", "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)") shouldBe true
    anotherEntry("The Hunger Games", "The Hunger Games: Mockingjay Part 1")            shouldBe true
    anotherEntry("Toy Story",        "Toy Story 5")                                     shouldBe true
    anotherEntry("Rocky",            "Rocky II")                                        shouldBe true
    anotherEntry("Blade Runner",     "Blade Runner 2049")                               shouldBe true
    anotherEntry("Star Wars",        "Star Wars: Episode IV")                           shouldBe true
    anotherEntry("Szybcy i wściekli", "Szybcy i wściekli: część 8")                     shouldBe true
  }

  it should "let a decorated screening of the same film fold" in {
    anotherEntry("Toy Story 5",              "Toddler Club: Toy Story 5")               shouldBe false
    anotherEntry("Fallen Angels by Noel Coward", "GB: Fallen Angels by Noel Coward")    shouldBe false
    anotherEntry("The Matrix",               "Cineworld 30: The Matrix")                shouldBe false
    anotherEntry("Casablanca",               "Casablanca 1942")                          shouldBe false
    anotherEntry("Ojczyzna",                 "Ojczyzna - pokaz przedpremierowy 2026")    shouldBe false
    anotherEntry("Top Gun",                  "Top Gun - Re-Release")                     shouldBe false
    anotherEntry("Cars 20th Anniversary",    "Toddler Club Cars 20th Anniversary")       shouldBe false
  }
}
