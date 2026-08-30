package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `CreditSpelling.alignedTo` — take an authoritative source's spelling for the
 * names it knows, change nothing else, and never change the shape of the list.
 */
class CreditSpellingSpec extends AnyFlatSpec with Matchers {

  behavior of "CreditSpelling.alignedTo"

  it should "take the authority's spelling for an internal capital no casing rule can derive" in {
    CreditSpelling.alignedTo(
      Seq("Leonardo Dicaprio", "Danny Devito", "Shia Labeouf"),
      Seq("Leonardo DiCaprio", "Danny DeVito", "Shia LaBeouf")
    ) shouldBe Seq("Leonardo DiCaprio", "Danny DeVito", "Shia LaBeouf")
  }

  it should "leave a name the authority doesn't carry exactly as it was" in {
    CreditSpelling.alignedTo(
      Seq("Leonardo Dicaprio", "Jan Kowalski"),
      Seq("Leonardo DiCaprio")
    ) shouldBe Seq("Leonardo DiCaprio", "Jan Kowalski")
  }

  it should "return the input untouched when the authority is empty" in {
    val names = Seq("Leonardo Dicaprio", "Jan Kowalski")
    CreditSpelling.alignedTo(names, Seq.empty) shouldBe names
  }

  it should "tolerate incidental whitespace on either side of the match" in {
    CreditSpelling.alignedTo(
      Seq("  sandra   bullock ", "Keanu\tReeves"),
      Seq("Sandra Bullock", "Keanu Reeves")
    ) shouldBe Seq("Sandra Bullock", "Keanu Reeves")
  }

  it should "never fuzzy-match — a diacritic or a different name is not the same person" in {
    CreditSpelling.alignedTo(
      Seq("Michał Żebrowski", "Danny Devitto", "Leo Dicaprio"),
      Seq("Michal Zebrowski", "Danny DeVito", "Leonardo DiCaprio")
    ) shouldBe Seq("Michał Żebrowski", "Danny Devitto", "Leo Dicaprio")
  }

  it should "keep both entries when two of them match one authoritative name" in {
    val aligned = CreditSpelling.alignedTo(
      Seq("Danny Devito", "Sandra Bullock", "danny devito"),
      Seq("Danny DeVito", "Sandra Bullock")
    )
    aligned shouldBe Seq("Danny DeVito", "Sandra Bullock", "Danny DeVito")
    aligned should have size 3
  }

  it should "preserve order regardless of the authority's order" in {
    CreditSpelling.alignedTo(
      Seq("c cooper", "b bardot", "a adams"),
      Seq("A Adams", "B Bardot", "C Cooper")
    ) shouldBe Seq("C Cooper", "B Bardot", "A Adams")
  }

  it should "resolve an authority that spells one name two ways to its first spelling" in {
    CreditSpelling.alignedTo(
      Seq("danny devito"),
      Seq("Danny DeVito", "Danny Devito")
    ) shouldBe Seq("Danny DeVito")
  }

  it should "return already-agreeing names byte-identical" in {
    val names = Seq("Christoph Waltz", "Sandra Bullock")
    CreditSpelling.alignedTo(names, names) shouldBe names
  }
}
