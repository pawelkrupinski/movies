package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Every pair of instalments the pipeline has wrongly merged or split, replayed through the
 * real title decisions — `SequelMarker` / `TitleContainment` / `sanitize`.
 *
 * Twenty-odd fixes between 2026-08 and 2026-09 each pinned ONE pair next to the rule
 * it broke; five films were wrong in prod for weeks before any of them. This file is
 * the regression corpus as one list, so a change to any rule is measured against
 * every past incident at once, not only the one its author was thinking of. Each
 * entry names the commit whose fix it pins. Resolver-level pairs (Lalka, Mistyczka's
 * director walk, Mockingjay's fuzzy match, Homo sapiens) live in the worker's
 * `ResolutionCorporaSpec`, where the TMDB stub is.
 *
 * Add a pair whenever a merge/split/mis-resolve is fixed.
 */
class MatchingCorporaSpec extends AnyFlatSpec with Matchers {

  // ── Bare titles ────────────────────────────────────────────────────────────

  /** Two titles naming different instalments of one series — the title ALONE must
   *  say so, because the cinemas' own evidence is usually silent (UK slots publish an
   *  original title one time in nine). */
  private val differentInstalments: Seq[(String, String, String)] = Seq(
    ("f430c1de5", "The Hunger Games: Mockingjay - Part 1", "The Hunger Games: Mockingjay - Part 2"),
    ("f430c1de5", "The Hunger Games: Mockinjay - Part 1", "The Hunger Games: Mockingjay - Part 2"),
    ("f430c1de5", "Rocky II", "Rocky III"),
    ("f430c1de5", "Kingsman 2", "Kingsman 3"),
    ("f430c1de5", "Toy Story", "Toy Story 5"),
    ("130b89c55", "The Hunger Games", "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)"),
    ("130b89c55", "Star Wars", "Star Wars: Episode IV"),
    ("130b89c55", "Szybcy i wściekli", "Szybcy i wściekli: część 8"),
    ("d6f65d7d7", "Dune", "Dune: Part Two"),
    ("d6f65d7d7", "Wicked", "Wicked: Part Three"),
    ("260bc21e3", "The Hunger Games: Catching Fire", "The Hunger Games: Mockingjay - Part 1"),
    ("260bc21e3", "The Hunger Games: The Ballad of Songbirds and Snakes", "The Hunger Games: Mockingjay - Part 2"),
    ("8176372e4", "The Hunger Games: Catching Fire", "The Hunger Games: Sunrise on the Reaping"),
    ("2948a4041", "The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 2"),
    ("60820c6c6", "Bring It On", "Bring It On: All or Nothing")
  )

  /** Two spellings of ONE film that the instalment check must not tell apart. */
  private val sameInstalment: Seq[(String, String, String)] = Seq(
    ("e3699d05e", "Mortal Kombat 2", "Mortal Kombat II"),
    ("e3699d05e", "Dune: Part 2", "Dune: Part Two"),
    ("2948a4041", "The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 1"),
    ("2948a4041", "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)", "The Hunger Games: Mockingjay - Part 2"),
    ("2948a4041", "The Hunger Games: The Ballad of Songbirds & Snakes", "The Hunger Games: The Ballad of Songbirds and Snakes"),
    ("f430c1de5", "Guru", "Gourou"),
    ("130b89c55", "Toy Story 5", "Toddler Club: Toy Story 5"),
    ("130b89c55", "Casablanca", "Casablanca 1942"),
    ("130b89c55", "The Matrix", "Cineworld 30: The Matrix")
  )

  private def tokens(t: String) = TitleContainment.tokens(t)

  "SequelMarker.differentInstalments" should "tell every historical pair of instalments apart, both ways round" in {
    for ((sha, a, b) <- differentInstalments) withClue(s"[$sha] '$a' vs '$b': ") {
      SequelMarker.differentInstalments(tokens(a), tokens(b)) shouldBe true
      SequelMarker.differentInstalments(tokens(b), tokens(a)) shouldBe true
    }
  }

  it should "never tell two spellings of one instalment apart" in {
    for ((sha, a, b) <- sameInstalment) withClue(s"[$sha] '$a' vs '$b': ") {
      SequelMarker.differentInstalments(tokens(a), tokens(b)) shouldBe false
      SequelMarker.differentInstalments(tokens(b), tokens(a)) shouldBe false
    }
  }

  "TitleContainment.decorates" should "never read one instalment as a decoration of another" in {
    for ((sha, a, b) <- differentInstalments) withClue(s"[$sha] '$a' vs '$b': ") {
      TitleContainment.decorates(tokens(a), tokens(b)) shouldBe false
      TitleContainment.decorates(tokens(b), tokens(a)) shouldBe false
    }
  }

  "the title key" should "never give two instalments one merge key" in {
    for ((sha, a, b) <- differentInstalments) withClue(s"[$sha] '$a' vs '$b': ") {
      titleNormalizer.sanitize(a) should not be titleNormalizer.sanitize(b)
    }
  }
}
