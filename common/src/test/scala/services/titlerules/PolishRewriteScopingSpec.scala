package services.titlerules

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/**
 * A Canonical rule whose replacement writes WORDS is language-specific and must
 * name its countries — the invariant stated on [[TitleRule.countries]]. This spec
 * holds the line for the rules in [[ExtraTitleRules]].
 *
 * It was written for a live regression: `xtra-canonical-mandalorian-grogu-en`
 * mapped "The Mandalorian and Grogu" → the Polish "Mandalorian i Grogu" so the
 * English-titled Polish listing would merge, but it declared no countries and so
 * shipped to Germany. German cinemas list the film as "The Mandalorian And
 * Grogu", which matched, pinning the Berlin row to the Polish key
 * `mandalorianigrogu` — and showtimes-de rendered "Mandalorian i Grogu". Nothing
 * could re-key the row while the rule was installed, because every re-key runs
 * through the same `sanitize`.
 *
 * The UK corpus escaped by accident: its cinemas spell the film "The Mandalorian
 * & Grogu", which this pattern (anchored on "and") never matched. That near-miss
 * is why the guard is a blanket one over every rule rather than one case.
 */
class PolishRewriteScopingSpec extends AnyFlatSpec with Matchers {

  private def sanitizeFor(country: Country, title: String): String =
    TitleNormalizer.withRules(TitleRuleSet.forCountry(country))(TitleNormalizer.sanitize(title))

  private val GermanListing = "The Mandalorian And Grogu"

  "the Mandalorian canonical rule" should "map the English listing onto the Polish key in Poland" in {
    sanitizeFor(Country.Poland, GermanListing) shouldBe
      sanitizeFor(Country.Poland, "Mandalorian i Grogu")
  }

  it should "leave a German cinema's listing on its OWN key, not the Polish one" in {
    val germanKey = sanitizeFor(Country.Germany, GermanListing)
    germanKey should not be sanitizeFor(Country.Poland, "Mandalorian i Grogu")
    // And specifically: the key a German cinema slot can actually produce.
    germanKey shouldBe sanitizeFor(Country.Germany, "The Mandalorian and Grogu")
  }

  it should "leave the UK listing alone too" in {
    sanitizeFor(Country.UnitedKingdom, GermanListing) should not be
      sanitizeFor(Country.Poland, "Mandalorian i Grogu")
  }

  // The blanket guard. A Canonical rule that only DELETES text (an empty
  // replacement — format tags, bracketed years, punctuation) is language-neutral
  // and may stay unscoped; one that WRITES text is not, and a future unscoped
  // Polish rewrite would reach every country's corpus exactly as this one did.
  "every Canonical rule that rewrites words" should "declare the countries it applies to" in {
    val unscopedRewrites = ExtraTitleRules.all.filter { r =>
      r.scope == RuleScope.Canonical && r.replacement.trim.nonEmpty && r.countries.isEmpty
    }
    withClue(s"unscoped word-rewriting Canonical rules: ${unscopedRewrites.map(_.id).mkString(", ")} — ") {
      unscopedRewrites shouldBe empty
    }
  }
}
