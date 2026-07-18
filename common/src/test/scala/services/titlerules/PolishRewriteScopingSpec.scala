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
 * is why the guard below covers every Canonical rule rather than this one case.
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

  // ── the blanket guard ─────────────────────────────────────────────────────
  //
  // A rule that only DELETES text (format tags, bracketed years, punctuation) is
  // language-neutral and may stay unscoped — that's most of the ~227 rules, which
  // is why `countries = None` defaults to "everywhere". A CANONICAL rule that
  // WRITES words is the dangerous shape: that tier produces the `sanitize` merge
  // key, so a foreign-language rewrite pins the row to a key no local cinema slot
  // can produce, and every re-key path runs through the same `sanitize`.
  //
  // Covers every code-defined rule from BOTH sources, not just ExtraTitleRules.

  private val allCodeRules = TitleRules.all ++ ExtraTitleRules.all

  "every canonical rule that rewrites words" should "declare the countries it applies to" in {
    val offenders = allCodeRules.filterNot(_.countryScopeValid)
    withClue(s"unscoped word-rewriting canonical rules: ${offenders.map(_.id).mkString(", ")} — ") {
      offenders shouldBe empty
    }
  }

  // The runtime half of the guard: the admin editor can create a rule no compiler
  // ever sees, so an unscoped canonical word rewrite must be INERT rather than
  // applied everywhere — the same degradation a malformed pattern gets.
  "an unscoped canonical word rewrite" should "be a no-op instead of applying everywhere" in {
    val leaky = TitleRule("test-unscoped-rewrite", RuleScope.Canonical, None,
      """(?iu)^The Mandalorian and Grogu$""", "Mandalorian i Grogu", applyAll = false, order = 0)
    leaky.countryScopeValid shouldBe false
    leaky("The Mandalorian and Grogu") shouldBe "The Mandalorian and Grogu"
  }

  it should "still apply once it names its countries" in {
    val scoped = TitleRule("test-scoped-rewrite", RuleScope.Canonical, None,
      """(?iu)^The Mandalorian and Grogu$""", "Mandalorian i Grogu", applyAll = false, order = 0,
      countries = Some(Set(Country.Poland)))
    scoped.countryScopeValid shouldBe true
    scoped("The Mandalorian and Grogu") shouldBe "Mandalorian i Grogu"
  }

  // A per-cinema rule is already bound to one venue, which lives in one country,
  // so it may rewrite words without declaring them — the Farys "Tot Story" typo
  // fix is the live example. Forcing a country onto every per-venue typo fix
  // would be noise, not safety.
  "a per-cinema word rewrite" should "stay valid without declaring countries" in {
    TitleRule("test-per-cinema-rewrite", RuleScope.PerCinema, Some("farys"),
      """(?iu)^Tot Story$""", "Toy Story 5", applyAll = false, order = 0)
      .countryScopeValid shouldBe true
  }

  // The GlobalStructural tier shapes the TMDB search string, never the merge key,
  // and legitimately carries letters in language-neutral FORMAT rewrites. Guarding
  // it would have silently disabled this live rule ("Avatar 3D" -> "Avatar 3 D")
  // for no safety gain — it is the reason the guard is Canonical-only.
  "a structural format rewrite carrying a letter" should "stay valid without declaring countries" in {
    val formatRule = TitleRule("test-structural-format", RuleScope.GlobalStructural, None,
      """(\d+)D""", "$1 D", applyAll = true, order = 0)
    formatRule.countryScopeValid shouldBe true
    formatRule("Avatar 3D") shouldBe "Avatar 3 D"
  }
}
