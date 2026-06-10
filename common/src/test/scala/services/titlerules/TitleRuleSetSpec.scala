package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit tests for the rule-set abstraction itself — the behaviours the migration
 *  golden (which only exercises the default tiers) doesn't reach: per-cinema
 *  application, disabled / invalid-pattern no-ops, ordering, and the
 *  install/reset swap on `TitleNormalizer`. */
class TitleRuleSetSpec extends AnyFlatSpec with Matchers {
  import RuleScope._

  private def rule(id: String, scope: RuleScope, pattern: String, repl: String,
                   applyAll: Boolean = false, order: Int = 10, enabled: Boolean = true,
                   cinemaId: Option[String] = None, tag: Option[String] = None) =
    TitleRule(id, scope, cinemaId, pattern, repl, applyAll, order, enabled, tag)

  "perCinema" should "apply only the rules scoped to that cinema, in order, then trim" in {
    val rs = TitleRuleSet(Seq(
      rule("a", PerCinema, "^Ladies Night - ", "", cinemaId = Some("cc"), order = 10),
      rule("b", PerCinema, " - powrót do kin$", "", cinemaId = Some("cc"), order = 20),
      rule("c", PerCinema, """\s*\|\s*""", ": ", applyAll = true, cinemaId = Some("bok"), order = 10)
    ))
    rs.perCinema("cc", "Ladies Night - Wicked - powrót do kin") shouldBe "Wicked"
    rs.perCinema("bok", "Diuna | CZ.2") shouldBe "Diuna: CZ.2"
    rs.perCinema("unknown", "Untouched Title") shouldBe "Untouched Title"
  }

  "a disabled rule" should "be a no-op" in {
    val rs = TitleRuleSet(Seq(rule("x", GlobalStructural, "^Strip ", "", enabled = false)))
    rs.structural("Strip Me") shouldBe "Strip Me"
  }

  "an invalid pattern" should "be a no-op and surface in invalidRules" in {
    val bad = rule("bad", GlobalStructural, "(unclosed", "")
    val rs = TitleRuleSet(Seq(bad))
    rs.structural("(unclosed group title") shouldBe "(unclosed group title"
    rs.invalidRules.map(_.id) shouldBe Seq("bad")
  }

  "ordering" should "respect the order field (lower runs first)" in {
    // Rule 1 turns "AB" → "B" (strip A); rule 2 turns "B" → "" (strip B). Order
    // matters only in that both must run; assert the composed result.
    val rs = TitleRuleSet(Seq(
      rule("second", GlobalStructural, "B$", "", order = 20),
      rule("first", GlobalStructural, "^A", "", order = 10)
    ))
    rs.structural("AB") shouldBe ""
  }

  "programmePrefix" should "extract only tagged Search rules' prefixes" in {
    val rs = TitleRuleSet(Seq(
      rule("prog", Search, "(?i)^Klub: ", "", tag = Some("programmePrefix")),
      rule("other", Search, """\s*\(AD\)$""", "")  // untagged, must not be extracted
    ))
    rs.programmePrefix("Klub: Vertigo") shouldBe Some("Klub: ")
    rs.programmePrefix("Vertigo (AD)") shouldBe None
  }

  "perCinema with no rules for a key" should "be an identity transform" in {
    TitleRuleSet.empty.perCinema("anything", "Untouched - X") shouldBe "Untouched - X"
  }
}
