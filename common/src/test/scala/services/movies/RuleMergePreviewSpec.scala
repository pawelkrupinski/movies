package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRuleSet}
import RuleMergePreview.Entry

class RuleMergePreviewSpec extends AnyFlatSpec with Matchers {

  private val noRules = TitleRuleSet.empty

  // A "cinema-city" rule that strips the "Ladies Night - " decoration.
  private val withLadiesNight = TitleRuleSet(Seq(
    TitleRule("cc-ln", RuleScope.PerCinema, Some("cinema-city"),
      "^Ladies Night - ", "", applyAll = false, order = 10)))

  private val entries = Seq(
    Entry("cinema-city", "Ladies Night - Anora", Some(2024)),
    Entry("multikino",   "Anora",                Some(2024))
  )

  "groups" should "NOT merge the decorated and plain titles without the rule" in {
    RuleMergePreview.groups(noRules, entries) shouldBe Seq.empty
  }

  it should "merge them once the cinema-city strip rule is added" in {
    val gs = RuleMergePreview.groups(withLadiesNight, entries)
    gs should have size 1
    gs.head.titles shouldBe Seq("Anora", "Ladies Night - Anora") // the two raw rows that become one
    gs.head.display shouldBe "Anora"                              // search title (per-cinema merge anchor)
    gs.head.displayTitle shouldBe "Anora"                        // rendered title (display ladder)
  }

  // A merge where the cleaned spellings differ in casing/diacritics: the
  // rendered DISPLAY title runs the shared ladder, so the well-formed,
  // diacritic-bearing spelling wins regardless of which slot anchors the search.
  it should "render the well-formed display title even when a cinema ships an ALL-CAPS spelling" in {
    val mixed = Seq(
      Entry("multikino",   "DIABEL", Some(2025)), // ALL-CAPS, diacritic-flattened
      Entry("cinema-city", "Diabeł", Some(2025))  // canonical spelling
    )
    val gs = RuleMergePreview.groups(noRules, mixed)
    gs should have size 1
    gs.head.titles shouldBe Seq("DIABEL", "Diabeł")
    gs.head.displayTitle shouldBe "Diabeł" // ladder prefers diacritics + mixed case
  }

  "newMerges" should "report exactly the merge the added rule introduces" in {
    val nm = RuleMergePreview.newMerges(current = noRules, draft = withLadiesNight, entries)
    nm should have size 1
    nm.head.year shouldBe Some(2024)
  }

  "mergeKey" should "be year-agnostic in the string and stable across diacritics" in {
    RuleMergePreview.mergeKey(noRules, "x", "Diabeł") shouldBe RuleMergePreview.mergeKey(noRules, "x", "Diabel")
  }
}
