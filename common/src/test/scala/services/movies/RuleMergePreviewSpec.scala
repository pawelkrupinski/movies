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
    gs.head.display shouldBe "Anora"                              // the merged row's display title
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
