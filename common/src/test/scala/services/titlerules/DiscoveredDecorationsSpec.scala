package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/** The decorations the discovery bot proposed ([[ExtraTitleRules.discovered]]): each strips its own measured venue
 *  titles to the search title it was measured with, none of which the rules without it already give, and each is a
 *  query-only strip — the screening keeps its own row. */
class DiscoveredDecorationsSpec extends AnyFlatSpec with Matchers {

  private val every = TitleRules.all ++ ExtraTitleRules.all
  private val full  = new TitleNormalizer(TitleRuleSet(every))

  "Each discovered decoration" should "be a search strip with its own id and at least one example" in {
    ExtraTitleRules.discovered.map(_.rule.id).distinct should have size ExtraTitleRules.discovered.size.toLong
    ExtraTitleRules.discovered.foreach { d =>
      d.rule.id should startWith("xtra-discovered-")
      d.rule.scope shouldBe RuleScope.GlobalStructural
      d.rule.replacement shouldBe ""
      d.rule.patternValid shouldBe true
      d.examples should not be empty
    }
  }

  it should "strip its measured titles, which the rules without it do not" in {
    ExtraTitleRules.discovered.foreach { d =>
      val without = new TitleNormalizer(TitleRuleSet(every.filterNot(_.id == d.rule.id)))
      d.examples.foreach { case (title, searched) =>
        withClue(s"${d.rule.id} on \"$title\": ") {
          full.apiQuery(title).trim shouldBe searched
          without.apiQuery(title).trim should not be searched
        }
      }
    }
  }
}
