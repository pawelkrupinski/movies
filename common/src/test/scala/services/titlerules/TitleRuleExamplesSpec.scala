package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every title-rule row against its own examples (`title-rules.tsv`'s `examples` column, [[TitleRuleExamples]]): what
 *  the row's tier makes of each billing under the rules of the first country holding the row. */
class TitleRuleExamplesSpec extends AnyFlatSpec with Matchers {

  "Every row of the table" should "read each of its examples as the example says" in {
    val misread = for {
      row             <- TitleRuleTable.rows
      rules            = TitleRuleExamples.ruleSet(TitleRuleExamples.countryOf(row.rule))
      (input, wanted) <- row.examples
      read             = TitleRuleExamples.reading(row.rule, rules, input)
      if read != wanted
    } yield s"${row.rule.id}: '$input' reads '$read', the example says '$wanted'"
    misread shouldBe empty
  }

  it should "carry an example" in {
    TitleRuleTable.rows.filter(_.examples.isEmpty).map(_.rule.id) shouldBe empty
  }
}
