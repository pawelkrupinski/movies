package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Each title-rule row removed in turn: one of its examples must read otherwise without it, which proves the row is
 *  load-bearing. A row none of whose examples moves is fully covered — by another row or by the billing parse the lookup
 *  query reads (`BillingParse.work`) — and is listed in [[TitleRuleMutationSpec.Covered]], the merge or removal
 *  candidates of docs/investigations/2026-10-22-rule-consolidation.md ("Examples and the mutation check"). The list
 *  must stay exact: a covered row made load-bearing again, or a load-bearing one covered, fails here. */
class TitleRuleMutationSpec extends AnyFlatSpec with Matchers {
  import TitleRuleMutationSpec.Covered

  "Every row of the table" should "change one of its examples when removed, save the rows another covers" in {
    val covered = TitleRuleTable.rows.filterNot { row =>
      val country = TitleRuleExamples.countryOf(row.rule)
      val (full, without) = (TitleRuleExamples.ruleSet(country), TitleRuleExamples.ruleSet(country, Set(row.rule.id)))
      row.examples.exists { case (input, _) =>
        TitleRuleExamples.reading(row.rule, full, input) != TitleRuleExamples.reading(row.rule, without, input)
      }
    }.map(_.rule.id).toSet
    withClue("newly covered (merge or remove it, or give it an example only it reads): ")(covered.diff(Covered) shouldBe empty)
    withClue("load-bearing again (take it off the list): ")(Covered.diff(covered) shouldBe empty)
  }
}

object TitleRuleMutationSpec {
  /** The rows whose examples another row or the billing parse reads alike without them. */
  val Covered: Set[String] = Set(
    "canonical-gwiezdne-wojny",   // xtra-canonical-gwiezdne-wojny-ci folds every casing; kept as the seed's (ProdTitlesNormalizationSpec)
    "helios-suffix-3",            // xtra-helios-na-scenie takes " w Helios na Scenie" in any case
    "xtra-paren-pokaz-seans",     // xtra-paren-screening-event takes a bracket naming a pokaz or a seans
    "xtra-filmoterapia-w-kinie",  // the billing parse: "FILMOTERAPIA W LUNIE | <film> | seans" is a strand's piece
    "xtra-pokaz-colon-suffix")    // the billing parse: "<film>: pokaz z prelekcją …" closes on a piece opening on "pokaz"
}
