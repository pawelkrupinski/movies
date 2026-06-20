package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.RuleScope.{GlobalStructural, PerCinema}
import services.titlerules.{TitleRuleDefaults, TitleRuleRecord}

/** Guards [[ApplyExtraTitleRules.plan]] — the pure half of the prod upsert. The
 *  load-bearing assertion is that a PerCinema rule lands in its OWN per-cinema
 *  record (keyed by cinema slug), NOT a single bogus "PerCinema" record: the
 *  earlier `groupBy(_.scope)` did the latter, which would have duplicated every
 *  seed per-cinema rule and written a malformed record. */
class ApplyExtraTitleRulesSpec extends AnyFlatSpec with Matchers {

  private def planned(current: Seq[TitleRuleRecord]) = ApplyExtraTitleRules.plan(current)

  // Against an empty store, plan must seed each (scope, cinemaId) record afresh.
  private val fromEmpty = planned(Seq.empty)
  private val byId      = fromEmpty.map { case (rec, _) => rec.id -> rec }.toMap

  "ApplyExtraTitleRules.plan" should "key each PerCinema rule by its own cinema slug, never a single 'PerCinema' record" in {
    byId.keySet should contain noElementsOf Seq("PerCinema")
    byId.keySet should contain allElementsOf
      Seq("kino-bajka", "cyfrowe-kino", "kino-kijow",
          "kino-oskard", "kino-na-starowce", "kino-stary-mlyn", "kino-farys")
  }

  it should "place the Oskard rule in the kino-oskard record with the right scope + cinemaId" in {
    val rec = byId("kino-oskard")
    rec.scope shouldBe PerCinema
    rec.cinemaId shouldBe Some("kino-oskard")
    rec.rules.map(_.id) should contain("xtra-oskard-kino-cafe")
  }

  it should "append the global additions onto the seeded GlobalStructural baseline (never drop it)" in {
    val rec = byId("GlobalStructural")
    rec.scope shouldBe GlobalStructural
    val seedIds = TitleRuleDefaults.all.filter(r => r.scope == GlobalStructural && r.cinemaId.isEmpty && !r.last).map(_.id)
    rec.rules.map(_.id) should contain allElementsOf seedIds          // baseline preserved
    rec.rules.map(_.id) should contain("xtra-pp-najlepsze-z-najgorszych")  // new addition present
  }

  it should "be idempotent — re-planning against the freshly-written records adds nothing" in {
    planned(fromEmpty.map(_._1)) shouldBe empty
  }

  it should "leave an existing per-cinema record untouched when the additions carry none for it" in {
    // multikino already has seed rules in prod; the additions add nothing for it,
    // so it must not appear in the plan at all (nothing to upsert).
    val mkRules = TitleRuleDefaults.all.filter(_.cinemaId.contains("multikino"))
    val existingMultikino =
      TitleRuleRecord("multikino", PerCinema, Some("multikino"),
        mkRules.filterNot(_.last), mkRules.filter(_.last))
    planned(Seq(existingMultikino)).map(_._1.id) should not contain "multikino"
  }
}
