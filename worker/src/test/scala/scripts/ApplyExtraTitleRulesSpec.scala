package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.RuleScope.{Canonical, GlobalStructural, PerCinema}
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
  private val byId      = fromEmpty.map(p => p.record.id -> p.record).toMap

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
    planned(fromEmpty.map(_.record)) shouldBe empty
  }

  it should "leave an existing per-cinema record untouched when the additions carry none for it" in {
    // multikino already has seed rules in prod; the additions add nothing for it,
    // so it must not appear in the plan at all (nothing to upsert).
    val mkRules = TitleRuleDefaults.all.filter(_.cinemaId.contains("multikino"))
    val existingMultikino =
      TitleRuleRecord("multikino", PerCinema, Some("multikino"),
        mkRules.filterNot(_.last), mkRules.filter(_.last))
    planned(Seq(existingMultikino)).map(_.record.id) should not contain "multikino"
  }

  // The reconcile path: when prod carries an OLD version of a code rule (a pattern
  // since fixed in ExtraTitleRules — e.g. the Ukrainian-marker exclusion) AND the
  // id is on the --update allowlist, plan must UPDATE that rule in place.
  private val laRule = "xtra-canonical-trailing-lang-format"
  private val canonical = TitleRuleRecord.idFor(Canonical, None)
  private val codeRule  = ExtraTitleRules.all.find(_.id == laRule).getOrElse(fail("seed rule missing"))
  private val stalePattern = "(?iu)STALE-PATTERN$"
  private val staleProd = TitleRuleRecord(canonical, Canonical, None,
    Seq(codeRule.copy(pattern = stalePattern, note = Some("old"))), Seq.empty)

  it should "update an allowlisted rule in place when its code pattern has drifted" in {
    val p = ApplyExtraTitleRules.plan(Seq(staleProd), updateIds = Set(laRule))
      .find(_.record.id == canonical).getOrElse(fail("canonical not planned"))
    p.updated should contain(laRule)
    val reconciled = p.record.rules.find(_.id == laRule).get
    reconciled.pattern shouldBe codeRule.pattern          // code pattern wins
    reconciled.pattern should not be stalePattern
  }

  it should "NOT update a drifted rule that is absent from the --update allowlist" in {
    // No allowlist — the drifted rule is left ALONE (a separator-generalised prod
    // pattern is never silently reverted to the seed). The record may still be
    // planned to APPEND the other canonical extras, but nothing is UPDATED and the
    // stale pattern survives verbatim.
    val planned = ApplyExtraTitleRules.plan(Seq(staleProd), updateIds = Set.empty)
      .find(_.record.id == canonical)
    planned.foreach(_.updated shouldBe empty)
    planned.flatMap(_.record.rules.find(_.id == laRule)).map(_.pattern) shouldBe Some(stalePattern)
  }
}
