package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The record domain: how a `TitleRuleRecord` flattens to the flat `Seq[TitleRule]`
 *  the rest of the app consumes, and how a flat seq groups back into records
 *  (the seeding + migration path). */
class TitleRuleRecordSpec extends AnyFlatSpec with Matchers {
  import RuleScope._

  private def r(id: String, scope: RuleScope, cinema: Option[String], pattern: String,
                order: Int = 0, last: Boolean = false): TitleRule =
    TitleRule(id, scope, cinema, pattern, "", applyAll = false, order = order, last = last)

  "toRules" should "stamp order from list position and last from which list, forcing scope/cinema" in {
    val record = TitleRuleRecord("cinema-city", PerCinema, Some("cinema-city"),
      // deliberately give the embedded rules a wrong order/scope to prove toRules overrides them
      rules     = Seq(r("a", GlobalStructural, None, "a", order = 99), r("b", GlobalStructural, None, "b", order = 99)),
      lastRules = Seq(r("z", GlobalStructural, None, "z", order = 99)))
    val flat = record.toRules
    flat.map(x => (x.id, x.order, x.last)) shouldBe Seq(("a", 0, false), ("b", 1, false), ("z", 0, true))
    flat.foreach { x => x.scope shouldBe PerCinema; x.cinemaId shouldBe Some("cinema-city") }
  }

  "a last rule" should "apply after the normal rules of the same scope (via TitleRuleSet)" in {
    // Order-dependent strips: "ab" then "a". On "aab", [ab, a] → "" but [a, ab] → "b".
    val ab = TitleRule("ab", GlobalStructural, None, "ab", "", applyAll = true, order = 0)
    val a  = TitleRule("a",  GlobalStructural, None, "a",  "", applyAll = true, order = 1)
    TitleRuleSet(Seq(ab, a)).search("aab") shouldBe ""
    TitleRuleSet(Seq(ab.copy(last = true), a)).search("aab") shouldBe "b"
  }

  "fromRules" should "group by (scope, cinema), split last into lastRules, and order each list" in {
    val rules = Seq(
      r("s2", GlobalStructural, None, "b", order = 20),
      r("s1", GlobalStructural, None, "a", order = 10),
      r("sl", GlobalStructural, None, "z", order = 5, last = true),  // a last rule with a low order
      r("cc", PerCinema, Some("cinema-city"), "x"))
    val recs = TitleRuleRecord.fromRules(rules)

    val gs = recs.find(_.scope == GlobalStructural).getOrElse(fail("no GlobalStructural record"))
    gs.id shouldBe "GlobalStructural"
    gs.cinemaId shouldBe None
    gs.rules.map(_.id) shouldBe Seq("s1", "s2")   // ordered by `order`, NOT mixed with the last one
    gs.lastRules.map(_.id) shouldBe Seq("sl")

    val cc = recs.find(_.scope == PerCinema).getOrElse(fail("no PerCinema record"))
    cc.id shouldBe "cinema-city"
    cc.cinemaId shouldBe Some("cinema-city")
  }

  "fromRules then toRules" should "preserve the whole default rule set (modulo synthesized order)" in {
    val rules = TitleRuleDefaults.all
    val back  = TitleRuleRecord.fromRules(rules).flatMap(_.toRules)
    back.map(_.id).toSet shouldBe rules.map(_.id).toSet
    back.size shouldBe rules.size
    // a default per-cinema rule lands under its cinema's record id
    TitleRuleRecord.fromRules(rules).map(_.id) should contain ("GlobalStructural")
  }
}

/** The in-memory store is a plain map keyed by record id; the business logic
 *  (flattening to `findAll`) lives above the seam on the trait. */
class InMemoryTitleRulesRepositorySpec extends AnyFlatSpec with Matchers {
  "InMemoryTitleRulesRepository" should "round-trip records and derive findAll by flattening" in {
    val repository = new InMemoryTitleRulesRepository()
    val record = TitleRuleRecord("GlobalStructural", RuleScope.GlobalStructural, None,
      rules     = Seq(TitleRule("a", RuleScope.GlobalStructural, None, "x", "", applyAll = false, order = 0)),
      lastRules = Seq(TitleRule("b", RuleScope.GlobalStructural, None, "y", "", applyAll = false, order = 0, last = true)))
    repository.upsertRecord(record)

    repository.loadRecords().map(_.id) shouldBe Seq("GlobalStructural")
    repository.findAll().map(r => (r.id, r.last)) shouldBe Seq(("a", false), ("b", true))

    repository.deleteRecord("GlobalStructural")
    repository.loadRecords() shouldBe empty
    repository.findAll() shouldBe empty
  }
}
