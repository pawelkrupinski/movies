package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Locks which scopes rewrite the stored record. The admin editor keys the
 *  per-rule "affected films" preview off `changesRecord == false`, so the split
 *  is behaviour, not documentation. */
class RuleScopeSpec extends AnyFlatSpec with Matchers {
  import RuleScope._

  "changesRecord" should "be true for the scopes that reshape the stored row (display title / documentId)" in {
    PerCinema.changesRecord shouldBe true
    Canonical.changesRecord shouldBe true
  }

  it should "be false for the external-lookup-only scope (apiQuery)" in {
    GlobalStructural.changesRecord shouldBe false
  }

  "the transient scopes" should "be exactly the external-lookup tier" in {
    RuleScope.all.filterNot(_.changesRecord) shouldBe Seq(GlobalStructural)
  }

  "all" should "no longer contain the removed Search scope" in {
    RuleScope.all shouldBe Seq(PerCinema, GlobalStructural, Canonical)
    RuleScope.all.map(_.name) should not contain "Search"
  }

  "byName" should "resolve the legacy \"Search\" name to GlobalStructural (alias)" in {
    RuleScope.byName("Search") shouldBe Some(GlobalStructural)
  }

  it should "still resolve the live scope names" in {
    RuleScope.byName("GlobalStructural") shouldBe Some(GlobalStructural)
    RuleScope.byName("Canonical") shouldBe Some(Canonical)
    RuleScope.byName("PerCinema") shouldBe Some(PerCinema)
  }
}
