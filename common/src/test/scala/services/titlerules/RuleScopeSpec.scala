package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Locks which scopes rewrite the stored record. The admin editor keys the
 *  per-rule "affected films" preview off `changesRecord == false`, so the split
 *  is behaviour, not documentation. */
class RuleScopeSpec extends AnyFlatSpec with Matchers {
  import RuleScope._

  "changesRecord" should "be true for the scopes that reshape the stored row (display title / docId)" in {
    PerCinema.changesRecord shouldBe true
    Canonical.changesRecord shouldBe true
  }

  it should "be false for the external-lookup-only scopes (searchTitle / apiQuery)" in {
    GlobalStructural.changesRecord shouldBe false
    Search.changesRecord shouldBe false
  }

  "the transient scopes" should "be exactly the external-lookup tiers" in {
    RuleScope.all.filterNot(_.changesRecord) shouldBe Seq(GlobalStructural, Search)
  }
}
