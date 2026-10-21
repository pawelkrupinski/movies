package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.IdentityCalibration.CopiedShare
import services.identity.IdentityMeasures.{ListingFilm, Measure, Number}

/** A feed's copied fact weighs its measured share of a venue's own fact (`IdentityCalibration.ScopeModel.copied`). */
class CopiedShareSpec extends AnyFlatSpec with Matchers {

  private val base  = IdentityCalibration.resolver
  private val scope = base.scopes(ListingFilm)
  private def with_(shares: (String, CopiedShare)*) =
    base.copy(scopes = base.scopes.updated(ListingFilm, scope.copy(copied = shares.toMap)))
  private val same: Map[String, Measure] = Map("year.delta" -> Number(0))
  private val off: Map[String, Measure]  = Map("year.delta" -> Number(-9))
  private val own = (m: Map[String, Measure]) => base.logOdds(ListingFilm, m)

  "a copied fact" should "weigh its feed's support share where it backs the film, its dissent share where it weighs against" in {
    val model = with_("kinoheld" -> CopiedShare(support = 0.25, dissent = 0.5))
    val prior = scope.prior
    own(same) - prior should be > 0.0
    own(off) - prior should be < 0.0
    model.logOdds(ListingFilm, IdentityMeasures.copiedBy(Some("kinoheld"), same)) - prior shouldBe ((own(same) - prior) * 0.25 +- 1e-9)
    model.logOdds(ListingFilm, IdentityMeasures.copiedBy(Some("kinoheld"), off)) - prior shouldBe ((own(off) - prior) * 0.5 +- 1e-9)
  }

  it should "weigh in full from a feed with no measured share, and a venue's own fact in full" in {
    val model = with_("kinoheld" -> CopiedShare(support = 0.25, dissent = 0.5))
    model.logOdds(ListingFilm, IdentityMeasures.copiedBy(Some("cineamo"), same)) shouldBe own(same)
    model.logOdds(ListingFilm, IdentityMeasures.copiedBy(None, same)) shouldBe own(same)
  }

  it should "never count as a fact of its own for or against the film" in {
    IdentityMeasures.isCopiedSignal(IdentityMeasures.copiedSignal("year.delta", "kinoheld")) shouldBe true
    IdentityMeasures.isCopiedSignal("year.delta") shouldBe false
  }
}
