package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A source's search priors' spread, scaled about each prior's positives-weighted mean: the weights the
 *  signal-combination experiment measured (its s150 calibration, 2026-10-04), the facts' weights untouched. */
class CalibrationPriorSpreadSpec extends AnyFlatSpec with Matchers {
  private val base = IdentityCalibration.resolver
  private def rank(c: IdentityCalibration) = c.scopes(IdentityMeasures.ListingFilm).signals("search.rank")

  "a prior spread of 1.5" should "widen the search priors about their mean as the experiment's calibration did, and leave the facts alone" in {
    val wide = base.withPriorSpread(1.5)
    rank(wide).bins.head.weight shouldBe 3.7574024451854693 +- 1e-9
    rank(wide).bins(1).weight shouldBe -2.2455546123158663 +- 1e-9
    rank(wide).missing("not-returned") shouldBe -1.4560916678989106 +- 1e-9
    val facts = (c: IdentityCalibration) => c.scopes(IdentityMeasures.ListingFilm).signals.filterNot { case (name, _) => IdentityCalibration.PriorSignals(name) }
    facts(wide) shouldBe facts(base)
    wide.version shouldBe s"${base.version}-spread1.5"
  }

  "a prior spread of 1" should "be the calibration itself" in {
    base.withPriorSpread(1.0) should be theSameInstanceAs base
  }
}
