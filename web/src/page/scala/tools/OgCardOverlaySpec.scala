package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The home card's overlay text is written in the DEPLOYMENT's language. This guards the bug the UK
 * expansion exposed: the generator used to hardcode Polish for every country, so a
 * `KINOWO_COUNTRY=uk` run rendered Polish onto the English cards.
 *
 * Pure string logic — no Chrome needed (the CDP path is covered by `OgCardPostersReadySpec`).
 */
class OgCardOverlaySpec extends AnyFlatSpec with Matchers {

  "The home-card tagline" should "be localized per deployment language" in {
    OgCardGenerator.homeTagline(Country.Poland)        shouldBe "Repertuar kin w Twoim mieście"
    OgCardGenerator.homeTagline(Country.UnitedKingdom) shouldBe "Cinema listings in your city"
    OgCardGenerator.homeTagline(Country.Germany)       shouldBe "Kinoprogramm in deiner Stadt"
  }
}
