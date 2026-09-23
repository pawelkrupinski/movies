package tools

import models.{Country, London, Poznan}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The OG-card overlay text is written in the DEPLOYMENT's language — the per-city
 * line and the home tagline both. This guards the bug the UK expansion exposed:
 * the generator used to hardcode the Polish prefix "Repertuar kin …" for every
 * country, so a `KINOWO_COUNTRY=uk` run rendered "Repertuar kin in London" onto
 * the English cards. The per-city line now comes from the shared
 * [[controllers.FilterDescription.cityHeading]] and the home tagline branches on
 * the country language, so a non-PL deployment gets clean English (or German).
 *
 * Pure string logic — no Chrome needed (the CDP path is covered by
 * `OgCardPostersReadySpec`).
 */
class OgCardOverlaySpec extends AnyFlatSpec with Matchers {

  "The per-city overlay line" should "read plain English for a UK city, not a half-Polish mix" in {
    OgCardGenerator.cityTagline(London) shouldBe "Cinema listings in London"
    OgCardGenerator.cityTagline(London) should not include "Repertuar"
  }

  it should "stay byte-identical Polish for a Polish city" in {
    OgCardGenerator.cityTagline(Poznan) shouldBe "Repertuar kin w Poznaniu"
  }

  "The home-card tagline" should "be localized per deployment language" in {
    OgCardGenerator.homeTagline(Country.Poland)        shouldBe "Repertuar kin w Twoim mieście"
    OgCardGenerator.homeTagline(Country.UnitedKingdom) shouldBe "Cinema listings in your city"
    OgCardGenerator.homeTagline(Country.Germany)       shouldBe "Kinoprogramm in deiner Stadt"
  }
}

/**
 * When does a generator run fail its CI leg? `main` used to exit 0 however many
 * cards failed — a leg that wrote 150 of 468 US cards read as green, and only a
 * run that wrote NOTHING tripped the upload's `if-no-files-found`. Below
 * [[OgCardGenerator.MinSuccessRatio]] it now exits non-zero.
 */
class OgCardRunOutcomeSpec extends AnyFlatSpec with Matchers {

  "A run" should "pass when every card was written" in {
    OgCardGenerator.runSucceeded(ok = 468, total = 468) shouldBe true
  }

  it should "tolerate a sparse handful of failed cities" in {
    OgCardGenerator.runSucceeded(ok = 460, total = 468) shouldBe true
  }

  it should "fail when a large share of the cards failed" in {
    OgCardGenerator.runSucceeded(ok = 312, total = 468) shouldBe false // the ~1/3 blank-poster era
  }

  it should "fail the single-card home run when that card failed" in {
    OgCardGenerator.runSucceeded(ok = 0, total = 1) shouldBe false
    OgCardGenerator.runSucceeded(ok = 1, total = 1) shouldBe true
  }
}
