package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures
import services.identity.IdentityMeasures.{Film, Listing}

/** The calibration's LABEL rule: what a listing's own evidence says about production's proposal. */
class IdentityCalibrateSpec extends AnyFlatSpec with Matchers {

  private def m(l: Listing, f: Film) = IdentityMeasures.listingFilm(l, f, None, 0, 0)

  "a proposal" should "be denied by the title when the listing names ANOTHER film of its own search more closely" in {
    // Cinesa's "Vengadores: Endgame (Reestreno)", 2026, the Russos: filed under Avengers: Doomsday
    // (2026, the Russos) — year and director agree, but the title names Endgame, which its own
    // search returned.
    val listing  = Listing("Vengadores: Endgame (Reestreno)", year = Some(2026), directors = Seq("Anthony Russo", "Joe Russo"))
    val doomsday = Film("Vengadores: Doomsday", year = Some(2026), directors = Some(Seq("Anthony Russo", "Joe Russo")))
    val endgame  = Film("Vengadores: Endgame", year = Some(2019), directors = Some(Seq("Anthony Russo", "Joe Russo")))
    val (_, deny) = IdentityCalibrate.proposalAgreement(m(listing, doomsday), Seq(m(listing, endgame)))
    deny should contain ("title")
  }

  it should "not be denied by the title when no other film of its search is named more closely" in {
    // A truncated spelling names its film only by shared words — and nothing names another film better.
    val listing = Listing("Niesamowite przygody skarpetek 3. Ale ko", year = Some(2026))
    val film    = Film("Niesamowite przygody skarpetek 3. Ale kosmos!", year = Some(2026))
    val (agree, deny) = IdentityCalibrate.proposalAgreement(m(listing, film), Seq(m(listing, Film("Skarpety", year = Some(2020)))))
    deny shouldBe empty
    agree should contain ("year")
  }
}
