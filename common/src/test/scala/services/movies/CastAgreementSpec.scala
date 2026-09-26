package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Cast as MATCHING evidence: two lists of the same film differ in length (a venue prints
 * its top three billed, TMDB its top five) and in order (Kinoteka bills "Fiona Shaw, Daisy
 * Edgar-Jones" on one page and "Daisy Edgar-Jones, Fiona Shaw" on the next), so agreement
 * is set overlap measured against the SMALLER list — with a floor, so one shared name is
 * never strong agreement on its own.
 */
class CastAgreementSpec extends AnyFlatSpec with Matchers {

  private val tmdbTopFive = Seq("Daisy Edgar-Jones", "Esmé Creed-Miles", "George MacKay", "Caitríona Balfe", "Fiona Shaw")

  "cast agreement" should "read a venue's shorter list inside TMDB's as FULL agreement, in any order" in {
    val venue = Seq("Fiona Shaw", "Daisy Edgar-Jones", "Esme Creed-Miles")
    CastAgreement.overlap(venue, tmdbTopFive) shouldBe Some(1.0)
    CastAgreement.agrees(venue, tmdbTopFive) shouldBe true
  }

  it should "not care which way round a name is written" in {
    CastAgreement.overlap(Seq("Shaw Fiona", "Edgar-Jones Daisy", "MacKay George"), tmdbTopFive) shouldBe Some(1.0)
  }

  it should "read two lists with nobody in common as disagreement" in {
    val other = Seq("Emma Thompson", "Kate Winslet", "Hugh Grant")
    CastAgreement.overlap(other, tmdbTopFive) shouldBe Some(0.0)
    CastAgreement.agrees(other, tmdbTopFive) shouldBe false
  }

  it should "measure a partial overlap against the smaller list" in {
    // Two of three: agreement. Two of five against five: not enough.
    CastAgreement.overlap(Seq("Fiona Shaw", "George MacKay", "Hugh Grant"), tmdbTopFive) shouldBe Some(2.0 / 3)
    CastAgreement.agrees(Seq("Fiona Shaw", "George MacKay", "Hugh Grant"), tmdbTopFive) shouldBe true
    val mostlyOthers = Seq("Fiona Shaw", "George MacKay", "Hugh Grant", "Emma Thompson", "Kate Winslet")
    CastAgreement.overlap(mostlyOthers, tmdbTopFive) shouldBe Some(0.4)
    CastAgreement.agrees(mostlyOthers, tmdbTopFive) shouldBe false
  }

  it should "never let ONE shared name carry agreement on its own" in {
    // A one-name list inside TMDB's is a subset, but a single common actor is what two
    // films of one director share all the time.
    CastAgreement.overlap(Seq("Fiona Shaw"), tmdbTopFive) shouldBe Some(1.0 / CastAgreement.ComparableFloor)
    CastAgreement.agrees(Seq("Fiona Shaw"), tmdbTopFive) shouldBe false
  }

  it should "have nothing to say when either side published no cast" in {
    CastAgreement.overlap(Nil, tmdbTopFive) shouldBe None
    CastAgreement.overlap(tmdbTopFive, Seq("  ")) shouldBe None
    CastAgreement.agrees(Nil, tmdbTopFive) shouldBe false
  }
}
