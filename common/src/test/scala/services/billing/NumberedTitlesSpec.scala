package services.billing

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberedTitlesSpec extends AnyFlatSpec with Matchers {

  "A title's instalment" should "be the number closing its series" in {
    BillingParse("Der Herr der Ringe 2 (Extended Version)").instalment shouldBe Some(2)
    BillingParse("Dune Part Two").instalment shouldBe Some(2)
    BillingParse("Rocky IV").instalment shouldBe Some(4)
    BillingParse("2026 - 2027 Leadership Video Part 1").instalment shouldBe Some(1)
  }

  it should "be none where a number names the title, opening it as well as closing it" in {
    // the parse read "13 GOING ON 30" as the 30th instalment of "13 going on"
    BillingParse("13 GOING ON 30").instalment shouldBe None
    BillingParse("9 to 5").instalment shouldBe None
    BillingParse("1 IN 2").instalment shouldBe None
  }

  it should "be none where the number is a date's or a season's, joined to another by a slash" in {
    // the parse read "$5 Secret Movie 10/19/26" as instalment 10
    BillingParse("$5 Secret Movie 10/19/26").instalment shouldBe None
    BillingParse("Baby Comptines 02/12/2026").instalment shouldBe None
    BillingParse("RB&O 26/27: Carmen").instalment shouldBe None
    BillingParse("Berliner Philharmoniker 25/26: Silvesterkonzert").instalment shouldBe None
  }

  "An instalment's subtitle" should "leave out a closing version tag in any language the parse reads one" in {
    BillingParse("The Lord of the Rings: The Two Towers – Extended").subtitle.map(_.words) shouldBe Some(Seq("towers"))
    BillingParse("The Lord of the Rings - Extended Edition").subtitle shouldBe None
    BillingParse("Władca Pierścieni: Dwie wieże – wersja reżyserska").subtitle.map(_.words) shouldBe Some(Seq("dwie", "wieze"))
    BillingParse("El Señor de los Anillos: Las dos torres – versión extendida").subtitle.map(_.words) shouldBe Some(Seq("torres"))
    BillingParse("Kiler: wersja reżyserska").subtitle shouldBe None
    BillingParse("Terminator 2: Judgment Day – Remastered 4K").subtitle.map(_.words) shouldBe Some(Seq("judgment"))
    BillingParse("Harry Potter and the Deathly Hallows: Part 1").subtitle shouldBe None
  }
}
