package services.movies

import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators.{genName, latinNames}

/** `SamePerson` as a relation: symmetric, reflexive on anything non-blank, and
 *  built on a tokeniser that is a fixpoint of its own output. */
class SamePersonPropertySpec extends IdentityPropertySpec {

  /** Real credit shapes, plus arbitrary text with the letters the fold treats
   *  specially (undecomposable ı/ø/đ/ß, Polish diacritics, initials). */
  private val genCredit: Gen[String] = Gen.frequency(
    3 -> genName,
    1 -> Gen.alphaStr,
    1 -> Gen.choose(0, 12).flatMap(Gen.listOfN(_, Gen.oneOf("abcdeijklmnorstuvwzłóżźćńąęśİıøØßđĐ. -'".toSeq))).map(_.mkString))

  "SamePerson" should "be symmetric" in {
    forAll(genCredit, genCredit) { (a, b) =>
      SamePerson(a, b) shouldBe SamePerson(b, a)
    }
  }

  it should "be reflexive for any non-blank credit" in {
    forAll(genCredit.suchThat(_.trim.nonEmpty)) { a =>
      SamePerson(a, a) shouldBe true
    }
  }

  "SamePerson.tokens" should "be a fixpoint: re-tokenising the joined tokens changes nothing" in {
    forAll(genCredit) { a =>
      val tokens = SamePerson.tokens(a)
      SamePerson.tokens(tokens.mkString(" ")) shouldBe tokens
    }
  }

  it should "read every Latin credit in the pool as at least one token" in {
    latinNames.foreach(name => SamePerson.tokens(name) should not be empty)
  }
}
