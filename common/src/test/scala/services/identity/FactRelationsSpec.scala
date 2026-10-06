package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The one yes/no reading of two sides' published years and directors the agreement, broadcast join, corrections and
 *  measures share ([[FactRelations]]). */
class FactRelationsSpec extends AnyFlatSpec with Matchers {

  "two years" should "be near within one year, and apart only when both are stated and are not" in {
    FactRelations.yearsNear(2025, 2026) shouldBe true
    FactRelations.yearsNear(2024, 2026) shouldBe false
    FactRelations.yearsAgree(Some(2025), Some(2026)) shouldBe true
    FactRelations.yearsApart(Some(2024), Some(2026)) shouldBe true
    FactRelations.yearsApart(None, Some(2026)) shouldBe false
    FactRelations.yearsAgree(None, Some(2026)) shouldBe false
    FactRelations.nearDelta(-1.0) shouldBe true
    FactRelations.nearDelta(2.0) shouldBe false
  }

  "two credits" should "be the same person, or other people, only when both name someone" in {
    FactRelations.samePerson(Seq("Jungjae HA"), Seq("Ha Jung-jae")) shouldBe true
    FactRelations.samePerson(Nil, Seq("Ha Jung-jae")) shouldBe false
    FactRelations.otherPerson(Seq("Agnieszka Holland"), Seq("Andrzej Wajda")) shouldBe true
    FactRelations.otherPerson(Nil, Seq("Andrzej Wajda")) shouldBe false
  }

  "other people" should "share no name's stem, and be read across scripts only where the caller asks" in {
    FactRelations.otherPeople(Seq("Agnieszka Holland"), Seq("Andrzej Wajda"), acrossScripts = false) shouldBe true
    // a respelling sharing a stem is no other person
    FactRelations.otherPeople(Seq("Simona Risi"), Seq("Simona Lina Risi"), acrossScripts = false) shouldBe false
    // names in two scripts: the agreement's listing contradiction reads none, a correction's does
    FactRelations.otherPeople(Seq("Kira Muratova"), Seq("Андрей Тарковский"), acrossScripts = false) shouldBe false
    FactRelations.otherPeople(Seq("Kira Muratova"), Seq("Андрей Тарковский"), acrossScripts = true) shouldBe true
  }
}
