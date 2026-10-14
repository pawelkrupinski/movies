package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The venue decorations the resolver strips (`TitleDecorations`); how they are learned is `TitleDecorationsLearnerSpec`'s (worker
 *  tests). The titles below are test data: nothing here reaches the resolver as a rule. */
class TitleDecorationsSpec extends AnyFlatSpec with Matchers {

  "A learned bill tail" should "make a film billed with a talk no double bill, and its title before the talk a shape" in {
    val d = TitleDecorations(Set.empty, Set.empty, Set(Seq("spotkanie", "z")))
    val talk = IdentityMeasures.Listing("Kalafior przeznaczenia + spotkanie z reżyserką", decorations = d)
    IdentityMeasures.billsTwoWorks(talk) shouldBe false
    d.strip("Kalafior przeznaczenia + spotkanie z reżyserką") should contain("Kalafior przeznaczenia")
    IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing("Basia + Kocia Szajka", decorations = d)) shouldBe true
    IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing("Basia + Kocia Szajka + spotkanie z autorką", decorations = d)) shouldBe true
  }

  "Stripping" should "cut a learned decoration off either edge, keeping the title's own text" in {
    val d = TitleDecorations(Set(Seq("4dx", "rewind")), Set(Seq("2d", "pl"), Seq("w", "helios", "na", "scenie"), Seq("pokaz", "w", "dkf")))
    d.strip("(4DX Rewind) Shrek") shouldBe Seq("Shrek")
    d.strip("André Rieu. Niech żyje Maastricht! w Helios na Scenie") shouldBe Seq("André Rieu. Niech żyje Maastricht!")
    d.strip("LALKA 2D PL") shouldBe Seq("LALKA")
    d.strip("Crash (pokaz w DKF)") shouldBe Seq("Crash")
    d.strip("Shrek") shouldBe Nil
    d.strip("2D PL") shouldBe Nil
    // Only at an edge: a decoration inside a title is part of it.
    d.strip("Shrek 2D PL Special") shouldBe Nil
  }

  it should "become a title shape, searched and read by the title relation" in {
    val d = TitleDecorations(Set(Seq("4dx", "rewind")), Set(Seq("2d", "pl")))
    val l = IdentityMeasures.Listing("(4DX Rewind) Shrek 2D PL", decorations = d)
    IdentityMeasures.titleShapes(l) should contain allOf ("Shrek 2D PL", "(4DX Rewind) Shrek", "Shrek")
    IdentityMeasures.searchQueries(l) should contain("Shrek")
    IdentityMeasures.titleRelation(l, IdentityMeasures.Film("Shrek")) shouldBe IdentityMeasures.Category("segment")
    IdentityMeasures.titleRelation(l.copy(decorations = TitleDecorations.None), IdentityMeasures.Film("Shrek")) shouldBe
      IdentityMeasures.Category("overlap")
  }

  it should "add the undecorated title's group to the venues that corroborate a listing" in {
    val d = TitleDecorations(Set.empty, Set(Seq("2d", "pl")))
    IdentityMeasures.titleGroups(IdentityMeasures.Listing("Mistyczka 2D PL", decorations = d)) shouldBe Seq("mistyczka2dpl", "mistyczka")
    IdentityMeasures.titleGroups(IdentityMeasures.Listing("Mistyczka 2D PL")) shouldBe Seq("mistyczka2dpl")
    // A delimited banner segment is not a decoration: it keeps the listing's own group only.
    IdentityMeasures.titleGroups(IdentityMeasures.Listing("Kino Seniora: Mistyczka", decorations = d)) shouldBe Seq("kinoseniora" + "mistyczka")
  }
}
