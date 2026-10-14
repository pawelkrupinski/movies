package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The supervised decoration learners (`DecorationTokens`, `DecorationSegments`): how a matched title is aligned with its
 *  film's titles, how a title splits at its delimiters, and that a fit is a function of its rows. Titles are test data. */
class DecorationLearnersSpec extends AnyFlatSpec with Matchers {

  "Aligning a matched title" should "mark the edge words no film title covers as decoration" in {
    val a = DecorationTokens.align("Edukacja MH: Johnny 2D", Seq("Johnny", "Johnny (2022)")).get
    a.inner shouldBe Seq("johnny")
    a.decoration shouldBe IndexedSeq(true, true, false, true)
    DecorationTokens.align("Johnny", Seq("Johnny")).get.clean shouldBe true
  }

  it should "refuse a title no film title runs through, or two longest ones in different places" in {
    DecorationTokens.align("Klub Filmowy", Seq("Johnny")) shouldBe None
    DecorationTokens.align("Lalka / Lalka", Seq("Lalka")) shouldBe None
  }

  "Splitting at delimiters" should "name the delimiter on each side of every segment" in {
    val segs = DecorationSegments.segments("Akademia Polskiego Filmu: Strachy | Kino Pałacowe").get
    segs.map(s => (s.key, s.before, s.after)) shouldBe IndexedSeq(("akademia polskiego filmu", "edge", "colon"), ("strachy", "colon", "pipe"),
      ("kino palacowe", "pipe", "edge"))
    DecorationSegments.segments("„Baranek Shaun” Rodzinne Poranki").get.map(s => (s.key, s.before, s.after)) shouldBe
      IndexedSeq(("baranek shaun", "edge", "quote"), ("rodzinne poranki", "quote", "edge"))
  }

  it should "keep a title billing several works whole: a plus, or two quoted titles" in {
    DecorationSegments.billsSeveral(DecorationSegments.segments("Historia kina w Popielawach + Pruska kultura").get) shouldBe true
    DecorationSegments.billsSeveral(DecorationSegments.segments("Akademia: „Historia kina” i „Pruska kultura”").get) shouldBe true
    DecorationSegments.billsSeveral(DecorationSegments.segments("Akademia Polskiego Filmu: Strachy").get) shouldBe false
  }

  it should "label segments by the words the film's title covers, and refuse a title starting inside one" in {
    val segs = DecorationSegments.segments("Akademia Polskiego Filmu: Strachy").get
    DecorationSegments.labels(segs, DecorationTokens.align("Akademia Polskiego Filmu: Strachy", Seq("Strachy")).get) shouldBe Some(IndexedSeq(true, false))
    DecorationSegments.labels(segs, DecorationTokens.align("Akademia Polskiego Filmu: Strachy", Seq("Filmu Strachy")).get) shouldBe None
  }

  "A logistic fit" should "learn the separating feature, and give the same weights from any order of its rows" in {
    val rows = (1 to 40).map(i => (Array(1.0, if (i % 2 == 0) 1.0 else 0.0, (i % 5) / 5.0), if (i % 2 == 0) 1.0 else 0.0))
    val w = LogisticFit.fit(rows.map(_._1).toArray, rows.map(_._2).toArray, 1.0, 25)
    w(1) should be > 1.0
    val shuffled = new scala.util.Random(3).shuffle(rows)
    LogisticFit.fit(shuffled.map(_._1).toArray, shuffled.map(_._2).toArray, 1.0, 25) shouldBe w
  }
}
