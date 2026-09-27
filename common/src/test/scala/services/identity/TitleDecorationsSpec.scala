package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The venue decorations the resolver learns and strips (`TitleDecorations`). The titles below are
 *  test data: nothing here reaches the resolver as a rule. */
class TitleDecorationsSpec extends AnyFlatSpec with Matchers {

  private val listings = Seq(
    "Cinema A" -> "(4DX Rewind) Shrek", "Cinema A" -> "(4DX Rewind) Michael", "Cinema B" -> "Shrek", "Cinema C" -> "Michael",
    "Cinema D" -> "OPERA Otello", "Cinema D" -> "OPERA Manon", "Cinema E" -> "Otello", "Cinema E" -> "Manon",
    "Cinema F" -> "Lalka 2D", "Cinema F" -> "Lalka 2D PL", "Cinema G" -> "Lalka")
  private val records = Seq("Shrek", "Michael", "The Metropolitan Opera: Otello", "Lalka", "Manon")

  "Learning" should "take a run that recurs around different films' titles and no record carries" in {
    val learned = TitleDecorations.learn(listings, records)
    learned.map(d => (d.side, d.decoration, d.films)) shouldBe Seq(("prefix", "4dx rewind", 2))
    learned.head.examples shouldBe Seq("michael", "shrek")
    learned.head.venues shouldBe 1
  }

  it should "keep a run a film record's title carries: it is the film's word, not the venue's" in {
    // "OPERA" recurs around two works exactly as "(4DX Rewind)" does, but TMDB titles a record
    // "The Metropolitan Opera: Otello" — the house is part of the record's title.
    TitleDecorations.learn(listings, records).map(_.decoration) should not contain "opera"
    TitleDecorations.learn(listings, records.filterNot(_.contains("Opera"))).map(_.decoration) should contain("opera")
  }

  it should "not count a decorated spelling of one film as a second film" in {
    // "2D" is seen around "Lalka" and "Lalka 2D": one film.
    TitleDecorations.learn(listings, records).map(_.decoration) should not contain "2d"
  }

  it should "learn the same list from any order of its inputs" in {
    val reference = TitleDecorations.learn(listings, records)
    (1 to 5).foreach { seed =>
      val rnd = new scala.util.Random(seed)
      TitleDecorations.learn(rnd.shuffle(listings), rnd.shuffle(records)) shouldBe reference
    }
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

  "The resolver's artefact" should "load, and hold only what learning emits" in {
    val artefact = TitleDecorations.fromResource(TitleDecorations.ResourcePath).get
    artefact.decorations.foreach { d =>
      Set("prefix", "suffix") should contain(d.side)
      d.films should be >= TitleDecorations.MinFilms
      d.examples should not be empty
    }
    artefact.decorations shouldBe artefact.decorations.sortBy(d => (-d.films, d.side, d.decoration))
  }
}
