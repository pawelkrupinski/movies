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

  "Learning a run seen around ONE film" should "take it when two venues carry it, its rest is a record's title and no longer record's title runs into it" in {
    // UK, Cineworld's "Girls Like Girls Unlimited Screening" (×87): the only film the run is seen
    // around, so the two-film rule never learns it, and the whole title's own search is empty.
    val cineworld = Seq("Cineworld Aberdeen", "Cineworld Swindon").map(_ -> "Girls Like Girls Unlimited Screening")
    val plain     = Seq("Odeon Birmingham" -> "Girls Like Girls")
    val empty     = Seq("Girls Like Girls Unlimited Screening" -> true, "Girls Like Girls" -> false)
    val learned   = TitleDecorations.learn(cineworld ++ plain, Seq("Girls Like Girls"), empty)
    learned.map(d => (d.side, d.decoration, d.films, d.venues)) shouldBe Seq(("suffix", "unlimited screening", 1, 2))
    learned.head.examples shouldBe Seq("girls like girls")
    // At one venue only it is indistinguishable from that venue's title for another film.
    TitleDecorations.learn(cineworld.take(1) ++ plain, Seq("Girls Like Girls"), empty) shouldBe Nil
    // A rest no record is titled is no evidence the run decorates a film.
    TitleDecorations.learn(cineworld ++ plain, Seq("Girls Like Girls 2"), empty) shouldBe Nil
  }

  it should "keep a run that starts with another film's title: the title is a programme of two" in {
    // PL, Helios's "Basia. Humor w paski mam + Kocia Szajka - Festiwal TAURON Młode Horyzonty…".
    val bill = Seq("Helios Alfa", "Helios Bielany").map(_ -> "Basia. Humor w paski mam + Kocia Szajka - Seanse HDD") :+
      ("Kino Muza" -> "Basia. Humor w paski mam")
    val empty = Seq("Basia. Humor w paski mam + Kocia Szajka - Seanse HDD" -> true)
    TitleDecorations.learn(bill, Seq("Basia. Humor w paski mam", "Kocia Szajka"), empty) shouldBe Nil
    TitleDecorations.learn(bill, Seq("Basia. Humor w paski mam"), empty).map(_.decoration) shouldBe Seq("kocia szajka seanse hdd")
  }

  it should "keep a run whose decorated title's own search found a film: the title names that one" in {
    // US Alamo's "Michael Mann's Manhunter: The Final Cut" beside the biopic "Michael": its own
    // searches find "Manhunter", so "Mann's Manhunter: The Final Cut" is no decoration of "Michael".
    val alamo = Seq("Alamo Chicago", "Alamo Omaha").map(_ -> "Michael Mann's Manhunter: The Final Cut") :+ ("AMC" -> "Michael")
    TitleDecorations.learn(alamo, Seq("Michael", "Manhunter"), Seq("Michael Mann's Manhunter: The Final Cut" -> false)) shouldBe Nil
    // Unrecorded is not empty.
    TitleDecorations.learn(alamo, Seq("Michael", "Manhunter")) shouldBe Nil
  }

  it should "keep a run a longer record's title continues into: the rest is not the film the title names" in {
    // "Friday the 13th (1980)" beside a listing of F. Gary Gray's "Friday": the title names the
    // record "Friday the 13th", whose words run on past "Friday" into the run.
    val listings = Seq("Picture House" -> "Friday the 13th (1980)", "Prince Charles" -> "Friday the 13th (1980)", "Cinema B" -> "Friday")
    val empty    = Seq("Friday the 13th (1980)" -> true)
    TitleDecorations.learn(listings, Seq("Friday", "Friday the 13th"), empty) shouldBe Nil
    TitleDecorations.learn(listings, Seq("Friday"), empty).map(_.decoration) shouldBe Seq("the 13th 1980")
  }

  "Learning what a bill joins last" should "take a run that starts it after different films' titles, as an event's tail" in {
    // PL: "Kalafior przeznaczenia + spotkanie z reżyserką", "Punku + spotkanie z reżyserem", "Lalka + PJM": a talk or a
    // signed screening billed after the film, not a second work — read as a double bill, no rule took the film alone.
    val ls = Seq("A" -> "Kalafior przeznaczenia + spotkanie z reżyserką", "B" -> "Punku + spotkanie z reżyserem",
      "A" -> "Kalafior przeznaczenia", "B" -> "Punku", "C" -> "Lalka + PJM", "C" -> "Obcy + PJM", "C" -> "Lalka", "C" -> "Obcy")
    val tails = TitleDecorations.learn(ls, Nil).filter(_.side == "tail")
    tails.map(_.decoration) should contain allOf ("spotkanie z", "pjm")
    tails.find(_.decoration == "pjm").map(_.examples) shouldBe Some(Seq("lalka", "obcy"))
  }

  it should "keep a second film billed after one film only, or one a record's title carries" in {
    // UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea" and PL "Basia… + Kocia Szajka" bill the same second
    // work after the same first: a double bill, whichever venues carry it.
    val ls = Seq("A" -> "Basia + Kocia Szajka", "B" -> "Basia + Kocia Szajka", "A" -> "Basia", "C" -> "Lalka + PJM", "C" -> "Obcy + PJM",
      "C" -> "Lalka", "C" -> "Obcy")
    TitleDecorations.learn(ls, Nil).filter(_.side == "tail").map(_.decoration) should not contain "kocia"
    TitleDecorations.learn(ls, Seq("PJM: The Movie")).filter(_.side == "tail").map(_.decoration) should not contain "pjm"
  }

  "A learned bill tail" should "make a film billed with a talk no double bill, and its title before the talk a shape" in {
    val d = TitleDecorations(Set.empty, Set.empty, Set(Seq("spotkanie", "z")))
    val talk = IdentityMeasures.Listing("Kalafior przeznaczenia + spotkanie z reżyserką", decorations = d)
    IdentityMeasures.billsTwoWorks(talk) shouldBe false
    d.strip("Kalafior przeznaczenia + spotkanie z reżyserką") should contain("Kalafior przeznaczenia")
    IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing("Basia + Kocia Szajka", decorations = d)) shouldBe true
    IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing("Basia + Kocia Szajka + spotkanie z autorką", decorations = d)) shouldBe true
  }

  "Relearning" should "keep what an earlier recording learned when the programme it was seen around has ended" in {
    // PL's "WAJDA: re-wizje …", "Seans Seniora …" and "… 30 rocznica" were learned on 09-28 and gone from the 10-03
    // recording's programme; relearned from that one recording alone they were dropped — and Kino Konesera's, BOKino's
    // and Pora dla Seniora's banners, new since 09-28, were unknown until a relearn. A banner a venue once billed around
    // two films is still a banner when it comes back.
    val earlier = TitleDecorations.learn(Seq("A" -> "Seans Seniora Lalka", "B" -> "Seans Seniora Obcy", "A" -> "Lalka", "B" -> "Obcy"), Nil)
    val now     = TitleDecorations.learn(Seq("A" -> "Kino Konesera Cwał", "A" -> "Kino Konesera Pianista", "A" -> "Cwał", "A" -> "Pianista"), Nil)
    TitleDecorations.accumulate(earlier, now, Nil).map(_.decoration) should contain theSameElementsAs Seq("seans seniora", "kino konesera")
  }

  it should "drop an earlier decoration a film record's title now carries: it is the film's word now" in {
    val earlier = TitleDecorations.learn(Seq("A" -> "Seans Seniora Lalka", "B" -> "Seans Seniora Obcy", "A" -> "Lalka", "B" -> "Obcy"), Nil)
    TitleDecorations.accumulate(earlier, Nil, Seq("Seans seniora")).map(_.decoration) shouldBe empty
  }

  it should "prefer what this recording learned of a decoration it learned again, and order the list as learning does" in {
    val earlier = TitleDecorations.learn(Seq("A" -> "Seans Seniora Lalka", "B" -> "Seans Seniora Obcy", "A" -> "Lalka", "B" -> "Obcy"), Nil)
    val now     = TitleDecorations.learn(Seq("A" -> "Seans Seniora Lalka", "B" -> "Seans Seniora Obcy", "C" -> "Seans Seniora Cwał",
      "A" -> "Lalka", "B" -> "Obcy", "C" -> "Cwał", "A" -> "Kino Konesera Cwał", "A" -> "Kino Konesera Pianista", "A" -> "Pianista"), Nil)
    val merged  = TitleDecorations.accumulate(earlier, now, Nil)
    merged.find(_.decoration == "seans seniora").map(_.films) shouldBe Some(3)
    merged shouldBe merged.sortBy(d => (-d.films, d.side, d.decoration))
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

  "Proposing candidates" should "offer a banner around films no other listing bills, which learning cannot see" in {
    val programme = Seq("Kino X" -> "Binti Edukacja Młode Horyzonty", "Kino X" -> "Fritzi – przyjaźń bez granic Edukacja Młode Horyzonty",
      "Kino X" -> "Lydia i Władca Burz Edukacja Młode Horyzonty", "Kino Y" -> "Bez końca 2D PL LOLO", "Kino Y" -> "Ministranci 2D PL LOLO",
      "Kino Y" -> "Chopin 2D PL LOLO", "Kino Z" -> "Love Actually", "Kino Z" -> "Love Story", "Kino Z" -> "Love Me Tender")
    val records = Seq("Binti", "Love Actually", "Love Story", "Love Me Tender")
    TitleDecorations.learn(programme, records) shouldBe empty   // no remainder is another listing's whole title
    val proposed = TitleDecorations.candidates(programme, records, TitleDecorations.None).map(d => (d.side, d.decoration))
    proposed should contain allOf (("suffix", "edukacja mlode horyzonty"), ("suffix", "2d pl lolo"), ("suffix", "lolo"))
    proposed should not contain (("prefix", "love"))           // a record title carries it: the film's word
  }

  it should "propose neither a run seen around fewer than three films nor a decoration already known" in {
    val few = Seq("A" -> "Binti Seans Specjalny", "A" -> "Johnny Seans Specjalny")
    TitleDecorations.candidates(few, Nil, TitleDecorations.None) shouldBe empty
    val three = few :+ ("A" -> "Fritzi Seans Specjalny")
    TitleDecorations.candidates(three, Nil, TitleDecorations.None).map(_.decoration) should contain("seans specjalny")
    TitleDecorations.candidates(three, Nil, TitleDecorations(Set.empty, Set(Seq("seans", "specjalny")))).map(_.decoration) should not contain "seans specjalny"
  }

  "Aligning one film's titles" should "take the run one venue adds around titles another bills plain, film after film" in {
    val clusters = Seq(
      Seq("Kino X" -> "Fritzi – przyjaźń bez granic Edukacja Młode Horyzonty", "Kino Y" -> "Fritzi - przyjaźń bez granic"),
      Seq("Kino X" -> "Binti Edukacja Młode Horyzonty", "Kino Z" -> "Binti"),
      Seq("Kino X" -> "Akademia Polskiego Filmu: Strachy", "Kino Y" -> "Strachy"),       // one film only: not yet a decoration
      Seq("Kino W" -> "Hamnet", "Kino V" -> "Hamnet 2D napisy"))
    TitleDecorations.aligned(clusters, TitleDecorations.None).map(d => (d.side, d.decoration, d.films)) shouldBe
      Seq(("suffix", "edukacja mlode horyzonty", 2))
    TitleDecorations.aligned(clusters, TitleDecorations.None, minFilms = 1).map(_.decoration) should contain allOf ("akademia polskiego filmu", "2d napisy")
    TitleDecorations.aligned(clusters, TitleDecorations(Set.empty, Set(Seq("edukacja", "mlode", "horyzonty")))) shouldBe empty   // known already
  }

  "Withholding decorations" should "drop exactly the named edge runs, each from its own side" in {
    val d = TitleDecorations(Set(Seq("kino", "seniora"), Seq("dkf")), Set(Seq("2d", "pl"), Seq("dkf")))
    d.without(Set("prefix" -> Seq("dkf"), "suffix" -> Seq("2d", "pl"))) shouldBe TitleDecorations(Set(Seq("kino", "seniora")), Set(Seq("dkf")))
    d.without(Set.empty) shouldBe d
  }

  "The resolver's artefact" should "load, and hold only what learning emits" in {
    val artefact = TitleDecorations.fromResource(TitleDecorations.ResourcePath).get
    artefact.decorations.foreach { d =>
      Set("prefix", "suffix", "tail") should contain(d.side)
      d.films should be >= 1
      if (d.films < TitleDecorations.MinFilms) d.venues should be >= TitleDecorations.MinVenues
      d.examples should not be empty
    }
    artefact.decorations shouldBe artefact.decorations.sortBy(d => (-d.films, d.side, d.decoration))
  }
}
