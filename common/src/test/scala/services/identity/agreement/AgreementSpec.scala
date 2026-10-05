package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, IdentityMeasures, Listing}
import services.movies.SingleCountryNormalizer

/** A cluster TMDB matched to nothing is identified by the film ≥3 film-database families each identify on their own —
 *  never when a family names another film, the title does not name it, or the listing bills several works. */
class AgreementSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.listing

  private def film(title: String, year: Int, director: String, runtime: Int = 100) =
    IdentityMeasures.Film(title, None, Nil, Some(year), Some(runtime), Some(Seq(director)), None, None)
  private val klondike2022 = film("Klondike", 2022, "Maryna Er Gorbach", 100)
  private val klondike1932 = film("Klondike", 1932, "Phil Rosen", 68)

  private def picks(listings: Seq[Listing], families: Seq[FamilyAnswers]): Seq[FamilyVerdict] =
    families.flatMap(answers => Agreement.verdict(listings, answers, NoVenueDetails, normalizer, IdentityCalibration.resolver).toOption)

  "Three families each identifying the listing" should "agree on the film, by cross-id or by its facts" in {
    val bare = Seq(listing(KinoMuza, "Klondike", year = Some(2022)))
    val families = Seq(
      new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike2022, Map("imdb" -> "tt16315948")))),
      new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q110000001" -> SourceRecord(klondike2022, Map("imdb" -> "tt16315948")))),
      new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike2022.copy(title = "Klondike", runtime = Some(101))))))
    val agreed = Agreement.agreed(bare, picks(bare, families))
    agreed.map(_.families) shouldBe Some(Set(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb))
    agreed.flatMap(_.crossId("imdb")) shouldBe Some("tt16315948")
  }

  it should "agree on nothing when another family names another film, or only two agree" in {
    val bare = Seq(listing(KinoMuza, "Klondike"))
    val three = Seq(
      new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike2022, Map("imdb" -> "tt16315948")))),
      new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q1" -> SourceRecord(klondike2022, Map("imdb" -> "tt16315948")))),
      new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike2022))))
    val other = new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map("klondike_1932" -> SourceRecord(klondike1932)))
    Agreement.agreed(bare, picks(bare, three :+ other)) shouldBe None
    Agreement.agreed(bare, picks(bare, three.take(2))) shouldBe None
  }

  it should "agree on nothing for a title that does not name the film, or a listing billing several works" in {
    val records = (family: VoterFamily) => new HeldFamilyAnswers(family, Map("1" -> SourceRecord(film("Znachor", 1937, "Michał Waszyński"))))
    val all     = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(records)
    val series  = listing(KinoMuza, "Akademia Polskiego Filmu: Kino żydowskie w Polsce")
    Agreement.agreed(Seq(series), all.map(f => FamilyVerdict.took(FamilyPick(f.family, "1", SourceRecord(film("Znachor", 1937, "Michał Waszyński")))))) shouldBe None
    val bill = listing(KinoMuza, "Znachor + Pan Tadeusz")
    Agreement.billsSeveral(bill) shouldBe true
    Agreement.agreed(Seq(bill), all.map(f => FamilyVerdict.took(FamilyPick(f.family, "1", SourceRecord(film("Znachor", 1937, "Michał Waszyński")))))) shouldBe None
  }

  it should "join a family to the agreement through any agreeing family's record, not only the first family's" in {
    // DE "Fantasy" (fixture identity-unmatched): Filmweb credits "Kukla", IMDb "Kukla Kesherovic" — a clash on their own;
    // Wikidata's record credits "Kukla" too and links IMDb's id, so the three name one film
    val imdb    = SourceRecord(film("Fantasy", 2025, "Kukla Kesherovic", 98), Map("imdb" -> "tt36112899"))
    val wiki    = SourceRecord(film("Fantasy", 2025, "Kukla").copy(runtime = None), Map("imdb" -> "tt36112899", "wikidata" -> "Q135441923"))
    val filmweb = SourceRecord(film("Fantasy", 2025, "Kukla", 98))
    val bare    = Seq(listing(KinoMuza, "Fantasy", year = Some(2025)))
    val agreed  = Agreement.agreed(bare, Seq(FamilyVerdict.took(FamilyPick(VoterFamily.Imdb, "tt36112899", imdb)),
      FamilyVerdict.took(FamilyPick(VoterFamily.Wiki, "Q135441923", wiki)), FamilyVerdict.took(FamilyPick(VoterFamily.Filmweb, "10088643", filmweb))))
    agreed.map(_.families) shouldBe Some(Set(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb))
  }

  "A family taking another film" should "be outweighed only by a margin of the quorum, or not count when the title does not name its film" in {
    // DE "Command Performance" (fixture identity-unmatched): Wikidata and Filmweb take Lundgren's 2009 film, IMDb and RT
    // lean to it, Metacritic takes Roeg's "Performance"; US "A Night at the Opera"-style: a pick the title does not name
    val command = SourceRecord(film("Command Performance", 2009, "Dolph Lundgren", 89), Map("imdb" -> "tt1210801"))
    val roeg    = SourceRecord(film("Performance", 1970, "Nicolas Roeg", 105))
    val bare    = Seq(listing(KinoMuza, "Command Performance"))
    val took    = (family: VoterFamily, record: SourceRecord) => FamilyVerdict.took(FamilyPick(family, "1", record))
    val leaned  = (family: VoterFamily) => FamilyVerdict(family, None, Seq(command), leaning = Some(command))
    val five    = Seq(took(VoterFamily.Wiki, command), took(VoterFamily.Filmweb, command), leaned(VoterFamily.Imdb), leaned(VoterFamily.RottenTomatoes))
    Agreement.agreed(bare, five :+ took(VoterFamily.Metacritic, roeg)) should not be empty
    Agreement.agreed(bare, five.dropRight(1) :+ took(VoterFamily.Metacritic, roeg)) shouldBe None
    val oldMaid = SourceRecord(film("The Old Maid", 1939, "Edmund Goulding", 95))
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki, command), took(VoterFamily.Filmweb, command), took(VoterFamily.Imdb, command),
      took(VoterFamily.Metacritic, oldMaid))) should not be empty
  }

  "A family that weighed the film the others agree on and took none, leaning to another" should "turn the agreement down" in {
    // PL "Okładka „Tempo”" (fixture identity-unmatched): a Finnish stage piece at a mime festival; Filmweb, RT and
    // Wikidata take Styles' 2003 "Tempo" on the title alone, IMDb weighed it and leans to "Old" ("Tempo" in Brazil)
    val tempo   = film("Tempo", 2003, "Eric Styles")
    val old     = SourceRecord(film("Old", 2021, "M. Night Shyamalan"), Map("imdb" -> "tt10954652"))
    val dance   = Seq(listing(KinoMuza, "Okładka „Tempo”"))
    val takers  = Seq(VoterFamily.Filmweb, VoterFamily.RottenTomatoes, VoterFamily.Wiki).map(f => FamilyVerdict.took(FamilyPick(f, "1", SourceRecord(tempo))))
    val weighed = Seq(SourceRecord(tempo, Map("imdb" -> "tt0307553")), old)
    Agreement.agreed(dance, takers) should not be empty
    Agreement.agreed(dance, takers :+ FamilyVerdict(VoterFamily.Imdb, None, weighed, leaning = Some(old))) shouldBe None
    Agreement.agreed(dance, takers :+ FamilyVerdict(VoterFamily.Imdb, None, Seq(SourceRecord(klondike1932)), leaning = Some(SourceRecord(klondike1932)))) should not be empty
  }

  it should "count as one family dissenting, which a wider agreement outweighs" in {
    // a turn-down is one family's evidence against the film, as a family taking another is — not a veto
    val tempo   = SourceRecord(film("Tempo", 2003, "Eric Styles"), Map("imdb" -> "tt0307553"))
    val old     = SourceRecord(film("Old", 2021, "M. Night Shyamalan"), Map("imdb" -> "tt10954652"))
    val bare    = Seq(listing(KinoMuza, "Tempo"))
    val takers  = Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes, VoterFamily.Metacritic)
      .map(f => FamilyVerdict.took(FamilyPick(f, "1", tempo)))
    val imdb    = FamilyVerdict(VoterFamily.Imdb, None, Seq(tempo, old), leaning = Some(old))
    Agreement.agreed(bare, takers :+ imdb) should not be empty
    Agreement.agreed(bare, takers.take(3) :+ imdb) shouldBe None
  }

  it should "not turn it down for an edition of the film itself" in {
    // UK "Ken Russell's The Devils presented by Deeper Into Movies" (fixture identity-unmatched): IMDb and Wikidata take
    // the 1971 film, Metacritic leans to it; RT weighed it and leans to its own undated "Director's Cut" page — the same
    // work, no other film
    val devils   = SourceRecord(film("The Devils", 1971, "Ken Russell", 111), Map("imdb" -> "tt0066993"))
    val cut      = SourceRecord(IdentityMeasures.Film("Ken Russell's The Devils: The Director's Cut", directors = Some(Seq("Ken Russell"))),
      Map("rt" -> "ken_russells_the_devils"))
    val billed   = Seq(listing(KinoMuza, "Ken Russell's The Devils presented by Deeper Into Movies", director = Some("Ken Russell")))
    val agreeing = Seq(FamilyVerdict.took(FamilyPick(VoterFamily.Imdb, "tt0066993", devils)), FamilyVerdict.took(FamilyPick(VoterFamily.Wiki, "Q655996", devils)),
      FamilyVerdict(VoterFamily.Metacritic, None, Seq(devils), leaning = Some(devils)))
    Agreement.agreed(billed, agreeing :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(devils, cut), leaning = Some(cut))) should not be empty
    // a record crediting no director is no edition known to be the film's, and turns it down
    val undirected = SourceRecord(IdentityMeasures.Film("Ken Russell's The Devils: The Director's Cut"))
    Agreement.agreed(billed, agreeing :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(devils, undirected), leaning = Some(undirected))) shouldBe None
  }

  it should "not turn it down when the listing's own year or director contradicts the film it leans to" in {
    // DE "Überleben" (fixture identity-unmatched): the venue credits Danial Miller in 2020; Filmweb leans to the 2022
    // "Survive" by another director
    val survive2020 = SourceRecord(film("Survive", 2020, "Danial Miller", 80), Map("imdb" -> "tt11465950"))
    val survive2022 = SourceRecord(film("Survive", 2022, "Lee Yoon-ji", 92))
    val credited    = Seq(listing(KinoMuza, "Survive", year = Some(2020), director = Some("Danial Miller")))
    val takers      = Seq(VoterFamily.Imdb, VoterFamily.Wiki).map(f => FamilyVerdict.took(FamilyPick(f, "1", survive2020)))
    val filmweb     = FamilyVerdict(VoterFamily.Filmweb, None, Seq(survive2020, survive2022), leaning = Some(survive2022))
    Agreement.agreed(credited, takers :+ filmweb) should not be empty
    Agreement.agreed(Seq(listing(KinoMuza, "Survive")), takers :+ filmweb) shouldBe None
  }

  it should "not turn it down when its evidence leans to no film at all" in {
    // US "Spider Baby" (fixture identity-unmatched): Metacritic weighed Hill's film among Spider-Man films, 5.3% the best,
    // 3.0% the next — no film its evidence favours, so no evidence against the one three families took
    val spider  = SourceRecord(film("Spider Baby or, The Maddest Story Ever Told", 1967, "Jack Hill", 81).copy(alternativeTitles = Seq("Spider Baby")),
      Map("imdb" -> "tt0058606"))
    val bare    = Seq(listing(KinoMuza, "Spider Baby"))
    val takers  = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.RottenTomatoes).map(f => FamilyVerdict.took(FamilyPick(f, "1", spider)))
    Agreement.agreed(bare, takers :+ FamilyVerdict(VoterFamily.Metacritic, None, Seq(spider))) should not be empty
  }

  it should "not turn it down while that film is the one its own evidence leans to" in {
    // PL "Sukienka" (fixture identity-unmatched): RT weighed "The Dress" at 6.0%, its runner-up at 2.7% — below its cut, but
    // the film its evidence favours; Tempo's IMDb weighed it at 2.9% under "Old" at 33%, and leaned to that
    val dress  = film("The Dress", 2020, "Tadeusz Łysiak", 30)
    val bare   = Seq(listing(KinoMuza, "Sukienka"))
    val takers = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(f => FamilyVerdict.took(FamilyPick(f, "1", SourceRecord(dress.copy(alternativeTitles = Seq("Sukienka"))))))
    val rt     = SourceRecord(dress.copy(year = None))
    Agreement.agreed(bare, takers :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(rt), leaning = Some(rt))) should not be empty
    Agreement.agreed(bare, takers :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(rt), leaning = Some(SourceRecord(klondike1932)))) shouldBe None
  }

  "Families leaning to the film others took" should "complete the agreement, never make one alone" in {
    // PL "Kafarnaum" (fixture identity-unmatched): Wikidata and Filmweb take Labaki's 2018 film; IMDb leans to it at 33.0%
    // beside 3.1%, below its cut
    val capernaum = film("Capernaum", 2018, "Nadine Labaki", 126).copy(alternativeTitles = Seq("Kafarnaum"))
    val record    = SourceRecord(capernaum, Map("imdb" -> "tt8267604"))
    val bare      = Seq(listing(KinoMuza, "Kafarnaum"))
    val took      = (family: VoterFamily) => FamilyVerdict.took(FamilyPick(family, "1", record))
    val leaned    = (family: VoterFamily) => FamilyVerdict(family, None, Seq(record), leaning = Some(record))
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), took(VoterFamily.Filmweb))) shouldBe None
    val agreed = Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), took(VoterFamily.Filmweb), leaned(VoterFamily.Imdb)))
    agreed.map(a => (a.families, a.leaning)) shouldBe Some((Set(VoterFamily.Wiki, VoterFamily.Filmweb), Set(VoterFamily.Imdb)))
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), leaned(VoterFamily.Imdb))) shouldBe None
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), leaned(VoterFamily.Filmweb), leaned(VoterFamily.Imdb))) should not be empty
    Agreement.agreed(bare, Seq(leaned(VoterFamily.Wiki), leaned(VoterFamily.Filmweb), leaned(VoterFamily.Imdb))) shouldBe None
  }

  it should "not complete it when the listing's title is only the film's translation, and another film's own — three takers do" in {
    // PL "Obcy w domu" (fixture identity-unmatched, labelled the 1986 Polish film): Wikidata and Filmweb take "Hider in the
    // House" (1989), released in Poland under that title, IMDb leaning to it — but IMDb also weighed the film whose
    // original title the listing bills
    val hider  = SourceRecord(film("Hider in the House", 1989, "Matthew Patrick", 108).copy(alternativeTitles = Seq("Obcy w domu")),
      Map("imdb" -> "tt0097503"))
    val polish = SourceRecord(IdentityMeasures.Film("Obcy w domu", Some("Obcy w domu"), Nil, Some(1986), Some(72), Some(Nil), None, None),
      Map("imdb" -> "tt7605088"))
    val bare   = Seq(listing(KinoMuza, "Obcy w domu"))
    val took   = (family: VoterFamily) => FamilyVerdict.took(FamilyPick(family, "1", hider))
    val imdb   = FamilyVerdict(VoterFamily.Imdb, None, Seq(hider, polish), leaning = Some(hider))
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), took(VoterFamily.Filmweb), imdb)) shouldBe None
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), took(VoterFamily.Filmweb), imdb.copy(weighed = Seq(hider)))) should not be empty
    Agreement.agreed(bare, Seq(took(VoterFamily.Wiki), took(VoterFamily.Filmweb), took(VoterFamily.RottenTomatoes), imdb)) should not be empty
  }

  "A family leaning to the film the model votes for" should "complete the agreement through the model's record, as it links the two" in {
    // PL "11. UFF - Demony" (fixture identity-unmatched): Filmweb takes Vorozhbit's "Demony" (2026); the model leans to
    // TMDB's "Демони" (2027, alternatively "Demony") by her, and IMDb leans to "Demons" (2026), which TMDB's record
    // names by its IMDb id though its English title is not Filmweb's
    val filmweb = SourceRecord(film("Demony", 2026, "Natalya Vorozhbit", 110), Map("filmweb" -> "10131126"))
    val tmdb    = SourceRecord(IdentityMeasures.Film("Демони", Some("Демони"), Seq("Demony", "Demons"), Some(2027), Some(110),
      Some(Seq("Наталія Ворожбит")), None, None), Map("tmdb" -> "1085176", "imdb" -> "tt20149536"))
    val imdb    = SourceRecord(film("Demons", 2026, "Natalya Vorozhbit", 110), Map("imdb" -> "tt20149536"))
    val bare    = Seq(listing(KinoMuza, "Demony"))
    val verdicts = Seq(FamilyVerdict.took(FamilyPick(VoterFamily.Filmweb, "10131126", filmweb)),
      FamilyVerdict(VoterFamily.Imdb, None, Seq(imdb), leaning = Some(imdb)))
    val agreed = Agreement.agreed(bare, verdicts, modelVote = Some(tmdb))
    agreed.map(a => (a.families, a.leaning, a.corroborated)) shouldBe Some((Set(VoterFamily.Filmweb), Set(VoterFamily.Imdb), Set(Agreement.ModelVote)))
    // the film the model voted for is TMDB's: the agreement names it by its ids
    agreed.map(a => (a.crossId("tmdb"), a.crossId("imdb"), a.crossId("filmweb"))) shouldBe Some((Some("1085176"), Some("tt20149536"), Some("10131126")))
    Agreement.agreed(bare, verdicts) shouldBe None   // no record links IMDb's to Filmweb's
  }

  "A venue's catalogue naming the film a family took by its id" should "complete two takers' agreement" in {
    // PL "Imago" (fixture identity-unmatched): the venue lists Filmweb's 872645, which Filmweb takes — Chajdas's 2023
    // film, which Metacritic takes undated
    val imago    = film("Imago", 2023, "Olga Chajdas", 113)
    val verdicts = Seq(FamilyVerdict.took(FamilyPick(VoterFamily.Filmweb, "872645", SourceRecord(imago, Map("filmweb" -> "872645")))),
      FamilyVerdict.took(FamilyPick(VoterFamily.Metacritic, "imago", SourceRecord(imago.copy(year = None), Map("metacritic" -> "imago")))))
    def catalogued(id: String) = Seq(listing(KinoMuza, "Imago", year = Some(2023)).copy(catalogueIds = Seq(services.identity.CatalogueId("filmweb", id))))
    Agreement.agreed(catalogued("872645"), verdicts).map(_.corroborated) shouldBe Some(Set(Agreement.Catalogue))
    Agreement.agreed(catalogued("999"), verdicts) shouldBe None
    // IMDb weighed Zengotita's 2025 short, "Imago" in the original, beside Chajdas's own record, "Imago" in the original
    // too: the title the venue bills is the agreed film's own, not only a translation another film's original shares
    val short   = SourceRecord(film("Imago", 2025, "Ariel Zengotita", 13).copy(originalTitle = Some("Imago")), Map("imdb" -> "tt38781946"))
    val chajdas = SourceRecord(imago.copy(originalTitle = Some("Imago")), Map("imdb" -> "tt14417122"))
    val weighed = (records: Seq[SourceRecord]) => verdicts :+ FamilyVerdict(VoterFamily.Imdb, None, records)
    Agreement.agreed(catalogued("872645"), weighed(Seq(short))) shouldBe None
    Agreement.agreed(catalogued("872645"), weighed(Seq(short, chajdas))) should not be empty
  }

  "The listing's own year and director, or the model leaning to the film," should "complete two takers' agreement" in {
    // DE "Die Story von Joanna" (fixture identity-unmatched): Wikidata and Filmweb take Damiano's 1975 film, which the venue
    // credits to him in 1975; US "AKW"-style: two takers and the TMDB film the model leans to, linked by its IMDb id
    val joanna = SourceRecord(film("Die Story von Joanna", 1975, "Gerard Damiano", 105), Map("imdb" -> "tt0073750"))
    val took   = (family: VoterFamily) => FamilyVerdict.took(FamilyPick(family, "1", joanna))
    val two    = Seq(took(VoterFamily.Wiki), took(VoterFamily.Filmweb))
    Agreement.agreed(Seq(listing(KinoMuza, "Die Story von Joanna")), two) shouldBe None
    Agreement.agreed(Seq(listing(KinoMuza, "Die Story von Joanna", year = Some(1975), director = Some("Gerard Damiano"))), two)
      .map(_.corroborated) shouldBe Some(Set(Agreement.ListingFacts))
    Agreement.agreed(Seq(listing(KinoMuza, "Die Story von Joanna", year = Some(1975), director = Some("Jess Franco"))), two) shouldBe None
    val lean = SourceRecord(IdentityMeasures.Film("", None, Nil, None, None, None, None, None), Map("tmdb" -> "40023", "imdb" -> "tt0073750"))
    Agreement.agreed(Seq(listing(KinoMuza, "Die Story von Joanna")), two, modelVote = Some(lean)).map(_.corroborated) shouldBe
      Some(Set(Agreement.ModelVote))
    Agreement.agreed(Seq(listing(KinoMuza, "Die Story von Joanna")), two, modelVote = Some(lean.copy(crossIds = Map("imdb" -> "tt0000001")))) shouldBe None
  }

  "The listing's own year, director and running time" should "complete one taker's agreement, as two votes of the venue's own" in {
    // DE "Pettersson und Findus Mitmachkino 2" (fixture identity-unmatched): IMDb alone takes it, its record dating it
    // nowhere; the venues credit its three directors and its 59 minutes
    val mitmachkino = SourceRecord(IdentityMeasures.Film("Pettersson und Findus Mitmachkino 2", None, Nil, None, Some(59), Some(Seq("Dirk Hampel")),
      None, None), Map("imdb" -> "tt41600591"))
    val imdb = Seq(FamilyVerdict.took(FamilyPick(VoterFamily.Imdb, "tt41600591", mitmachkino)))
    def billed(year: Option[Int] = Some(2024), director: Option[String] = Some("Dirk Hampel"), runtime: Option[Int] = Some(59)) =
      Seq(listing(KinoMuza, "Pettersson und Findus Mitmachkino 2", year, director, runtime))
    Agreement.agreed(billed(), imdb).map(_.corroborated) shouldBe Some(Set(Agreement.ListingFacts, Agreement.ListingRuntime))
    Agreement.agreed(billed(runtime = Some(75)), imdb) shouldBe None
    Agreement.agreed(billed(runtime = None), imdb) shouldBe None
    Agreement.agreed(billed(director = Some("Jess Franco")), imdb) shouldBe None
    Agreement.agreed(billed(director = None), imdb) shouldBe None
    // a record dating the film ten years before the venue does is not it; one within a year is
    val dated = (year: Int) => Seq(FamilyVerdict.took(FamilyPick(VoterFamily.Imdb, "tt41600591",
      mitmachkino.copy(film = mitmachkino.film.copy(year = Some(year))))))
    Agreement.agreed(billed(), dated(2014)) shouldBe None
    Agreement.agreed(billed(), dated(2023)).map(_.corroborated) shouldBe Some(Set(Agreement.ListingFacts, Agreement.ListingRuntime))
    // the running time alone is no vote: it counts only beside the venue's crediting the director
    val widely = Seq(models.KinoMuza, models.KinoApollo, models.Rialto).map(listing(_, "Pettersson und Findus Mitmachkino 2", runtime = Some(59)))
    Agreement.agreed(widely, dated(2026), thisYear = Some(2026)) shouldBe None
  }

  "Venues billing the title widely" should "complete two takers' agreement on a current film, unless a listing's facts rule it out" in {
    // UK/US "Festive Fun with Peppa Cinema Experience" (fixture identity-unmatched): IMDb and RT take the 2026 event film,
    // billed by over a hundred venues each; a one-venue "Zamki na piasku" has only its two takers
    val peppa  = SourceRecord(IdentityMeasures.Film("Festive Fun with Peppa Cinema Experience", None, Nil, Some(2026), Some(60), Some(Nil), None, None),
      Map("imdb" -> "tt46666063"))
    val takers = Seq(VoterFamily.Imdb, VoterFamily.RottenTomatoes).map(f => FamilyVerdict.took(FamilyPick(f, "1", peppa)))
    val venues = Seq(models.KinoMuza, models.KinoApollo, models.Rialto).map(listing(_, "Festive Fun with Peppa Cinema Experience"))
    Agreement.agreed(venues, takers, thisYear = Some(2026)).map(_.corroborated) shouldBe Some(Set(Agreement.Venues))
    Agreement.agreed(venues.take(2), takers, thisYear = Some(2026)) shouldBe None
    Agreement.agreed(venues, takers, thisYear = Some(2030)) shouldBe None   // an older film's namesake, billed widely now
    Agreement.agreed(venues :+ listing(models.KinoBulgarska, "Festive Fun with Peppa Cinema Experience", year = Some(2019)), takers,
      thisYear = Some(2026)) shouldBe None
  }

  "A listing naming a stage work" should "agree on none of its screen namesakes" in {
    val relay = listing(KinoMuza, "ReTransmisje Met: Na żywo w HD - Così fan tutte")
    Agreement.stagesAWork(relay) shouldBe true
    Agreement.stagesAWork(listing(KinoMuza, "Klondike")) shouldBe false
    // the work run into a house's word (PL Kino Powiśle) or after its composer (DE) is staged as well
    Agreement.stagesAWork(listing(KinoMuza, "OPERA-COSI FAN TUTTE")) shouldBe true
    Agreement.stagesAWork(listing(KinoMuza, "Met Opera 2026/27: Wolfgang Amadeus Mozart COSÌ FAN TUTTE")) shouldBe true
    val brass = film("Così fan tutte", 1992, "Tinto Brass")
    Agreement.agreed(Seq(relay), Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(f => FamilyVerdict.took(FamilyPick(f, "1", SourceRecord(brass))))) shouldBe None
  }

  "A family's questions" should "fetch a hit's record only if its search ranked it first or its title shares a word, and at most six" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    val hits  = Seq(SourceHit("a", "Something Else", None, None), SourceHit("b", "Klondike Gold", None, None),
      SourceHit("c", "Unrelated", None, None), SourceHit("d", "", None, None)) ++
      (1 to 6).map(i => SourceHit(s"k$i", s"Klondike $i", None, None))
    val family = new FamilyAnswers {
      val family: VoterFamily = VoterFamily.Imdb
      def titled(text: String)     = Answer.Known(hits)
      def directedBy(name: String) = Answer.Known(Nil)
      def record(id: String)       = { asked += id; Answer.Known(None) }
    }
    val lookups = new FamilyLookups(family, NoVenueDetails, Seq("Klondike"))
    val numbered = lookups.candidates(services.identity.CandidateQuery.Title("Klondike")).toOption.get
    numbered.foreach(hit => lookups.film(hit.tmdbId))
    asked.toSeq shouldBe Seq("a", "b", "d", "k1", "k2")   // first (any title), a shared word, no title to judge; then the cap of 6
  }

  it should "search no director on a family whose director searches never decided a take, nor a non-Latin title on an English-only one" in {
    val searched = scala.collection.mutable.ArrayBuffer.empty[String]
    def counting(of: VoterFamily) = new FamilyLookups(new FamilyAnswers {
      val family: VoterFamily = of
      def titled(text: String)     = { searched += s"${of.label} title $text"; Answer.Known(Nil) }
      def directedBy(name: String) = { searched += s"${of.label} director $name"; Answer.Known(Nil) }
      def record(id: String)       = Answer.Known(None)
    }, NoVenueDetails)
    Seq(VoterFamily.RottenTomatoes, VoterFamily.Metacritic, VoterFamily.Filmweb, VoterFamily.Imdb).foreach { family =>
      val lookups = counting(family)
      lookups.candidates(services.identity.CandidateQuery.Director("Andrzej Wajda"))
      lookups.candidates(services.identity.CandidateQuery.Title("Сталкер"))
    }
    searched.toSeq shouldBe Seq("filmweb title Сталкер", "imdb director Andrzej Wajda", "imdb title Сталкер")
  }

  "A family whose questions are not answered yet" should "pick nothing yet — a gap, not a verdict" in {
    val bare = Seq(listing(KinoMuza, "Klondike"))
    Agreement.verdict(bare, new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty, unanswered = true), NoVenueDetails, normalizer, IdentityCalibration.resolver) shouldBe Answer.Unknown
  }

  "Two records of one film in two languages" should "be the same film by director, year and a shared title word" in {
    Agreement.equivalent(film("Die Unbeugsamen", 2021, "Torsten Körner"), film("Die Unbeugsamen – The Undefeated", 2021, "Torsten Körner")) shouldBe true
    Agreement.equivalent(klondike2022, klondike1932) shouldBe false
  }

  "Two records of one film crediting its director in two transliterations" should "be the same film by title, year and running time" in {
    // DE "Maria's Lovers" (fixture identity-unmatched): Filmweb's "Andriej Konczałowski", IMDb's "Andrei Konchalovsky"
    val filmweb = film("Kochankowie Marii", 1984, "Andriej Konczałowski", 109).copy(originalTitle = Some("Maria's Lovers"))
    val imdb    = film("Maria's Lovers", 1984, "Andrei Konchalovsky", 109)
    Agreement.equivalent(filmweb, imdb) shouldBe true
    Agreement.equivalent(filmweb, imdb.copy(runtime = Some(95))) shouldBe false
    Agreement.equivalent(filmweb, imdb.copy(year = Some(1985))) shouldBe false
    Agreement.equivalent(film("Klondike", 2022, "Phil Rosen", 100), klondike2022) shouldBe false
  }
}
