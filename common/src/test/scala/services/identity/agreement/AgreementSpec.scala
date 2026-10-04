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

  "A family that weighed the film the others agree on and took none" should "turn the agreement down" in {
    val tempo   = film("Tempo", 2003, "Eric Styles")
    val dance   = Seq(listing(KinoMuza, "Okładka „Tempo”"))
    val takers  = Seq(VoterFamily.Filmweb, VoterFamily.RottenTomatoes, VoterFamily.Wiki).map(f => FamilyVerdict.took(FamilyPick(f, "1", SourceRecord(tempo))))
    Agreement.agreed(dance, takers) should not be empty
    Agreement.agreed(dance, takers :+ FamilyVerdict(VoterFamily.Imdb, None, Seq(SourceRecord(tempo, Map("imdb" -> "tt0307553"))))) shouldBe None
    Agreement.agreed(dance, takers :+ FamilyVerdict(VoterFamily.Imdb, None, Seq(SourceRecord(klondike1932)))) should not be empty
  }

  it should "not turn it down while that film is the one its own evidence leans to" in {
    // PL "Sukienka" (fixture identity-unmatched): RT weighed "The Dress" at 6.0%, its runner-up at 2.7% — below its cut, but
    // the film its evidence favours; Tempo's IMDb weighed it at 2.9% under "Old" at 33%, and leaned to that
    val dress  = film("The Dress", 2020, "Tadeusz Łysiak", 30)
    val bare   = Seq(listing(KinoMuza, "Sukienka"))
    val takers = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(f => FamilyVerdict.took(FamilyPick(f, "1", SourceRecord(dress.copy(alternativeTitles = Seq("Sukienka"))))))
    val rt     = SourceRecord(dress.copy(year = None))
    Agreement.agreed(bare, takers :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(rt))) shouldBe None
    Agreement.agreed(bare, takers :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(rt), leaning = Some(rt))) should not be empty
    Agreement.agreed(bare, takers :+ FamilyVerdict(VoterFamily.RottenTomatoes, None, Seq(rt), leaning = Some(SourceRecord(klondike1932)))) shouldBe None
  }

  "A listing naming a stage work" should "agree on none of its screen namesakes" in {
    val relay = listing(KinoMuza, "ReTransmisje Met: Na żywo w HD - Così fan tutte")
    Agreement.stagesAWork(relay) shouldBe true
    Agreement.stagesAWork(listing(KinoMuza, "Klondike")) shouldBe false
    val brass = film("Così fan tutte", 1992, "Tinto Brass")
    Agreement.agreed(Seq(relay), Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(f => FamilyVerdict.took(FamilyPick(f, "1", SourceRecord(brass))))) shouldBe None
  }

  "A family whose questions are not answered yet" should "pick nothing yet — a gap, not a verdict" in {
    val bare = Seq(listing(KinoMuza, "Klondike"))
    Agreement.verdict(bare, new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty, unanswered = true), NoVenueDetails, normalizer, IdentityCalibration.resolver) shouldBe Answer.Unknown
  }

  "Two records of one film in two languages" should "be the same film by director, year and a shared title word" in {
    Agreement.equivalent(film("Die Unbeugsamen", 2021, "Torsten Körner"), film("Die Unbeugsamen – The Undefeated", 2021, "Torsten Körner")) shouldBe true
    Agreement.equivalent(klondike2022, klondike1932) shouldBe false
  }
}
