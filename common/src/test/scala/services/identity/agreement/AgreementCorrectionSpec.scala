package services.identity.agreement

import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, IdentityMeasures, Listing, PosterAnswers, PosterHash, Resolution, ResolverDecision, ScreeningDays}
import services.movies.SingleCountryNormalizer

/** The model's own takes on their way to the projection, read against the evidence that can contradict them
 *  ([[Correction]]): withdrawn when the venues' Filmweb programmes contradict the take, or when the posters and the
 *  families name another film sharing no director with it; switched where two kinds of evidence name the same film. */
class AgreementCorrectionSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private val venuePoster = "https://kino.example/lalka.jpg"
  private val shown       = PosterHash(0x5a5a5a5a5a5aL)
  private def near(bits: Int) = PosterHash(shown.bits ^ ((1L << bits) - 1))
  private val far         = PosterHash(-1L)

  /** TMDB's two "Lalka"s: Kawalski's 2026 film the model takes, and Has's 1968 one (PL Kino za Rogiem, 2026-10-06). */
  private def table(hasDirector: String = "Wojciech Has") =
    new FilmTable(Seq(FilmTable.F(1001, "Lalka", 2026, "Maciej Kawalski", 162), FilmTable.F(1002, "Lalka", 1968, hasDirector, 159)), normalizer)
  private val day     = java.time.LocalDate.parse("2026-10-06")
  private val listing = FilmTable.listing(Multikino, "Lalka").copy(poster = Some(venuePoster), screenings = ScreeningDays.of(Seq(day)))
  private val model   = Resolution(Seq(ResolverDecision(Seq(listing.key), Some(1001), 0.9, ResolverDecision.Basis.OwnMatch, Seq("own match 1001"))()),
    1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  private def listingOf: services.movies.ListingKey => Option[Listing] = Map(listing.key -> listing).get

  private val has = SourceRecord(IdentityMeasures.Film("Lalka", None, Seq("The Doll"), Some(1968), Some(159), Some(Seq("Wojciech Has")), None, None), Map("tmdb" -> "1002"))
  private def silent = VoterFamily.values.map(family => family -> new HeldFamilyAnswers(family, Map.empty)).toMap[VoterFamily, FamilyAnswers]
  /** IMDb and Wikidata take Has's film; the rest find nothing. */
  private def takingHas = silent ++ Seq(VoterFamily.Imdb, VoterFamily.Wiki).map(family => family -> new HeldFamilyAnswers(family, Map("x" -> has)))
  /** The venue's poster is Has's film's: far from Kawalski's. */
  private val hasPoster = new HeldPosters(Map(venuePoster -> Some(shown)), Map(1001 -> Seq(far), 1002 -> Seq(near(3))))

  private def stage(families: Map[VoterFamily, FamilyAnswers], posters: PosterAnswers, lookups: FilmTable = table(),
                    listedOn: Option[VoterFamily] = None, changes: AnswerChanges = AnswerChanges.Unknown) =
    new AgreementStage(families, lookups, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      clock = _root_.tools.SpecClock.Pinned, changes = changes, posters = posters, tmdb = Some(lookups), listedOn = listedOn)
  private def decided(s: AgreementStage, version: Long = 1) = s.apply(model, listingOf, version).decisions.head

  "a model take" should "switch to the film the venue poster and more families name, sharing no director with the take" in {
    val taken = decided(stage(takingHas, hasPoster))
    (taken.basis, taken.film) shouldBe ((ResolverDecision.Basis.Corrected, Some(1002)))
    taken.explanation.last should startWith ("corrected from 'Lalka' (2026) by families and poster — ")
    taken.explanation.last should include ("a venue poster matches 'Lalka' (1968) (3 bits), not the take")
    taken.explanation.last should include ("imdb, wiki take 'Lalka' (1968), none the take")
  }

  it should "stand when the poster alone names another film, or the films share a director (a re-release's record)" in {
    decided(stage(silent, hasPoster)).basis shouldBe ResolverDecision.Basis.OwnMatch
    decided(stage(takingHas, hasPoster, table(hasDirector = "Maciej Kawalski"))).basis shouldBe ResolverDecision.Basis.OwnMatch
  }

  it should "stand unread on a cluster that is an event, not a film: no poster hashed, no family asked" in {
    val handed  = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    val concert = listing.copy(rawTitle = "Koncert: Lalka", title = "Koncert: Lalka", cleanTitle = "Koncert: Lalka")
    val s = new AgreementStage(takingHas, table(), normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = handed += _, clock = _root_.tools.SpecClock.Pinned, posters = new HeldPosters(Map.empty, Map.empty), tmdb = Some(table()))
    s.apply(model, Map(listing.key -> concert).get, 1).decisions.head.basis shouldBe ResolverDecision.Basis.OwnMatch
    (s.wantedPosters, s.wanted, handed.toSeq) shouldBe ((Set.empty, Set.empty, Seq.empty))
  }

  it should "read the other candidates' posters only when the take's own are far from the venue's, and ask the families only then" in {
    val handed  = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    // nothing hashed: the venue's poster and the take's are asked for, no other candidate's
    val waiting = new AgreementStage(takingHas, table(), normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = handed += _, clock = _root_.tools.SpecClock.Pinned, posters = new HeldPosters(Map.empty, Map.empty), tmdb = Some(table()))
    decided(waiting).basis shouldBe ResolverDecision.Basis.OwnMatch
    waiting.wantedPosters shouldBe Set(AgreementStage.PosterQuestion.Venue(venuePoster), AgreementStage.PosterQuestion.Film(1001))
    // the take's own poster matches the venue's: no veto can stand, nothing more is asked
    val own = stage(takingHas, new HeldPosters(Map(venuePoster -> Some(shown)), Map(1001 -> Seq(near(2)))))
    decided(own).basis shouldBe ResolverDecision.Basis.OwnMatch
    (own.wantedPosters, own.wanted) shouldBe ((Set.empty, Set.empty))
  }

  it should "be withdrawn when a poster questions it and its venues' Filmweb programme lists a film it contradicts, switched when both name that film" in {
    def programme(record: SourceRecord, on: java.time.LocalDate = day) = silent + (VoterFamily.Filmweb ->
      new HeldFamilyAnswers(VoterFamily.Filmweb, Map("1174" -> record), programmes = Map(listing.venue -> Seq(Showing("1174", ScreeningDays.of(Seq(on)))))))
    val onFilmweb = SourceRecord(IdentityMeasures.Film("Lalka", None, Nil, Some(1968), None, Some(Seq("Wojciech Has"))))
    // the programme's 1968 "Lalka" is a film TMDB's records here are not (another director): withdrawn, nothing to switch to
    val rybkowski = SourceRecord(IdentityMeasures.Film("Lalka", None, Nil, Some(1968), None, Some(Seq("Jan Rybkowski"))))
    val withdrawn = decided(stage(programme(rybkowski), hasPoster, listedOn = Some(VoterFamily.Filmweb)))
    (withdrawn.basis, withdrawn.film) shouldBe ((ResolverDecision.Basis.Withdrawn, None))
    withdrawn.explanation.last shouldBe "withdrawn 'Lalka' (2026) — filmweb: the venues' Filmweb programmes list 'Lalka' (1968) on their days; " +
      "poster: a venue poster matches 'Lalka' (1968) (3 bits), not the take"
    val switched = decided(stage(programme(onFilmweb), hasPoster, listedOn = Some(VoterFamily.Filmweb)))
    (switched.basis, switched.film) shouldBe ((ResolverDecision.Basis.Corrected, Some(1002)))
    switched.explanation.last should startWith ("corrected from 'Lalka' (2026) by filmweb and poster — ")
    // no poster questioning the take: its venues' programme is not asked at all
    val unasked = stage(programme(onFilmweb), new HeldPosters(Map(venuePoster -> Some(shown)), Map(1001 -> Seq(near(1)))), listedOn = Some(VoterFamily.Filmweb))
    decided(unasked).basis shouldBe ResolverDecision.Basis.OwnMatch
    unasked.wanted shouldBe empty
    // a programme listing the take itself, or the film on another day, contradicts nothing
    val kawalski = SourceRecord(IdentityMeasures.Film("Lalka", None, Nil, Some(2026), None, Some(Seq("Maciej Kawalski"))))
    decided(stage(programme(kawalski), hasPoster, listedOn = Some(VoterFamily.Filmweb))).basis shouldBe ResolverDecision.Basis.OwnMatch
    // (on another day, Filmweb's own take of the title is what joins the poster)
    decided(stage(programme(onFilmweb, day.plusDays(1)), hasPoster, listedOn = Some(VoterFamily.Filmweb))).explanation.last should (
      include ("families: filmweb take") and not include ("programmes list"))
  }

  it should "hand the same decision back while nothing it read is filed again, and decide again once an answer it read is" in {
    var films  = Map(1001 -> Seq(far))
    val posters = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] = Answer.Known(Some(shown))
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = films.get(tmdbId).fold[Answer[Seq[PosterHash]]](Answer.Unknown)(Answer.Known(_))
    }
    var filed = Set.empty[String]
    val s = stage(takingHas, posters, changes = _ => Some(filed))
    val first = decided(s)
    first.basis shouldBe ResolverDecision.Basis.OwnMatch
    s.wantedPosters shouldBe Set(AgreementStage.PosterQuestion.Film(1002))
    // a filing it never read decides nothing again
    films = films + (1002 -> Seq(near(3)))
    filed = Set("imdb|title|Something else")
    decided(s, version = 2).basis shouldBe ResolverDecision.Basis.OwnMatch
    // the poster it waited on, filed
    filed = Set("poster|film|1002")
    val second = decided(s, version = 3)
    second.basis shouldBe ResolverDecision.Basis.Corrected
    decided(s, version = 3) should be theSameInstanceAs second
  }

  it should "be trusted as stored after a restart, reading no answer, and read afresh only a few takes an apply" in {
    val verdicts = new InMemoryAgreementVerdicts
    def stageOver(posters: PosterAnswers, perApply: Int = AgreementStage.CorrectionsPerApply) =
      new AgreementStage(takingHas, table(), normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), verdicts,
        clock = _root_.tools.SpecClock.Pinned, posters = posters, tmdb = Some(table()), correctionsPerApply = perApply)
    decided(stageOver(hasPoster)).basis shouldBe ResolverDecision.Basis.Corrected
    var read = 0
    val counting = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] = { read += 1; hasPoster.venue(url) }
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = { read += 1; hasPoster.film(tmdbId) }
    }
    decided(stageOver(counting)).basis shouldBe ResolverDecision.Basis.Corrected
    read shouldBe 0
    // a take never read stands as the model took it while the apply's budget is spent
    decided(new AgreementStage(takingHas, table(), normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      clock = _root_.tools.SpecClock.Pinned, posters = hasPoster, tmdb = Some(table()), correctionsPerApply = 0)).basis shouldBe ResolverDecision.Basis.OwnMatch
  }

  "Correction.decide" should "withdraw on the programme alone, switch only where two kinds name one film apart from the take" in {
    import Correction.{Against, Outcome}
    val apart = (_: Int) => Some(false)
    val programme = Against(Correction.Filmweb, None, "p")
    val poster    = Against(Correction.Poster, Some(7), "q")
    val families  = Against(Correction.Families, Some(7), "f")
    Correction.decide("m", Seq(programme), apart).map(_.film) shouldBe Some(None)
    Correction.decide("m", Seq(poster), apart) shouldBe None
    Correction.decide("m", Seq(families), apart) shouldBe None
    Correction.decide("m", Seq(poster, families), apart) shouldBe Some(Outcome(Some(7), "corrected from m by families and poster — poster: q; families: f"))
    Correction.decide("m", Seq(poster, families), _ => Some(true)) shouldBe None
    Correction.decide("m", Seq(poster, families), _ => None) shouldBe None
    Correction.decide("m", Seq(programme.copy(film = Some(7)), poster), apart).flatMap(_.film) shouldBe Some(7)
    Correction.decide("m", Seq(programme.copy(film = Some(7)), poster), _ => Some(true)).map(_.film) shouldBe Some(None)
    Correction.decide("m", Seq(poster, families.copy(film = Some(8))), apart) shouldBe None
  }

  it should "count a contradiction only by the year or another director, never by a fact missing" in {
    val kawalski = IdentityMeasures.Film("Lalka", None, Nil, Some(2026), None, Some(Seq("Maciej Kawalski")))
    Correction.contradicts(kawalski.copy(year = Some(1968), directors = None), kawalski) shouldBe true
    Correction.contradicts(kawalski.copy(directors = Some(Seq("Wojciech Has"))), kawalski) shouldBe true
    Correction.contradicts(kawalski.copy(year = Some(2025), directors = Some(Seq("Maciej Kawalski"))), kawalski) shouldBe false
    Correction.contradicts(kawalski.copy(year = None, directors = None), kawalski) shouldBe false
    Correction.shareDirector(Seq("Simona Risi"), Seq("Simona Lina Risi")) shouldBe Some(true)
    Correction.shareDirector(Seq("Tobe Hooper"), Seq("David Blue Garcia")) shouldBe Some(false)
    Correction.shareDirector(Nil, Seq("David Blue Garcia")) shouldBe None
  }
}
