package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, IdentityMeasures, Resolution, ResolverDecision}
import services.movies.SingleCountryNormalizer

/** The model's no-matches, on their way to the projection: one ≥3 families agree on takes that film — TMDB's when its
 *  IMDb id finds one there — and one a family has not answered for yet stays as the model left it, its question named. */
class AgreementStageSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.listing

  private val klondike = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), Some(100), Some(Seq("Maryna Er Gorbach")), None, None)
  private val bare     = listing(KinoMuza, "Klondike", year = Some(2022))
  private val matched  = listing(KinoMuza, "Dune", year = Some(2021))
  private def agreeing(rtAnswered: Boolean = true) = Map(
    VoterFamily.Imdb           -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike, Map("imdb" -> "tt16315948")))),
    VoterFamily.Wiki           -> new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q1" -> SourceRecord(klondike, Map("imdb" -> "tt16315948")))),
    VoterFamily.Filmweb        -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike))),
    VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike)), unanswered = !rtAnswered))

  private def resolution = Resolution(Seq(
    ResolverDecision(Seq(bare.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)(),
    ResolverDecision(Seq(matched.key), Some(438631), 0.99, ResolverDecision.Basis.OwnMatch, Nil)()),
    2, Map(bare.key -> 0, matched.key -> 1), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  private def listingOf = Map(bare.key -> bare, matched.key -> matched).get

  "an unmatched cluster its families agree on" should "take the TMDB film the agreed IMDb id finds, and name the families" in {
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = imdb => Answer.Known(Option.when(imdb == "tt16315948")(913760)), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    val decided = stage.apply(resolution, listingOf, version = 1)
    val klondikeDecision = decided.decisions.head
    (klondikeDecision.film, klondikeDecision.basis) shouldBe ((Some(913760), ResolverDecision.Basis.Agreed))
    klondikeDecision.explanation.last should include ("filmweb, imdb, rt, wiki agree on 'Klondike' (2022) tt16315948")
    decided.decisions(1) shouldBe resolution.decisions(1)
  }

  it should "count the model's own best TMDB candidate as a voter beside two families, by its record's facts" in {
    // IMDb and Wikidata take "Klondike"; the model weighed TMDB's 913760 best, below every rule's cut (no lean, no IMDb id)
    val two = agreeing().view.filterKeys(Set(VoterFamily.Imdb, VoterFamily.Wiki)).toMap ++ Map(
      VoterFamily.Filmweb -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map.empty), VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map.empty))
    val tmdb = new services.identity.IdentityLookups {
      def hasDetail(listing: services.identity.Listing): Boolean = false
      def detail(listing: services.identity.Listing) = Answer.Known(None)
      def candidates(query: services.identity.CandidateQuery) = Answer.Known(Nil)
      def film(tmdbId: Int) = Answer.Known(Option.when(tmdbId == 913760)(klondike))
    }
    def decided(candidate: Option[ResolverDecision.Leaning]) = {
      val model = resolution.copy(decisions = resolution.decisions.updated(0,
        ResolverDecision(Seq(bare.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil, candidate = candidate)()))
      new AgreementStage(two, tmdb, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), new InMemoryAgreementVerdicts,
        clock = _root_.tools.SpecClock.Pinned).apply(model, listingOf, version = 1).decisions.head
    }
    decided(None).film shouldBe None
    val taken = decided(Some(ResolverDecision.Leaning(913760, 0)))
    (taken.film, taken.basis) shouldBe ((Some(913760), ResolverDecision.Basis.Agreed))
    taken.explanation.last should include ("corroborated by tmdb")
  }

  it should "hand back the same decision object while its verdict stands, so the projection redrafts it only when it moves" in {
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    val model = resolution
    val first = stage.apply(model, listingOf, version = 1).decisions.head
    stage.apply(model, listingOf, version = 2).decisions.head should be theSameInstanceAs first
  }

  it should "come to the same decisions without reading an answer, handed the same decisions while nothing was filed" in {
    var reads = 0
    val counting = agreeing().map { case (family, answers) => family -> new FamilyAnswers {
      val family: VoterFamily = answers.family
      def titled(text: String)     = { reads += 1; answers.titled(text) }
      def directedBy(name: String) = { reads += 1; answers.directedBy(name) }
      def record(id: String)       = { reads += 1; answers.record(id) }
    } }
    val stage = new AgreementStage(counting, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    val model = resolution
    val first = stage.apply(model, listingOf, version = 1)
    val readFirst = reads
    val again = stage.apply(model.copy(nodes = model.nodes + 1), listingOf, version = 1)   // a new resolution, the same decisions
    reads shouldBe readFirst
    again.decisions.head should be theSameInstanceAs first.decisions.head
  }

  it should "report each pass: clusters waiting and agreed, films taken, questions open, clusters resolved" in {
    val passes = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Applied]
    val stage  = new AgreementStage(agreeing(rtAnswered = false), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(Some(913760)), new InMemoryAgreementVerdicts, metrics = passes += _, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1)
    val waitingPass = passes.last
    (waitingPass.waiting, waitingPass.agreed, waitingPass.takenTmdb, waitingPass.resolves) shouldBe ((1, 0, 0, 1))
    waitingPass.open.get(VoterFamily.RottenTomatoes) shouldBe Some(1)
    val agreeingStage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(Some(913760)), new InMemoryAgreementVerdicts, metrics = passes += _, clock = _root_.tools.SpecClock.Pinned)
    agreeingStage.apply(resolution, listingOf, version = 1)
    val agreedPass = passes.last
    (agreedPass.waiting, agreedPass.verdicts, agreedPass.agreed, agreedPass.takenTmdb, agreedPass.takenFallback) shouldBe ((0, 1, 1, 1, 0))
  }

  it should "take the agreed IMDb id as its fallback film when TMDB holds none" in {
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head.fallback shouldBe Some(ResolverDecision.Fallback("imdb", "tt16315948", 1.0))
  }

  it should "keep the families' own ids with the decision, and its verdict across a restart without asking them again" in {
    val verdicts = new InMemoryAgreementVerdicts
    val first = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts, clock = _root_.tools.SpecClock.Pinned)
    first.apply(resolution, listingOf, version = 1).decisions.head.agreed shouldBe
      Map("imdb" -> "tt16315948", "wiki" -> "Q1", "filmweb" -> "880000", "rt" -> "klondike_2022")
    verdicts.all().map(_.agreed.map(_.families)) shouldBe Seq(Some(Set(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes)))
    var asked = 0
    val counting = agreeing().map { case (family, answers) => family -> new FamilyAnswers {
      val family: VoterFamily = answers.family
      def titled(text: String)     = { asked += 1; answers.titled(text) }
      def directedBy(name: String) = { asked += 1; answers.directedBy(name) }
      def record(id: String)       = { asked += 1; answers.record(id) }
    } }
    val restarted = new AgreementStage(counting, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts, clock = _root_.tools.SpecClock.Pinned)
    restarted.apply(resolution, listingOf, version = 0).decisions.head.film shouldBe Some(913760)
    asked shouldBe 0   // its families' answers cannot have moved while it was down: no answer read again, no resolve
  }

  it should "decide again a verdict stored under other rules" in {
    val verdicts = new InMemoryAgreementVerdicts
    new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned).apply(resolution, listingOf, version = 1)
    verdicts.replace(Set.empty, verdicts.all().map(_.copy(rules = "an older build")))
    var asked = 0
    val counting = agreeing().map { case (family, answers) => family -> new FamilyAnswers {
      val family: VoterFamily = answers.family
      def titled(text: String)     = { asked += 1; answers.titled(text) }
      def directedBy(name: String) = { asked += 1; answers.directedBy(name) }
      def record(id: String)       = { asked += 1; answers.record(id) }
    } }
    new AgreementStage(counting, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned).apply(resolution, listingOf, version = 0)
    asked should be > 0
    verdicts.all().map(_.rules) shouldBe Seq(AgreementStage.RulesVersion)
  }

  it should "decide again when an answer it read moved, and drop the verdict of a cluster no longer unmatched" in {
    val verdicts = new InMemoryAgreementVerdicts
    new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts, clock = _root_.tools.SpecClock.Pinned)
      .apply(resolution, listingOf, version = 1)
    val dissent = agreeing() ++ Seq(VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    // Filmweb's and RT's answers refiled after the restart (version 1 → 2): only a verdict that read them is decided again.
    val refiled = new AnswerChanges {
      def changedSince(version: Long): Option[Set[String]] =
        Some(if (version < 2) verdicts.all().head.reads.keySet.filter(q => q.startsWith("filmweb|") || q.startsWith("rt|")) else Set.empty)
    }
    val again = new AgreementStage(dissent, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned, changes = refiled)
    again.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)   // the restart trusts what it kept
    again.apply(resolution, listingOf, version = 2).decisions.head shouldBe resolution.decisions.head
    verdicts.all().map(_.agreed) shouldBe Seq(None)
    val matchedNow = resolution.copy(decisions = resolution.decisions.map(_.copy(film = Some(1))(services.identity.DecisionTrace.Empty)))
    again.apply(matchedNow, listingOf, version = 2)
    verdicts.all() shouldBe empty
  }

  it should "ask TMDB about the agreed IMDb id before taking it as a fallback" in {
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Unknown, new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head shouldBe resolution.decisions.head
    stage.wantedFinds shouldBe Set("tt16315948")
  }

  it should "hand the questions it meets unanswered to the queue at once, and a stale answer's too while it uses it" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    new AgreementStage(agreeing(rtAnswered = false), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = asked += _, clock = _root_.tools.SpecClock.Pinned).apply(resolution, listingOf, version = 1)
    asked.flatMap(_.questions) should contain (VoterFamily.RottenTomatoes -> "title|Klondike")
    val staleRt = agreeing() + (VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes,
      Map("klondike_2022" -> SourceRecord(klondike)), stale = true))
    val refreshed = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    val stage = new AgreementStage(staleRt, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, ask = refreshed += _, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)   // the stale answers still count
    refreshed.flatMap(_.questions).filter(_._1 == VoterFamily.RottenTomatoes) should contain (VoterFamily.RottenTomatoes -> "title|Klondike")
    refreshed.clear()
    stage.apply(resolution, listingOf, version = 1)                                // a quiet tick reads no answer again…
    refreshed shouldBe empty
    stage.apply(resolution, listingOf, version = 2)                                // …one after a filing re-reads the kept verdict's
    refreshed.flatMap(_.questions).filter(_._1 == VoterFamily.RottenTomatoes) should not be empty
  }

  it should "resolve a waiting cluster again only once all its questions are answered, or some after it waited long" in {
    var resolves = 0
    var rtAnswered, filmwebAnswered = false
    def flipping(of: VoterFamily, records: Map[String, SourceRecord], isAnswered: => Boolean): FamilyAnswers = new FamilyAnswers {
      private val family0 = of
      val family: VoterFamily = of
      private def held = new HeldFamilyAnswers(family0, records, unanswered = !isAnswered)
      def titled(text: String)     = { if (family0 == VoterFamily.Imdb) resolves += 1; held.titled(text) }
      def directedBy(name: String) = held.directedBy(name)
      def record(id: String)       = held.record(id)
      override def fresh(question: String) = held.fresh(question)
    }
    val families = Map(
      VoterFamily.Imdb           -> flipping(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike, Map("imdb" -> "tt16315948"))), true),
      VoterFamily.Wiki           -> flipping(VoterFamily.Wiki, Map("Q1" -> SourceRecord(klondike, Map("imdb" -> "tt16315948"))), true),
      VoterFamily.Filmweb        -> flipping(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike)), filmwebAnswered),
      VoterFamily.RottenTomatoes -> flipping(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike)), rtAnswered))
    val clock = new tools.MutableClock(java.time.Instant.parse("2026-10-04T21:00:00Z"))
    val handed = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    val stage = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, ask = handed += _, clock = clock)
    val model = resolution
    stage.apply(model, listingOf, version = 1).decisions.head.film shouldBe None
    val (afterFirst, handedFirst) = (resolves, handed.size)
    stage.apply(model, listingOf, version = 1)
    (resolves, handed.size) shouldBe ((afterFirst, handedFirst))   // a quiet tick: no resolve, nothing handed
    rtAnswered = true
    stage.apply(model, listingOf, version = 2)                      // one of its questions answered, Filmweb's still open: no resolve
    resolves shouldBe afterFirst
    clock.advanceSeconds(AgreementStage.PartialAfter.toSeconds)
    stage.apply(model, listingOf, version = 3)                      // waited long with some answered: resolved on what came
    resolves should be > afterFirst
    val afterPartial = resolves
    filmwebAnswered = true
    stage.apply(model, listingOf, version = 4).decisions.head.film shouldBe Some(913760)   // all answered: resolved, taken
    resolves should be > afterPartial
  }

  it should "stay as the model left it while a family has not answered, and name the question" in {
    val stage = new AgreementStage(agreeing(rtAnswered = false), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head shouldBe resolution.decisions.head
    stage.wanted should contain (VoterFamily.RottenTomatoes -> "title|Klondike")
  }
}
