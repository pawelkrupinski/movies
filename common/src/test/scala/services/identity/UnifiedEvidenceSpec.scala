package services.identity

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{FamilyPick, FamilyVerdict, SourceRecord, VoterFamily}

/** Every signal of the model and the agreement stage, read off one cluster as the unified model's features. */
class UnifiedEvidenceSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing
  import UnifiedEvidence._

  private def film(title: String, year: Int, director: String, runtime: Int = 100, imdb: Int = 0) =
    IdentityMeasures.Film(title, None, Nil, Some(year), Some(runtime), Some(Seq(director)), None, None, imdbNumber = imdb)
  private val klondike2022 = film("Klondike", 2022, "Maryna Er Gorbach", 100, imdb = 16315948)
  private val klondike1932 = film("Klondike", 1932, "Phil Rosen", 68, imdb = 22873)

  private val bare = Seq(listing(KinoMuza, "Klondike", year = Some(2022), director = Some("Maryna Er Gorbach")))
  private def candidate(id: Int, f: IdentityMeasures.Film, p: Double, denied: Boolean = false) =
    IdentityResolver.CandidateEvidence(id, f, p, Some(1), denied, titleNamesIt = true, seasonProduction = false, houseProduction = false)
  private def node(candidates: IdentityResolver.CandidateEvidence*) = IdentityResolver.NodeEvidence(bare.map(_.key), candidates)
  private val noMatch = ResolverDecision(bare.map(_.key), None, 0.4, ResolverDecision.Basis.BelowThreshold, Nil,
    leaning = Some(ResolverDecision.Leaning(913760, 16315948)), candidate = Some(ResolverDecision.Leaning(913760, 16315948)))()
  private def evidence(nodes: Seq[IdentityResolver.NodeEvidence], verdicts: Seq[FamilyVerdict] = Nil, posters: Seq[Map[Int, Option[Int]]] = Nil,
                       decision: ResolverDecision = noMatch) =
    ClusterEvidence(bare, decision, nodes, verdicts, posters, _ => None, thisYear = 2026)
  private def took(family: VoterFamily, record: SourceRecord) = FamilyVerdict.took(FamilyPick(family, "x", record))

  "a cluster's contenders" should "be the TMDB candidates some node did not deny, each with the model's signals" in {
    val found = contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30), candidate(52000, klondike1932, 0.02, denied = true)))))
    found.map(_.film) shouldBe Seq("tmdb:913760")
    found.head.signals("model.logit") shouldBe logit(0.30)
    found.head.signals.keySet should contain allOf ("model.lean", "model.best", "listing.facts", "title.namesIt")
    found.head.signals.get("model.deniedBySome") shouldBe None
  }

  it should "credit a family's take to the TMDB candidate it names by IMDb id, and turn a film only it names into its own contender" in {
    val imdbTakes = took(VoterFamily.Imdb, SourceRecord(klondike2022.copy(imdbNumber = 0), Map("imdb" -> "tt16315948")))
    val wikiTakes = took(VoterFamily.Wiki, SourceRecord(film("Klondike", 1932, "Phil Rosen"), Map("imdb" -> "tt0022873")))
    val found = contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30))), Seq(imdbTakes, wikiTakes))).map(c => c.film -> c.signals).toMap
    found.keySet shouldBe Set("tmdb:913760", "imdb:tt0022873")
    found("tmdb:913760").get("family.imdb.took") shouldBe Some(1.0)
    found("tmdb:913760").get("family.dissent") shouldBe Some(1.0)
    found("imdb:tt0022873").get("family.wiki.took") shouldBe Some(1.0)
    found("imdb:tt0022873").get("model.unscored") shouldBe Some(1.0)
    found("imdb:tt0022873").get("listing.contradicts") shouldBe Some(1.0)
  }

  it should "join a film IMDb names by its id alone to the TMDB candidate another family's record names by TMDB id" in {
    // PL "Ghost in the shell": IMDb's record links no TMDB id, Wikidata's links both — one film, TMDB's candidate
    val gits     = film("Ghost in the Shell", 1995, "Mamoru Oshii", 83)
    val imdbOnly = took(VoterFamily.Imdb, SourceRecord(gits.copy(directors = Some(Seq("Oshii Mamoru"))), Map("imdb" -> "tt0113568")))
    val wiki     = took(VoterFamily.Wiki, SourceRecord(gits.copy(title = "Kōkaku Kidōtai", directors = None), Map("imdb" -> "tt0113568", "tmdb" -> "9323")))
    val found = contenders(evidence(Seq(node(candidate(9323, gits.copy(title = "Ghost in the Shell (1995)"), 0.2))), Seq(imdbOnly, wiki)))
    found.map(_.film) shouldBe Seq("tmdb:9323")
    found.head.familyIds shouldBe Map("imdb" -> "x", "wiki" -> "x")
  }

  it should "carry the agreement's own verdict as a signal" in {
    val record = SourceRecord(klondike2022.copy(imdbNumber = 0), Map("imdb" -> "tt16315948"))
    val three  = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(took(_, record))
    contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30))), three)).head.signals.get("agreement.quorum") shouldBe Some(1.0)
  }

  it should "carry no agreement verdict where only review sites take the film, as the stage then takes nothing" in {
    // DE "André Rieus Weihnachtskonzert 2026": Metacritic and RT take a namesake the model's lean completes to a quorum;
    // their pages name no film a card stands on, so the stage takes nothing
    val record  = SourceRecord(klondike2022.copy(imdbNumber = 0))
    val reviews = Seq(VoterFamily.RottenTomatoes, VoterFamily.Metacritic).map(took(_, record))
    contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30))), reviews)).head.signals.get("agreement.quorum") shouldBe None
    val withFilmweb = reviews :+ took(VoterFamily.Filmweb, record)
    contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30))), withFilmweb)).head.signals.get("agreement.quorum") shouldBe Some(1.0)
  }

  it should "read a family that weighed the film and leaned to another as turning it down" in {
    // a lean the listing's own facts rule out is no turning down (its director is not the venue's): this one credits nobody
    val old  = SourceRecord(film("Old", 2022, "M. Night Shyamalan").copy(directors = None))
    val lean = FamilyVerdict(VoterFamily.RottenTomatoes, None, weighed = Seq(SourceRecord(klondike2022), old), leaning = Some(old))
    val found = contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30))), Seq(lean))).find(_.film == "tmdb:913760").get
    found.signals.get("family.turnedDown") shouldBe Some(1.0)
  }

  it should "read the venue posters: a match within the vote's bits, and another candidate's match against a film" in {
    val posters = Seq(Map(913760 -> Some(3), 52000 -> Some(20)))
    val found = contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.30), candidate(52000, klondike1932, 0.10))), posters = posters))
      .map(c => c.film -> c.signals).toMap
    found("tmdb:913760").get("poster.match") shouldBe Some(1.0)
    found("tmdb:52000").get("poster.otherMatches") shouldBe Some(1.0)
  }

  it should "credit the rule a node took the film by, and the pooled vote when no node did" in {
    val trace = DecisionTrace(None, None, Map(bare.head.key -> DecisionTrace.Node(Some("directors-title"), Nil, Nil, candidate = Some(913760))))
    val own = ResolverDecision(bare.map(_.key), Some(913760), 0.9, ResolverDecision.Basis.OwnMatch, Nil)(trace)
    contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.9))), decision = own)).head.signals.get("rule.director") shouldBe Some(1.0)
    val pooled = ResolverDecision(bare.map(_.key), Some(913760), 0.9, ResolverDecision.Basis.PooledMatch, Nil)()
    contenders(evidence(Seq(node(candidate(913760, klondike2022, 0.9))), decision = pooled)).head.signals.get("rule.pooled") shouldBe Some(1.0)
  }

  "the unified weights" should "explain a probability as its signals' contributions" in {
    val weights = UnifiedWeights("t", Seq("model.logit", "family.imdb.took"), Seq(-1.0, 0.5, 2.0), cut = 0.9, l2 = 1.0, folds = 5, rows = 1)
    val features = Map("model.logit" -> 2.0, "family.imdb.took" -> 1.0)
    weights.logOdds(features) shouldBe 2.0
    weights.explain(features) shouldBe "family.imdb.took=1.00 +2.00 model.logit=2.00 +1.00"
  }

  "the hybrid" should "veto a contender a guard trips whatever it scores, unless the model's own rule took it, and explain both" in {
    val hybrid = UnifiedWeights("h", Seq("family.imdb.took", "rule.title"), Seq(0.0, 5.0, 5.0), cut = 0.9, l2 = 1.0, folds = 5, rows = 1,
      guards = UnifiedEvidence.Guards)
    val contradicted = Map("family.imdb.took" -> 1.0, "listing.contradicts" -> 1.0)
    hybrid.vetoes(contradicted) shouldBe Seq("listing.contradicts")
    hybrid.decision(contradicted) should startWith("vetoed by listing.contradicts; guards passed: bill.several")
    hybrid.vetoes(contradicted + ("rule.title" -> 1.0)) shouldBe empty
    hybrid.decision(Map("family.imdb.took" -> 1.0)) should startWith("taken 99.3% ≥ 90.0%; guards passed: bill.several, stage.work")
  }

  "the guards" should "veto a film the title does not name, and another edition than the title numbers" in {
    // PL "Baczne oczka" filled with "Pucio kocha zwierzaki"; PL/UK "League of Legends Worlds 26" with the Worlds25 record
    def only(title: String, record: IdentityMeasures.Film) = {
      val listings = Seq(listing(KinoMuza, title))
      contenders(ClusterEvidence(listings, ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)(),
        Seq(IdentityResolver.NodeEvidence(listings.map(_.key), Seq(candidate(1, record, 0.1)))), Nil, Nil, _ => None, 2026)).head
    }
    val pucio  = only("Baczne oczka", IdentityMeasures.Film("Pucio kocha zwierzaki", year = Some(2026)))
    vetoes(pucio.signals.getOrElse(_, 0.0)) shouldBe Seq("title.namesNone")
    val worlds = only("League of Legends Worlds 26 | Finals in Cinema", IdentityMeasures.Film("League of Legends Worlds25 - Finals in Cinema", year = Some(2025)))
    vetoes(worlds.signals.getOrElse(_, 0.0)) should contain("edition.apart")
  }

  "the signal list" should "name every signal once, each with a direction" in {
    Names.distinct.size shouldBe Names.size
    RuleGroups.values.toSet.subsetOf(Names.toSet) shouldBe true
  }
}
