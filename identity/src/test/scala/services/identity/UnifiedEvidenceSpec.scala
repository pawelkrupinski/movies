package services.identity

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{FamilyPick, FamilyTook, FamilyVerdict, SourceRecord, VoterFamily}

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
  private def took(family: VoterFamily, record: SourceRecord) = FamilyTook(FamilyPick(family, "x", record))

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

  it should "join to one film no two records its facts hold apart, whatever an undated record of the title links them by" in {
    // PL "Homo sapiens?" (Cohn and Duprat's 2025 "Homo Argentum"): TMDB's undated, uncredited Swedish "Homo Sapiens" shares
    // the title with both Geyrhalter's 2016 film, which IMDb, Metacritic and RT take, and Filmweb's 2025 take — one
    // contender of the three read Geyrhalter's takers as a film released last year (families.current), and filled it
    val listings = Seq(listing(KinoMuza, "Homo sapiens?"))
    val undated  = IdentityMeasures.Film("Homo Sapiens", Some("Homo Sapiens"), directors = Some(Nil))
    val geyrhalter = film("Homo Sapiens", 2016, "Nikolaus Geyrhalter", 94)
    val verdicts = Seq(took(VoterFamily.Imdb, SourceRecord(geyrhalter, Map("imdb" -> "tt5450084"))),
      took(VoterFamily.Metacritic, SourceRecord(geyrhalter, Map("metacritic" -> "homo-sapiens"))),
      took(VoterFamily.RottenTomatoes, SourceRecord(geyrhalter, Map("rt" -> "homo_sapiens_2016"))),
      took(VoterFamily.Filmweb, SourceRecord(film("Homo sapiens?", 2025, "Mariano Cohn", 98), Map("filmweb" -> "10110656"))))
    val decision = ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()
    val found = contenders(ClusterEvidence(listings, decision, Seq(IdentityResolver.NodeEvidence(listings.map(_.key), Seq(candidate(1132522, undated, 0.02)))),
      verdicts, Nil, _ => None, thisYear = 2026))
    found.filter(_.signals.contains("families.current")).map(_.film) shouldBe empty
    found.filter(c => c.signals.contains("family.imdb.took") && c.signals.contains("family.filmweb.took")).map(_.film) shouldBe empty
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

  "the model's lean a family leans to too" should "fill a current release, never an older namesake a one-off event fits" in {
    // PL Kino Kosmos "Człowiek znikąd | uroczysty pokaz odrestaurowanego filmu" (Kłyk's restored Silesian western, in
    // no film database): the model leans to the 2010 Korean "The Man from Nowhere", Polish "Człowiek znikąd", Filmweb
    // leans to it too and IMDb takes it — two weak signals a pinned fill took it on, as it takes Kawalski's 2026 "Lalka"
    // (PL Kino CK Lublin) on the same two
    val listings = Seq(listing(KinoMuza, "Człowiek znikąd | uroczysty pokaz odrestaurowanego filmu"))
    def filled(year: Int) = {
      val korean  = IdentityMeasures.Film("Człowiek Znikąd", Some("아저씨"), Nil, Some(year), Some(119), Some(Seq("이정범")), None, None, imdbNumber = 1527788)
      val record  = SourceRecord(korean.copy(title = "The Man from Nowhere", originalTitle = Some("Ajeossi"), directors = Some(Seq("Lee Jeong-beom")),
        imdbNumber = 0), Map("imdb" -> "tt1527788"))
      val verdicts = Seq(took(VoterFamily.Imdb, record), FamilyVerdict(VoterFamily.Filmweb, None, Seq(record), leaning = Some(record)),
        took(VoterFamily.Wiki, SourceRecord(film("Człowiek znikąd", 1961, "Eldar Ryazanov", 81), Map("imdb" -> "tt0054739", "wikidata" -> "Q4508581"))))
      val lean     = Some(ResolverDecision.Leaning(51608, 1527788))
      val decision = ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil, leaning = lean, candidate = lean)()
      UnifiedRules.resolver.filled(contenders(ClusterEvidence(listings, decision,
        Seq(IdentityResolver.NodeEvidence(listings.map(_.key), Seq(candidate(51608, korean, 0.08)))), verdicts, Nil, _ => None, thisYear = 2026)))
        .map { case (contender, rule) => contender.film -> rule }
    }
    filled(2010) shouldBe None
    filled(2025) shouldBe Some("tmdb:51608" -> "model.lean&count.leaning&title.namesIt&film.current")
  }

  /** The film a lone candidate's fill takes and the rule it takes it by, if any: the cluster's listings, the model's one
   *  candidate (its lean where `lean`), the families' verdicts. */
  private def filledBy(listings: Seq[Listing], tmdb: Int, f: IdentityMeasures.Film, verdicts: Seq[FamilyVerdict],
                       lean: Boolean = false): Option[(String, String)] = {
    val leaning  = Option.when(lean)(ResolverDecision.Leaning(tmdb, f.imdbNumber))
    val decision = ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil, leaning = leaning, candidate = leaning)()
    UnifiedRules.resolver.filled(contenders(ClusterEvidence(listings, decision,
      Seq(IdentityResolver.NodeEvidence(listings.map(_.key), Seq(candidate(tmdb, f, 0.03)))), verdicts, Nil, _ => None, thisYear = 2026)))
      .map { case (contender, rule) => contender.film -> rule }
  }
  private def recordOf(f: IdentityMeasures.Film) = SourceRecord(f.copy(imdbNumber = 0), Map("imdb" -> f"tt${f.imdbNumber}%07d"))
  private def linked(title: String, cineamo: String) = listing(KinoMuza, title).copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Cineamo, cineamo)))

  "one film database's take of a bare title" should "fill no namesake on its own, the model scoring it" in {
    // DE Cinema & Kurbelkiste's "Sparrows", Zeughauskino's "DIE INSEL DER DÄMONEN" (the 1934 film), Kino im DFF's "CLOSE
    // YOUR EYES": Filmweb takes a namesake, the model scores it, nothing else speaks for it — a single family's take adds
    // wrong films as often as right ones (§20.6; 13 wrong of the 26 Filmweb fills on the capture of 2026-10-13)
    val sparrows = film("Sparrows", 1926, "William Beaudine", 84, imdb = 17423)
    filledBy(Seq(listing(KinoMuza, "Sparrows")), 73575, sparrows, Seq(took(VoterFamily.Filmweb, recordOf(sparrows)))) shouldBe None
  }

  "a film database's take the venue's feed links" should "fill the film, two sources naming it, unless contested by an older namesake" in {
    // DE 3001 Kino's "Amadeo": Cineamo links 648574m and Filmweb takes the 2023 film — two independent sources
    val amadeo = film("Amadeo", 2023, "Vicente Amorim", 105, imdb = 11143850)
    filledBy(Seq(linked("Amadeo", "648574m")), 648574, amadeo, Seq(took(VoterFamily.Filmweb, recordOf(amadeo)))) shouldBe
      Some("tmdb:648574" -> "family.feedLink&!family.dissent")
    // the feed linking another film: Filmweb's take alone
    filledBy(Seq(linked("Amadeo", "1m")), 648574, amadeo, Seq(took(VoterFamily.Filmweb, recordOf(amadeo)))) shouldBe None
    // DE Schloß-Theater's "Ladies Night", a women's evening: Cineamo links Norma Bailey's 2005 TV film and Wikidata takes
    // it, IMDb a 9-minute namesake the title names as well — contested, and no current release
    val ladies = film("Ladies Night", 2005, "Norma Bailey", 85, imdb = 422613)
    val short  = film("Ladies Night", 2016, "Kate Sullivan", 9, imdb = 5365772)
    filledBy(Seq(linked("Ladies Night", "223746m")), 223746, ladies,
      Seq(took(VoterFamily.Wiki, recordOf(ladies)), took(VoterFamily.Imdb, recordOf(short)))) shouldBe None
    // contested, but a current release (DE "The Arab", Cineamo's 1001315m): the feed and a database's take stand
    val arab = film("The Arab", 2026, "Malek Bensmaïl", 90, imdb = 39230003)
    filledBy(Seq(linked("The Arab", "1001315m")), 1001315, arab,
      Seq(took(VoterFamily.Filmweb, recordOf(arab)), took(VoterFamily.Imdb, recordOf(film("The Arab", 1924, "Rex Ingram", 70, imdb = 14674))))) shouldBe
      Some("tmdb:1001315" -> "family.feedLink&film.current")
  }

  it should "fill nothing where the year the feed copied denied the title's first hit" in {
    // DE Astor's "ATMOS: Michael" (capture 2026-10-13): Cineamo links Dreyer's 1924 "Michael" (55587m) and copies its year,
    // which denied TMDB's first hit, the 2026 biopic; Filmweb takes the 1924 film by that year — the feed's claim, restated
    val dreyer = film("Michael", 1924, "Carl Theodor Dreyer", 93, imdb = 15136)
    val biopic = film("Michael", 2026, "Antoine Fuqua", 127, imdb = 11378946)
    val listings = Seq(listing(KinoMuza, "ATMOS: Michael", year = Some(1924)).copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Cineamo, "55587m"))))
    def fill(firstDeniedBy: Map[String, IdentityMeasures.Measure]) = {
      val decision = ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.Vetoed, Nil)()
      val first    = candidate(936075, biopic, 0.06, denied = true).copy(measures = firstDeniedBy)
      val linkedTo = candidate(55587, dreyer, 0.06).copy(rank = Some(10))
      UnifiedRules.resolver.filled(contenders(ClusterEvidence(listings, decision,
        Seq(IdentityResolver.NodeEvidence(listings.map(_.key), Seq(first, linkedTo))), Seq(took(VoterFamily.Filmweb, recordOf(dreyer))), Nil,
        _ => None, thisYear = 2026))).map { case (contender, rule) => contender.film -> rule }
    }
    fill(Map("year.delta" -> IdentityMeasures.Number(-102))) shouldBe None
    // the first hit denied by no year: the link and the family's take stand as two sources
    fill(Map.empty) shouldBe Some("tmdb:55587" -> "family.feedLink&!family.dissent")
    // the feed linking the CURRENT film over an older first hit its copied year denied (DE Gröben-Lichtspiele's "Camouflage
    // OmU": Cineamo and Filmweb on the 2026 Ukrainian film, the 2016 namesake ranked first): the link and the take stand
    val ukrainian = film("Camouflage", 2026, "Lyubov Durakova", 90, imdb = 30000001)
    val older     = film("Camouflage", 2016, "Imge Özbilge", 80, imdb = 6000001)
    val camouflage = Seq(listing(KinoMuza, "Camouflage OmU", year = Some(2026)).copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Cineamo, "1735950m"))))
    UnifiedRules.resolver.filled(contenders(ClusterEvidence(camouflage, ResolverDecision(camouflage.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)(),
      Seq(IdentityResolver.NodeEvidence(camouflage.map(_.key), Seq(candidate(1177883, older, 0.05, denied = true).copy(measures = Map("year.delta" -> IdentityMeasures.Number(10))),
        candidate(1735950, ukrainian, 0.6).copy(rank = Some(2))))), Seq(took(VoterFamily.Filmweb, recordOf(ukrainian))), Nil, _ => None, thisYear = 2026)))
      .map { case (contender, rule) => contender.film -> rule } shouldBe Some("tmdb:1735950" -> "family.feedLink&!family.dissent")
  }

  "a film database's take of the namesake the release veto kept" should "fill it, unless the veto took out the search's first hit" in {
    // PL Kino Kultura Gołdap's "NADZIEJA" (capture 2026-10-13): Filmweb takes Mucha's 2007 documentary (277730), which TMDB
    // dates a Polish release of; the veto took out the 2019 namesake (616561, 15th in the search) for having none
    val nadzieja = film("Nadzieja", 2007, "Stanisław Mucha", 87, imdb = 847750)
    val later    = film("Nadzieja", 2019, "Maria Sødahl", 126, imdb = 8228538)
    def fill(vetoedRank: Int) = {
      val listings = Seq(listing(KinoMuza, "NADZIEJA"))
      val decision = ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()
      val kept     = candidate(277730, nadzieja, 0.03).copy(rank = Some(2))
      val vetoed   = candidate(616561, later, 0.03, denied = true).copy(rank = Some(vetoedRank), releasedBeside = Some(277730))
      UnifiedRules.resolver.filled(contenders(ClusterEvidence(listings, decision,
        Seq(IdentityResolver.NodeEvidence(listings.map(_.key), Seq(kept, vetoed))), Seq(took(VoterFamily.Filmweb, recordOf(nadzieja))), Nil,
        _ => None, thisYear = 2026))).map { case (contender, rule) => contender.film -> rule }
    }
    fill(vetoedRank = 15) shouldBe Some("tmdb:277730" -> "family.releaseKept&!model.vetoedAbove")
    // PL Kino za Rogiem Cafe's "Stan wyjątkowy": the veto took out TMDB's FIRST hit (Hřebejk's 2024 film, the venue's) and
    // Filmweb takes Brešan's 2018 namesake beside it — the take is the veto's own reading, nothing besides
    fill(vetoedRank = 1) shouldBe None
    // no namesake vetoed: Filmweb's take alone
    filledBy(Seq(listing(KinoMuza, "NADZIEJA")), 277730, nadzieja, Seq(took(VoterFamily.Filmweb, recordOf(nadzieja)))) shouldBe None
  }

  "a film database's take the venue's director credit names" should "fill the film, though the venue states no year" in {
    // UK Picture House Uckfield's "The Misanthrope (NT Live)", crediting Indhu Rubasingham: IMDb takes the 2026 production
    val misanthrope = film("The Misanthrope", 2026, "Indhu Rubasingham", 165, imdb = 39000001)
    val credited    = Seq(listing(KinoMuza, "The Misanthrope (NT Live)", director = Some("Indhu Rubasingham")))
    filledBy(credited, 1693710, misanthrope, Seq(took(VoterFamily.Imdb, recordOf(misanthrope)))) shouldBe Some("tmdb:1693710" -> "family.director")
    // another director credited: no fill
    filledBy(Seq(listing(KinoMuza, "The Misanthrope (NT Live)", director = Some("Simon Godwin"))), 1693710, misanthrope,
      Seq(took(VoterFamily.Imdb, recordOf(misanthrope)))) shouldBe None
  }

  "a family's take of the model's lean" should "fill a current release, or one two families take, never an older namesake one takes" in {
    // UK Act One Cinema's "THE COMIC STRIP", a clips-and-Q&A evening: Wikidata takes the 1981 TV film the model leans to
    val comic = film("The Comic Strip", 1981, "Bob Spiers", 30, imdb = 81876)
    filledBy(Seq(listing(KinoMuza, "THE COMIC STRIP")), 396598, comic, Seq(took(VoterFamily.Wiki, recordOf(comic))), lean = true) shouldBe None
    // UK Old Market Hall's "The Man with the Plan (15)": the 2026 film Wikidata takes and the model leans to
    val plan = film("The Man with the Plan", 2026, "Jenny Popplewell", 90, imdb = 30000002)
    filledBy(Seq(listing(KinoMuza, "The Man with the Plan (15)")), 1500000, plan, Seq(took(VoterFamily.Wiki, recordOf(plan))), lean = true) shouldBe
      Some("tmdb:1500000" -> "and.leanTakers&!family.dissent&film.current")
    // UK Mockingbird's "Love Actually With Wine": Metacritic and RT take Curtis's 2003 film, the model leans to it
    val love = film("Love Actually", 2003, "Richard Curtis", 135, imdb = 314331)
    filledBy(Seq(listing(KinoMuza, "Love Actually With Wine")), 508, love,
      Seq(took(VoterFamily.Metacritic, recordOf(love)), took(VoterFamily.RottenTomatoes, recordOf(love))), lean = true) shouldBe
      Some("tmdb:508" -> "and.leanTakers&!family.dissent&count.takers2")
  }

  "a billing under two names" should "fill no film one of them names that another candidate is named by both" in {
    // DE Acud Kino's "BARWY OCHRONNE / CAMOUFLAGE", Zanussi's 1977 film ("Camouflage" in English), joined to venues'
    // bare "CAMOUFLAGE": Gibalenko's 2026 "Камуфляж", billed widely, is named only by its English title there
    val billed = Seq("Acud Kino" -> "BARWY OCHRONNE / CAMOUFLAGE", "Harsefelder Lichtspiele" -> "CAMOUFLAGE", "IL KINO" -> "Camouflage",
      "Kino A" -> "Camouflage", "Kino B" -> "Camouflage").map { case (venue, title) =>
      listing(KinoMuza, title).copy(key = services.movies.ListingKey.Published(venue, title, None, Nil)) }
    val gibalenko = IdentityMeasures.Film("Камуфляж", Some("Камуфляж"), Seq("Camouflage"), Some(2026), Some(102), Some(Seq("Mykyta Gibalenko")))
    val zanussi   = IdentityMeasures.Film("Barwy ochronne", Some("Barwy ochronne"), Seq("Camouflage"), Some(1977), Some(96), Some(Seq("Krzysztof Zanussi")))
    def found(listings: Seq[Listing]) = {
      val decision = ResolverDecision(listings.map(_.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()
      contenders(ClusterEvidence(listings, decision, Seq(IdentityResolver.NodeEvidence(listings.map(_.key),
        Seq(candidate(1735950, gibalenko, 0.08), candidate(139467, zanussi, 0.03, denied = true)))), Nil, Nil, _ => None, thisYear = 2026))
    }
    found(billed).find(_.film == "tmdb:1735950").map(_.signals.contains("title.anothersOwn")) shouldBe Some(true)
    UnifiedRules.resolver.filled(found(billed)) shouldBe None
    // the bare titles alone: the 2026 film, as before
    UnifiedRules.resolver.filled(found(billed.tail)).map { case (c, rule) => c.film -> rule } shouldBe Some("tmdb:1735950" -> "venues.current")
  }

  "a current release billed widely" should "fill no film the title names only within another candidate's title, articles aside" in {
    // DE CineStar ×43 "Herr der Ringe - Rückkehr des Königs (ext.)" (recording 38355584482), the 2003 film's re-release:
    // a 14-minute 2025 short titled "Herr der Ringe" is the one current film the billing names, and venues.current
    // filled it — the billing names it only as the head of "Der Herr der Ringe - Die Rückkehr des Königs", the
    // venue dropping the articles
    val title    = "Herr der Ringe - Rückkehr des Königs (ext.)"
    val listings = Seq("CineStar Augsburg", "CineStar Berlin", "CineStar Kiel", "CineStar Erfurt").map(venue =>
      listing(KinoMuza, title).copy(key = services.movies.ListingKey.Published(venue, title, None, Nil)))
    val short    = IdentityMeasures.Film("Herr der Ringe", Some("Herr der Ringe"), Nil, Some(2025), Some(14))
    val king     = IdentityMeasures.Film("Der Herr der Ringe - Die Rückkehr des Königs", Some("The Lord of the Rings: The Return of the King"), Nil,
      Some(2003), Some(201), Some(Seq("Peter Jackson")), imdbNumber = 167260)
    val decision = ResolverDecision(listings.map(_.key), None, 0.17, ResolverDecision.Basis.BelowThreshold, Nil)()
    def found(ranked: IdentityResolver.CandidateEvidence*) =
      contenders(ClusterEvidence(listings, decision, Seq(IdentityResolver.NodeEvidence(listings.map(_.key), ranked)), Nil, Nil, _ => None, thisYear = 2026))
    UnifiedRules.resolver.filled(found(candidate(1434460, short, 0.03), candidate(122, king, 0.02))) shouldBe None
    UnifiedRules.resolver.filled(found(candidate(1434460, short, 0.03), candidate(122, king, 0.02, denied = true))) shouldBe None
    found(candidate(1434460, short, 0.03), candidate(122, king, 0.02)).find(_.film == "tmdb:1434460").map(_.signals.contains("title.anothersOwn")) shouldBe
      Some(true)
    // the short alone, no other candidate naming more of the billing: no title.anothersOwn, but the billing names it as one
    // piece beside "Rückkehr des Königs", which no venue states a fact of the short's around ([[BilledPiece.ofFamilies]])
    val alone = found(candidate(1434460, short, 0.03))
    alone.find(_.film == "tmdb:1434460").map(c => (c.signals.contains("title.anothersOwn"), c.signals.contains("bill.piece"))) shouldBe Some((false, true))
    UnifiedRules.resolver.filled(alone) shouldBe None
  }

  "a pinned rule" should "fire on a conjunction of signals and absences" in {
    val signals = Map("family.filmweb.took" -> 1.0, "model.logit" -> -3.5)
    UnifiedRules.fires("family.filmweb.took&!model.unscored", signals.getOrElse(_, 0.0)) shouldBe true
    UnifiedRules.fires("family.filmweb.took&!model.unscored", (signals + ("model.unscored" -> 1.0)).getOrElse(_, 0.0)) shouldBe false
    UnifiedRules.fires("family.imdb.took", signals.getOrElse(_, 0.0)) shouldBe false
    UnifiedRules.resolver.pinned should contain ("family.feedLink&!family.dissent")
  }

  "the signal list" should "name every signal once, each with a direction" in {
    Names.distinct.size shouldBe Names.size
    RuleGroups.values.toSet.subsetOf(Names.toSet) shouldBe true
  }
}
