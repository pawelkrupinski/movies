package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, Question, FilmTable, IdentityCalibration, IdentityMeasures, Resolution, ResolverDecision}
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

  "an unmatched cluster no families agree on" should "take, by the selected fill rule, the one guard-passing film billed widely as a current release" in {
    // US "SEVENTEEN World Tour 'NEW_'" ×391: IMDb alone holds the 2026 tour film, at venue after venue
    val tour    = IdentityMeasures.Film("Seventeen World Tour (New_)", None, Nil, Some(2026), None, None, None, None)
    val imdb    = Map(VoterFamily.Imdb -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt46658626" -> SourceRecord(tour, Map("imdb" -> "tt46658626"))))) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    def decided(title: String, venues: Seq[models.Cinema], rules: services.identity.UnifiedRules) = {
      val listings = venues.map(venue => listing(venue, title))
      val model = Resolution(Seq(ResolverDecision(listings.map(_.key), None, 0.2, ResolverDecision.Basis.BelowThreshold, Nil)()), listings.size,
        listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
      new AgreementStage(imdb, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
        clock = _root_.tools.SpecClock.Pinned, tmdb = Some(NoVenueDetails), rules = rules).apply(model, listings.map(l => l.key -> l).toMap.get, version = 1).decisions.head
    }
    val selected = services.identity.UnifiedRules("t", services.identity.UnifiedEvidence.Guards, Seq("venues.current"))
    val wide     = Seq(KinoMuza, models.Multikino, models.KinoPalacowe)
    val taken    = decided("SEVENTEEN World Tour 'NEW_'", wide, selected)
    (taken.basis, taken.fallback.map(_.id)) shouldBe ((ResolverDecision.Basis.Filled, Some("tt46658626")))
    taken.explanation.last should startWith ("filled by venues.current: 'Seventeen World Tour (New_) (2026)' imdb:tt46658626 — guards passed:")
    // the fill rule, as the id a take-change line and the rule-firing count name the take by
    (services.identity.DecidedBy.stage(taken, taken.members.head), services.identity.DecidedBy.of(taken, taken.members.head)) shouldBe
      (("fill", "fill:venues.current"))
    // two venues are no wide billing; a title not naming the film trips a guard; no rule selected takes nothing
    decided("SEVENTEEN World Tour 'NEW_'", wide.take(2), selected).basis shouldBe ResolverDecision.Basis.BelowThreshold
    decided("Muzyka", wide, selected).basis shouldBe ResolverDecision.Basis.BelowThreshold
    // an event no film database holds is asked nothing and taken as nothing ([[NonFilmEvents]])
    decided("Koncert", wide, selected).basis shouldBe ResolverDecision.Basis.Event
    decided("SEVENTEEN World Tour 'NEW_'", wide, selected.copy(fill = Nil)).basis shouldBe ResolverDecision.Basis.BelowThreshold
  }

  // US "A Prayer for the Dying" ×7 (re-capture 2026-10-06): Metacritic and RT agree on the 2026 film, but a film only review
  // sites agree on is taken as nothing — and that agreement kept the fill from being read at all
  it should "fill a cluster whose only agreement is review sites', which the stage does not take" in {
    val tour    = IdentityMeasures.Film("Seventeen World Tour (New_)", None, Nil, Some(2026), None, None, None, None)
    val record  = SourceRecord(tour, Map("imdb" -> "tt46658626"))
    val reviews = Map(
      VoterFamily.Metacritic     -> new HeldFamilyAnswers(VoterFamily.Metacritic, Map("seventeen-world-tour-new" -> record)),
      VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map("seventeen_world_tour_new" -> record)),
      VoterFamily.Imdb           -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt46658626" -> record))) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    val listings = Seq(KinoMuza, models.Multikino, models.KinoPalacowe).map(venue => listing(venue, "SEVENTEEN World Tour 'NEW_'"))
    val model = Resolution(Seq(ResolverDecision(listings.map(_.key), None, 0.2, ResolverDecision.Basis.BelowThreshold, Nil)()), listings.size,
      listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    def decided(families: Map[VoterFamily, FamilyAnswers]) =
      new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
        clock = _root_.tools.SpecClock.Pinned, tmdb = Some(NoVenueDetails),
        rules = services.identity.UnifiedRules("t", services.identity.UnifiedEvidence.Guards, Seq("venues.current")))
        .apply(model, listings.map(l => l.key -> l).toMap.get, version = 1).decisions.head
    // IMDb's take and the review sites' agree: the stage takes the agreement
    decided(reviews).basis shouldBe ResolverDecision.Basis.Agreed
    // IMDb only leaning, the review sites' agreement stands alone: the fill reads the cluster
    val leaning = reviews.updated(VoterFamily.Imdb, new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty))
    val filled  = decided(leaning)
    (filled.basis, filled.fallback.map(_.id)) shouldBe ((ResolverDecision.Basis.Filled, Some("tt46658626")))
  }

  // PL "Lalka (Dolly)" ×5: the fill's own resolve of the cluster asked TMDB what its store did not hold yet; the verdict
  // was kept with no fill, and stood once TMDB had answered — the families' agreement on Dolly never reached the fill again
  it should "read the fill again once TMDB answers what it could not, never keeping a verdict decided blind" in {
    val tour     = IdentityMeasures.Film("Seventeen World Tour (New_)", None, Nil, Some(2026), None, None, None, None)
    val families = Map(VoterFamily.Imdb -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt46658626" -> SourceRecord(tour, Map("imdb" -> "tt46658626"))))) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    var answered = false
    val tmdb = new services.identity.IdentityLookups {
      def hasDetail(listing: services.identity.Listing): Boolean = false
      def detail(listing: services.identity.Listing) = Answer.Known(None)
      def candidates(query: services.identity.CandidateQuery) = if (answered) Answer.Known(Nil) else Answer.Unknown
      def film(tmdbId: Int) = Answer.Known(None)
    }
    val listings = Seq(KinoMuza, models.Multikino, models.KinoPalacowe).map(venue => listing(venue, "SEVENTEEN World Tour 'NEW_'"))
    val model = Resolution(Seq(ResolverDecision(listings.map(_.key), None, 0.2, ResolverDecision.Basis.BelowThreshold, Nil)()), listings.size,
      listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    val stored = new InMemoryAgreementVerdicts
    val stage  = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), stored,
      clock = _root_.tools.SpecClock.Pinned, tmdb = Some(tmdb), rules = services.identity.UnifiedRules("t", services.identity.UnifiedEvidence.Guards, Seq("venues.current")))
    val byKey = listings.map(l => l.key -> l).toMap
    val blind = stage.apply(model, byKey.get, version = 1).decisions.head
    blind.basis shouldBe ResolverDecision.Basis.BelowThreshold
    stored.all().filter(_.filled.isEmpty) shouldBe empty   // nothing kept that a blind fill decided
    stage.waitingOn(blind).onTmdb shouldBe true            // waiting on TMDB, not settled
    answered = true
    val filled = stage.apply(model, byKey.get, version = 2).decisions.head
    (filled.basis, filled.fallback.map(_.id)) shouldBe ((ResolverDecision.Basis.Filled, Some("tt46658626")))
    stage.waitingOn(filled).settled shouldBe true
    stored.all().flatMap(_.filled).map(_.imdb) shouldBe Seq(Some("tt46658626"))
  }

  // the no-drift principle (docs/design/identity-no-drift.md): the year a verdict reads "current" by is an input — a
  // verdict decided in December stood into January, filled by a rule a whole resolve then no longer fired
  it should "decide a kept verdict again when the year turns, as a whole resolve in the new year would" in {
    val tour     = IdentityMeasures.Film("Seventeen World Tour (New_)", None, Nil, Some(2025), None, None, None, None)
    val families = Map(VoterFamily.Imdb -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt46658626" -> SourceRecord(tour, Map("imdb" -> "tt46658626"))))) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    val listings = Seq(KinoMuza, models.Multikino, models.KinoPalacowe).map(venue => listing(venue, "SEVENTEEN World Tour 'NEW_'"))
    val model = Resolution(Seq(ResolverDecision(listings.map(_.key), None, 0.2, ResolverDecision.Basis.BelowThreshold, Nil)()), listings.size,
      listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    var now   = java.time.Instant.parse("2026-12-31T12:00:00Z")
    val clock = new java.time.Clock {
      def getZone: java.time.ZoneId = java.time.ZoneOffset.UTC
      override def withZone(zone: java.time.ZoneId): java.time.Clock = this
      def instant(): java.time.Instant = now
    }
    def stageOn(stored: AgreementVerdicts) = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      stored, clock = clock, tmdb = Some(NoVenueDetails), rules = services.identity.UnifiedRules("t", services.identity.UnifiedEvidence.Guards, Seq("venues.current")))
    val byKey  = listings.map(l => l.key -> l).toMap
    val stored = new InMemoryAgreementVerdicts
    val stage  = stageOn(stored)
    stage.apply(model, byKey.get, version = 1).decisions.head.basis shouldBe ResolverDecision.Basis.Filled
    now = java.time.Instant.parse("2027-01-01T12:00:00Z")
    val whole = stageOn(new InMemoryAgreementVerdicts).apply(model, byKey.get, version = 1).decisions.head.basis
    whole shouldBe ResolverDecision.Basis.BelowThreshold
    stage.apply(model, byKey.get, version = 1).decisions.head.basis shouldBe whole
    stageOn(stored).apply(model, byKey.get, version = 1).decisions.head.basis shouldBe whole   // and after a restart
  }

  it should "take, by the pinned families.current rule, a current release two families take at one venue" in {
    // PL "TAJNY AGENT" (fixture identity-unmatched): IMDb and Filmweb take Mendonça Filho's 2025 film; one venue bills it
    val agent    = IdentityMeasures.Film("Tajny agent", None, Seq("The Secret Agent"), Some(2025), Some(158), Some(Seq("Kleber Mendonça Filho")), None, None)
    def families(year: Int) = Map(
      VoterFamily.Imdb    -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt27847051" -> SourceRecord(agent.copy(year = Some(year)), Map("imdb" -> "tt27847051")))),
      VoterFamily.Filmweb -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("10063391" -> SourceRecord(agent.copy(year = Some(year))))),
      VoterFamily.Wiki    -> new HeldFamilyAnswers(VoterFamily.Wiki, Map.empty))
    val one   = listing(KinoMuza, "TAJNY AGENT")
    val model = Resolution(Seq(ResolverDecision(Seq(one.key), None, 0.2, ResolverDecision.Basis.BelowThreshold, Nil)()), 1, Map(one.key -> 0),
      Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    def decided(year: Int, rules: services.identity.UnifiedRules) =
      new AgreementStage(families(year), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(1220564)),
        new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(NoVenueDetails), rules = rules)
        .apply(model, Map(one.key -> one).get, version = 1).decisions.head
    val shipped = services.identity.UnifiedRules.resolver
    val taken   = decided(2025, shipped)
    (taken.basis, taken.film) shouldBe ((ResolverDecision.Basis.Filled, Some(1220564)))
    taken.explanation.last should startWith ("filled by families.current: 'Tajny agent (2025)'")
    // an older film two families take is no current release; without the pinned rule nothing takes it
    decided(2019, shipped).basis shouldBe ResolverDecision.Basis.BelowThreshold
    decided(2025, shipped.copy(pinned = Nil)).basis shouldBe ResolverDecision.Basis.BelowThreshold
  }

  it should "take, by the pinned family.facts rule, the one film a film database takes that the venue's own year and director credit" in {
    // PL "11. UFF - Dowżenko. Pierwsze spojrzenie" (fixture identity-unmatched): Wikidata alone takes Konovalov's 2026
    // film; the festival's listing dates and credits it — two independent sources naming one film
    val film  = IdentityMeasures.Film("Dowżenko. Pierwsze spojrzenie", None, Nil, Some(2026), Some(80), Some(Seq("Konstantin Konovalov")), None, None)
    val families = Map(
      VoterFamily.Wiki -> new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q1" -> SourceRecord(film, Map("tmdb" -> "1483101")))),
      VoterFamily.Imdb -> new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty))
    def decided(one: services.identity.Listing, rules: services.identity.UnifiedRules) = {
      val model = Resolution(Seq(ResolverDecision(Seq(one.key), None, 0.14, ResolverDecision.Basis.BelowThreshold, Nil)()), 1, Map(one.key -> 0),
        Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
      new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
        new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(NoVenueDetails), rules = rules)
        .apply(model, Map(one.key -> one).get, version = 1).decisions.head
    }
    val credited = listing(KinoMuza, "Dowżenko. Pierwsze spojrzenie", year = Some(2026), director = Some("Konstantin Konovalov"))
    val shipped  = services.identity.UnifiedRules.resolver
    val taken    = decided(credited, shipped)
    (taken.basis, taken.film) shouldBe ((ResolverDecision.Basis.Filled, Some(1483101)))
    taken.explanation.last should startWith ("filled by family.facts: 'Dowżenko. Pierwsze spojrzenie (2026)'")
    // a family's take alone, the venue crediting nobody or another person, takes nothing; nor does the rule unpinned
    decided(listing(KinoMuza, "Dowżenko. Pierwsze spojrzenie", year = Some(2026)), shipped).basis shouldBe ResolverDecision.Basis.BelowThreshold
    decided(listing(KinoMuza, "Dowżenko. Pierwsze spojrzenie", year = Some(2026), director = Some("Oleksandr Dovzhenko")), shipped).basis shouldBe
      ResolverDecision.Basis.BelowThreshold
    // unpinned, the director's credit alone takes it (family.director, the year unread); without both, nothing does
    decided(credited, shipped.copy(pinned = shipped.pinned.filterNot(_ == "family.facts"))).explanation.last should startWith ("filled by family.director:")
    decided(credited, shipped.copy(pinned = shipped.pinned.filterNot(Set("family.facts", "family.director")))).basis shouldBe
      ResolverDecision.Basis.BelowThreshold
  }

  it should "take, by the pinned cast.venue rule, of two namesakes the one two of whose top-billed cast the venue's own text names" in {
    // PL Kino CK Lublin "Lalka" (fixture identity-unmatched; its own page, ck-lublin.bilety24.pl/wydarzenie/?id=162455): the
    // model weighs Kawalski's 2026 film and Has's 1968 one alike; the page names Dorociński, Urzędowska and Kondrat
    import services.identity.{CandidateQuery, DetailFacts, Hit, IdentityLookups, Listing, VenueNames}
    val ckLublin = "Miłość, która nie zna granic i ambicja, która nie zna ceny. „Lalka” powraca jako wielka filmowa opowieść. " +
      "Marcin Dorociński, Kamila Urzędowska i Marek Kondrat spotykają się na wielkim ekranie w ekranizacji powieści Bolesława Prusa."
    val records = Map(
      1321666 -> IdentityMeasures.Film("Lalka", Some("Lalka"), Seq("The Doll"), Some(2026), Some(151), Some(Seq("Maciej Kawalski")), None, None),
      81315   -> IdentityMeasures.Film("Lalka", Some("Lalka"), Seq("The Doll"), Some(1968), Some(159), Some(Seq("Wojciech Has")), None, None))
    val casts = Map(
      1321666 -> Seq("Marcin Dorociński", "Kamila Urzędowska", "Marek Kondrat", "Andrzej Seweryn", "Krystyna Janda", "Maria Dębska"),
      81315   -> Seq("Mariusz Dmochowski", "Beata Tyszkiewicz", "Tadeusz Fijewski", "Kalina Jędrusik", "Tadeusz Kondrat"))
    val namesakes = Seq(Hit(1321666, "Lalka", Some("Lalka"), Some(2026), 20.0), Hit(81315, "Lalka", Some("Lalka"), Some(1968), 3.0))
    def tmdb(page: Option[String], castOf: Int => Option[Seq[String]], hits: Seq[Hit] = namesakes) = new IdentityLookups {
      def hasDetail(listing: Listing): Boolean = page.isDefined
      def detail(listing: Listing) = Answer.Known(page.map(text => DetailFacts(None, Nil, None, None, synopsis = Some(text))))
      def candidates(query: CandidateQuery) = query match {
        case CandidateQuery.Title(title) if title.equalsIgnoreCase("Lalka") => Answer.Known(hits)
        case _ => Answer.Known(Nil)
      }
      def film(tmdbId: Int) = Answer.Known(records.get(tmdbId))
      override def cast(tmdbId: Int) = Answer.Known(castOf(tmdbId))
    }
    val families = Seq(VoterFamily.Imdb, VoterFamily.Wiki).map(family => family -> new HeldFamilyAnswers(family, Map.empty)).toMap
    def decided(one: Listing, lookups: IdentityLookups, rules: services.identity.UnifiedRules = services.identity.UnifiedRules.resolver) = {
      val model = Resolution(Seq(ResolverDecision(Seq(one.key), None, 0.3, ResolverDecision.Basis.BelowThreshold, Nil)()), 1, Map(one.key -> 0),
        Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
      new AgreementStage(families, lookups, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
        clock = _root_.tools.SpecClock.Pinned, tmdb = Some(lookups), rules = rules).apply(model, Map(one.key -> one).get, version = 1).decisions.head
    }
    val bare   = listing(KinoMuza, "Lalka")
    val stated = bare.copy(names = VenueNames.of(Seq(ckLublin)))
    val known  = tmdb(None, casts.get)
    val taken  = decided(stated, known)
    (taken.basis, taken.film) shouldBe ((ResolverDecision.Basis.Filled, Some(1321666)))
    taken.explanation.last should (startWith ("filled by cast.venue: 'Lalka (2026)' tmdb:1321666") and
      endWith ("the venue's own text names its cast Marcin Dorociński, Kamila Urzędowska, Marek Kondrat"))
    // the venue's detail page names them as well as its listing does
    decided(bare, tmdb(Some(ckLublin), casts.get)).film shouldBe Some(1321666)
    // no text, one name, a rival's cast not known, a feed catalogue's text, the rule unpinned: nothing taken
    decided(bare, known).basis shouldBe ResolverDecision.Basis.BelowThreshold
    decided(bare.copy(names = VenueNames.of(Seq("Gra Marcin Dorociński."))), known).basis shouldBe ResolverDecision.Basis.BelowThreshold
    decided(stated, tmdb(None, id => casts.get(id).filter(_ => id == 1321666))).basis shouldBe ResolverDecision.Basis.BelowThreshold
    decided(stated.copy(catalogueIds = Seq(services.identity.CatalogueId("webedia", "1"))), tmdb(Some(ckLublin), casts.get)).basis shouldBe
      ResolverDecision.Basis.BelowThreshold
    val shipped = services.identity.UnifiedRules.resolver
    decided(stated, known, shipped.copy(pinned = shipped.pinned.filterNot(_ == "cast.venue"))).basis shouldBe ResolverDecision.Basis.BelowThreshold
    // a tie-break, never a take: a lone candidate the text names is no tie (UK Electric Palace Hastings' "Straight Talk: A
    // Tribute to Dolly Parton" named the 1992 film's cast and was taken as it), nor is a listing billing a director one —
    // the director decides there
    def byCast(decision: ResolverDecision) = decision.explanation.lastOption.exists(_.startsWith("filled by cast.venue"))
    byCast(decided(stated, tmdb(None, casts.get, hits = namesakes.take(1)))) shouldBe false
    byCast(decided(stated.copy(directors = Seq("Maciej Kawalski")), known)) shouldBe false
  }

  "an unmatched cluster its families agree on" should "take the TMDB film the agreed IMDb id finds, and name the families" in {
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = imdb => Answer.Known(Option.when(imdb == "tt16315948")(913760)), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    val decided = stage.apply(resolution, listingOf, version = 1)
    val klondikeDecision = decided.decisions.head
    (klondikeDecision.film, klondikeDecision.basis) shouldBe ((Some(913760), ResolverDecision.Basis.Agreed))
    klondikeDecision.explanation.last should include ("filmweb, imdb, rt, wiki agree on 'Klondike' (2022) tt16315948")
    decided.decisions(1) shouldBe resolution.decisions(1)
  }

  it should "take no film the families agree on whose namesake's TMDB description the venue's synopsis names over its own, saying why" in {
    // PL Janosik "edukacja MH: Binti" (identity-unmatched capture): IMDb, Wikidata and Filmweb agree on Shamte's 2021 film;
    // the venue's page describes Migom's 2019 one — a girl in Belgium who meets Elias
    import services.identity.{CandidateQuery, DetailFacts, Hit, IdentityLookups, Listing, ProseWords}
    val shamte = IdentityMeasures.Film("Binti", Some("Binti"), Nil, Some(2021), Some(98), Some(Seq("Seko Shamte")), None, None)
    val migom  = IdentityMeasures.Film("Binti", Some("Binti"), Nil, Some(2019), Some(90), Some(Seq("Frederike Migom")), None, None)
    val families = Map(
      VoterFamily.Imdb    -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt11867140" -> SourceRecord(shamte, Map("imdb" -> "tt11867140")))),
      VoterFamily.Wiki    -> new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q108940090" -> SourceRecord(shamte, Map("imdb" -> "tt11867140", "tmdb" -> "855156")))),
      VoterFamily.Filmweb -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("10016218" -> SourceRecord(shamte))))
    val page = "Binti, nastoletnia imigrantka nielegalnie mieszkająca w Belgii, nie pamięta Konga. Gdy poznaje Eliasa, młodego obrońcę praw zwierząt."
    val overviews = Map(
      855156 -> "Four Tanzanian women are unknowingly connected through their ability to persevere extreme hardships in the city of Dar-es-Salaam.",
      577285 -> "Binti is a 12-year-old girl of Congolese origin. Binti meets Elias and befriends her, so they can stay in Belgium.")
    def tmdb(described: Set[Int]) = new IdentityLookups {
      def hasDetail(listing: Listing): Boolean = true
      def detail(listing: Listing) = Answer.Known(Some(DetailFacts(None, Nil, None, None, synopsis = Some(page))))
      def candidates(query: CandidateQuery) = query match {
        case CandidateQuery.Title(title) if title.equalsIgnoreCase("Binti") =>
          Answer.Known(Seq(Hit(855156, "Binti", Some("Binti"), Some(2021), 1.5), Hit(577285, "Binti", Some("Binti"), Some(2019), 1.1)))
        case _ => Answer.Known(Nil)
      }
      def film(tmdbId: Int) = Answer.Known(Map(855156 -> shamte, 577285 -> migom).get(tmdbId))
      override def overview(tmdbId: Int) = Answer.Known(overviews.get(tmdbId).filter(_ => described(tmdbId)).map(text => ProseWords.ofDescription(Seq(text))))
    }
    def decided(lookups: IdentityLookups) = {
      val billed = listing(KinoMuza, "Binti")
      val model  = Resolution(Seq(ResolverDecision(Seq(billed.key), None, 0.5, ResolverDecision.Basis.BelowThreshold, Nil)()), 1,
        Map(billed.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
      new AgreementStage(families, lookups, normalizer, IdentityCalibration.resolver,
        tmdbOf = imdb => Answer.Known(Option.when(imdb == "tt11867140")(855156)), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned,
        tmdb = Some(lookups)).apply(model, Map(billed.key -> billed).get, version = 1).decisions.head
    }
    val refused = decided(tmdb(Set(855156, 577285)))
    (refused.film, refused.basis) shouldBe ((None, ResolverDecision.Basis.BelowThreshold))
    refused.explanation.last shouldBe "no agreement on 'Binti' (2021): the venue's synopsis names 2 words of 'Binti' (2019)'s TMDB description and none of its own"
    // no description of the namesake, or of the film agreed on: nothing to compare, the agreement stands
    decided(tmdb(Set(855156))).film shouldBe Some(855156)
    decided(tmdb(Set(577285))).film shouldBe Some(855156)
  }

  it should "take no film the families agree on that the billing names as one piece beside another thing, nor fill one" in {
    // DE "Femme - Frauen heilen die Welt" (fixture identity-unmatched): four families agree on the 2023 "Femme" its first
    // piece names — the billing is a documentary its subtitle names
    def decided(title: String, year: Option[Int] = None) = {
      val billed = listing(KinoMuza, title, year)
      val model  = Resolution(Seq(ResolverDecision(Seq(billed.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()), 1,
        Map(billed.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
      new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver,
        tmdbOf = imdb => Answer.Known(Option.when(imdb == "tt16315948")(913760)), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
        .apply(model, Map(billed.key -> billed).get, version = 1).decisions.head
    }
    val piece = decided("Klondike - Frauen heilen die Welt")
    (piece.film, piece.basis) shouldBe ((None, ResolverDecision.Basis.BelowThreshold))
    piece.explanation.last should include ("one piece of the billing — 'frauen heilen die welt' is billed beside 'Klondike'")
    // the venue's own programme beside it is no other thing
    decided("Klondike - Film Club").film shouldBe Some(913760)
    // nor where the billing states the film's own year: the model's keep (BilledPiece), which this stage read none of
    decided("Klondike - Frauen heilen die Welt", Some(2022)).film shouldBe Some(913760)
  }

  it should "say why it takes no film the families agree on that a guard refuses, and keep that reason with its verdict" in {
    // a year the title bills four years from the agreed film's ([[services.identity.PosterEvidence.datingApart]])
    val billed   = listing(KinoMuza, "Klondike 2026")
    val model    = Resolution(Seq(ResolverDecision(Seq(billed.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()), 1,
      Map(billed.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    val verdicts = new InMemoryAgreementVerdicts
    val decided  = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = imdb => Answer.Known(Option.when(imdb == "tt16315948")(913760)), verdicts, clock = _root_.tools.SpecClock.Pinned)
      .apply(model, Map(billed.key -> billed).get, version = 1).decisions.head
    val why = "no agreement on 'Klondike' (2022): 'Klondike 2026' bills 2026"
    (decided.film, decided.basis, decided.explanation.last) shouldBe ((None, ResolverDecision.Basis.BelowThreshold, why))
    verdicts.all().flatMap(_.refused) shouldBe Seq(why)
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

  // DE "Dolly" ×10 (live drift, 2026-10-25): the verdict was decided while TMDB's record of the film the model leaned to
  // was not filed — two families and an unfacted vote agree on nothing — and kept with only the families' answers among
  // its reads, so the record's filing never reopened it. A verdict is a cache of everything it read: the record filed
  // reopens it, and it comes to what a resolve with the record there from the start comes to
  it should "decide a kept verdict again once the record of the film TMDB leans to is filed, as if it had been there all along" in {
    val two = agreeing().view.filterKeys(Set(VoterFamily.Imdb, VoterFamily.Wiki)).toMap ++ Map(
      VoterFamily.Filmweb -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map.empty), VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map.empty))
    var filed = false
    val tmdb = new services.identity.IdentityLookups {
      def hasDetail(listing: services.identity.Listing): Boolean = false
      def detail(listing: services.identity.Listing) = Answer.Known(None)
      def candidates(query: services.identity.CandidateQuery) = Answer.Known(Nil)
      def film(tmdbId: Int) = if (!filed) Answer.Unknown else Answer.Known(Option.when(tmdbId == 913760)(klondike))
    }
    val model = resolution.copy(decisions = resolution.decisions.updated(0,
      ResolverDecision(Seq(bare.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil, candidate = Some(ResolverDecision.Leaning(913760, 0)))()))
    def stageOver(stored: AgreementVerdicts) = new AgreementStage(two, tmdb, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      stored, clock = _root_.tools.SpecClock.Pinned)
    val stored = new InMemoryAgreementVerdicts
    val stage  = stageOver(stored)
    stage.apply(model, listingOf, version = 1).decisions.head.film shouldBe None
    // the record is in its receipt, so its filing moves the stage's answers ([[AgreementStage.reads]])
    stage.reads(AgreementStage.recordReadId(913760)) shouldBe true
    filed = true
    val whole = stageOver(new InMemoryAgreementVerdicts).apply(model, listingOf, version = 2).decisions.head
    (whole.film, whole.basis) shouldBe ((Some(913760), ResolverDecision.Basis.Agreed))
    stage.apply(model, listingOf, version = 2).decisions.head.film shouldBe whole.film
    stageOver(stored).apply(model, listingOf, version = 2).decisions.head.film shouldBe whole.film   // and after a restart
  }

  it should "read the year, director and running time the venue's own page states as the listing's facts, as the model does" in {
    // PL "The Taxidermist | Splat!FilmFest": IMDb alone takes it; the venue's page, not its listing, credits Paulo Nascimento, 99 min
    val undated = listing(KinoMuza, "Klondike")
    val alone   = Map(VoterFamily.Imdb -> agreeing()(VoterFamily.Imdb)) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    def page(facts: Option[services.identity.DetailFacts]) = new services.identity.IdentityLookups {
      def hasDetail(listing: services.identity.Listing): Boolean = facts.isDefined
      def detail(listing: services.identity.Listing) = Answer.Known(facts)
      def candidates(query: services.identity.CandidateQuery) = Answer.Known(Nil)
      def film(tmdbId: Int) = Answer.Known(None)
    }
    def decided(facts: Option[services.identity.DetailFacts]) = {
      val model = Resolution(Seq(ResolverDecision(Seq(undated.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()), 1,
        Map(undated.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
      new AgreementStage(alone, page(facts), normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
        new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned).apply(model, Map(undated.key -> undated).get, version = 1).decisions.head
    }
    decided(None).film shouldBe None
    val taken = decided(Some(services.identity.DetailFacts(Some(2022), Seq("Maryna Er Gorbach"), Some(100), None)))
    (taken.film, taken.basis) shouldBe ((Some(913760), ResolverDecision.Basis.Agreed))
    taken.explanation.last should include ("corroborated by listing, runtime")
  }

  it should "read a listings site's catalogue page as the catalogue's claim, not the venue's word" in {
    // A Flicks /movie/<slug>/ page is Flicks' own entry for the film, the one every UK and US venue's listing links: the
    // year and director it states are that entry's, so they corroborate as a feed's facts do, never as the venue's own
    val flicks  = listing(KinoMuza, "Klondike").copy(page = Some("https://www.flicks.co.uk/movie/klondike/"))
    val alone   = Map(VoterFamily.Imdb -> agreeing()(VoterFamily.Imdb)) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    val page = new services.identity.IdentityLookups {
      def hasDetail(listing: services.identity.Listing): Boolean = true
      def detail(listing: services.identity.Listing) = Answer.Known(Some(services.identity.DetailFacts(Some(2022), Seq("Maryna Er Gorbach"), Some(100), None)))
      def candidates(query: services.identity.CandidateQuery) = Answer.Known(Nil)
      def film(tmdbId: Int) = Answer.Known(None)
    }
    val model = Resolution(Seq(ResolverDecision(Seq(flicks.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()), 1,
      Map(flicks.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    val taken = new AgreementStage(alone, page, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned).apply(model, Map(flicks.key -> flicks).get, version = 1).decisions.head
    taken.explanation.mkString("\n") should not include "corroborated by listing"
  }

  /** DE "André Rieus Weihnachtskonzert 2026: Let it Snow" (fixture identity-unmatched): Metacritic and Rotten Tomatoes
   *  take the 2019 "Let It Snow", the model's best candidate (`vote`) — their records linking `linked` beside their own ids. */
  private def reviewSitesOnly(linked: Map[String, String], tmdbOf: String => Answer[Option[Int]],
                              vote: Option[ResolverDecision.Leaning] = Some(ResolverDecision.Leaning(913760, 0)),
                              billed: services.identity.Listing = bare): (AgreementStage, InMemoryAgreementVerdicts, ResolverDecision) = {
    val reviews = Map(
      VoterFamily.Metacritic     -> new HeldFamilyAnswers(VoterFamily.Metacritic, Map("klondike" -> SourceRecord(klondike, Map("metacritic" -> "klondike") ++ linked))),
      VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike, Map("rt" -> "klondike_2022") ++ linked))),
      VoterFamily.Imdb           -> new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty))
    val tmdb = new services.identity.IdentityLookups {
      def hasDetail(listing: services.identity.Listing): Boolean = false
      def detail(listing: services.identity.Listing) = Answer.Known(None)
      def candidates(query: services.identity.CandidateQuery) = Answer.Known(Nil)
      def film(tmdbId: Int) = Answer.Known(Option.when(tmdbId == 913760)(klondike))
    }
    val model = resolution.copy(decisions = resolution.decisions.updated(0, ResolverDecision(Seq(billed.key), None, 0.7,
      ResolverDecision.Basis.BelowThreshold, Nil, candidate = vote)()))
    val verdicts = new InMemoryAgreementVerdicts
    val stage = new AgreementStage(reviews, tmdb, normalizer, IdentityCalibration.resolver, tmdbOf = tmdbOf, verdicts,
      clock = _root_.tools.SpecClock.Pinned, identities = Seq(VoterFamily.Wiki, VoterFamily.Filmweb))
    (stage, verdicts, stage.apply(model, Map(billed.key -> billed, matched.key -> matched).get, version = 1).decisions.head)
  }

  it should "take no film only review sites take, though the model's vote completes them" in {
    // a review page names no film a card stands on
    val (_, _, decided) = reviewSitesOnly(Map.empty, tmdbOf = _ => Answer.Known(None))
    (decided.film, decided.fallback) shouldBe ((None, None))
  }

  it should "ask TMDB to find no IMDb id only review sites agree on, the take it would complete never made" in {
    // the listing's own credit completing them, not the model's vote: its TMDB id would name the film, no find needed
    val (stage, verdicts, decided) = reviewSitesOnly(Map("imdb" -> "tt16315948"), tmdbOf = _ => Answer.Unknown, vote = None,
      billed = listing(KinoMuza, "Klondike", year = Some(2022), director = Some("Maryna Er Gorbach")))
    verdicts.all().flatMap(_.agreed).map(_.crossId("imdb")) shouldBe Seq(Some("tt16315948"))   // they agree on the linked film
    (decided.film, decided.fallback) shouldBe ((None, None))
    stage.wanted.collect { case Question.Find(tt) => tt } shouldBe empty
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

  it should "take a film neither TMDB nor IMDb holds by the id of a family the country lets stand as its identity, Wikidata's first" in {
    // Wikidata, Filmweb and RT agree on a film no record links an IMDb id to (DE "Jeder Acker, jede Fabrik", PL "Płazy")
    val noImdb = Map(
      VoterFamily.Imdb           -> new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty),
      VoterFamily.Wiki           -> new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q141180912" -> SourceRecord(klondike, Map("wikidata" -> "Q141180912")))),
      VoterFamily.Filmweb        -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("10105049" -> SourceRecord(klondike, Map("filmweb" -> "10105049")))),
      VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike, Map("rt" -> "klondike_2022")))))
    def taken(identities: Seq[VoterFamily]) =
      new AgreementStage(noImdb, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
        clock = _root_.tools.SpecClock.Pinned, identities = identities).apply(resolution, listingOf, version = 1).decisions.head
    // with the film's title and year, that a link to its page is built of
    taken(Seq(VoterFamily.Wiki, VoterFamily.Filmweb)).fallback shouldBe Some(ResolverDecision.Fallback("wikidata", "Q141180912", 1.0, Some("Klondike"), Some(2022)))
    taken(Seq(VoterFamily.Filmweb)).fallback shouldBe Some(ResolverDecision.Fallback("filmweb", "10105049", 1.0, Some("Klondike"), Some(2022)))
    val none = taken(Nil)   // a country no family's own id stands in: the agreed film is not taken
    (none.fallback, none.basis) shouldBe ((None, ResolverDecision.Basis.BelowThreshold))
  }

  it should "keep the families' own ids with the decision, and its verdict across a restart without asking them again" in {
    val verdicts = new InMemoryAgreementVerdicts
    val first = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts, clock = _root_.tools.SpecClock.Pinned)
    first.apply(resolution, listingOf, version = 1).decisions.head.agreed shouldBe
      Map("imdb" -> "tt16315948", "wiki" -> "Q1", "filmweb" -> "880000", "rt" -> "klondike_2022")
    verdicts.all().filter(_.correction.isEmpty).map(_.agreed.map(_.families)) shouldBe Seq(Some(Set(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes)))
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

  it should "refuse after a restart, from its stored verdict, the agreed film it refused before: the store keeps what the take reads" in {
    // DE Museum-Lichtspiele's "Internationales Gitarrenfestival Tübingen - Eröffnungskonzert - Raíces" (identity-slice --drift,
    // recording 573): three families agree on 'Roots' (1954), its ORIGINAL title the billing's last piece. A fresh stage
    // refuses it; the stage a deploy brought up from the stored verdict took it, its film kept as a title and year alone
    val roots   = IdentityMeasures.Film("Roots", Some("Raíces"), Nil, Some(1954), Some(83), None, None, None)
    val record  = SourceRecord(roots, Map("imdb" -> "tt0047395"))
    val three   = Map(
      VoterFamily.Imdb    -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt0047395" -> record)),
      VoterFamily.Wiki    -> new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q2" -> record)),
      VoterFamily.Filmweb -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880001" -> SourceRecord(roots))))
    val billed  = listing(KinoMuza, "Internationales Gitarrenfestival Tübingen - Eröffnungskonzert - Raíces")
    val model   = Resolution(Seq(ResolverDecision(Seq(billed.key), None, 0.4, ResolverDecision.Basis.BelowThreshold, Nil)()), 1,
      Map(billed.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    // the verdicts as Mongo keeps them: each written as its document and read back
    val kept    = new InMemoryAgreementVerdicts
    val stored  = new AgreementVerdicts {
      def all(): Seq[StoredVerdict] = kept.all().map(v => AgreementVerdicts.decode(AgreementVerdicts.encode(v)))
      def replace(removed: Set[String], added: Seq[StoredVerdict]): Unit = kept.replace(removed, added)
    }
    def stage() = new AgreementStage(three, NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = imdb => Answer.Known(Option.when(imdb == "tt0047395")(122769)), stored, clock = _root_.tools.SpecClock.Pinned)
    val fresh = stage().apply(model, Map(billed.key -> billed).get, version = 1).decisions.head
    fresh.film shouldBe None
    fresh.explanation.last should include ("one piece of the billing")
    val restarted = stage().apply(model, Map(billed.key -> billed).get, version = 1).decisions.head
    (restarted.film, restarted.explanation.last) shouldBe ((fresh.film, fresh.explanation.last))
  }

  it should "decide a cluster it forgot as a stage with nothing kept, its stored verdict dropped (the drift check's adopt)" in {
    val verdicts = new InMemoryAgreementVerdicts
    var asked = 0
    val counting = agreeing().map { case (family, answers) => family -> new FamilyAnswers {
      val family: VoterFamily = answers.family
      def titled(text: String)     = { asked += 1; answers.titled(text) }
      def directedBy(name: String) = { asked += 1; answers.directedBy(name) }
      def record(id: String)       = { asked += 1; answers.record(id) }
    } }
    val stage = new AgreementStage(counting, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)
    asked = 0
    val again = stage.apply(resolution, listingOf, version = 1)
    asked shouldBe 0   // the same decisions, nothing filed: the verdict stands unread
    val cluster = stage.keyOf(again.decisions.head)
    stage.forget(Set(cluster))
    verdicts.all().map(_.id) should not contain cluster
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)
    asked should be > 0   // read again, as a fresh stage reads it
    verdicts.all().map(_.id) should contain (cluster)
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
    verdicts.all().filter(_.correction.isEmpty).map(_.rules) shouldBe Seq(s"${AgreementStage.RulesVersion}|${services.identity.UnifiedRules.resolver.version}")
  }

  // the owner (2026-10-25): every verdict prod stored before receipts were whole is decided again, but a budget an apply
  // at a time — a whole-corpus resolve at the deploy's boot is what OOM-killed worker-us — the rest standing meanwhile,
  // the backlog said until none is left, and how many are left reported
  it should "decide verdicts stored under other rules again a budget an apply at a time, the rest standing meanwhile" in {
    val listings = Seq(KinoMuza, models.Multikino, models.KinoPalacowe).map(venue => listing(venue, "Klondike", year = Some(2022)))
    val model = Resolution(listings.map(l => ResolverDecision(Seq(l.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()), 3,
      listings.zipWithIndex.map { case (l, i) => l.key -> i }.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    val byKey    = listings.map(l => l.key -> l).toMap
    val verdicts = new InMemoryAgreementVerdicts
    new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned).apply(model, byKey.get, version = 1).decisions.map(_.film) shouldBe Seq.fill(3)(Some(913760))
    verdicts.replace(Set.empty, verdicts.all().map(_.copy(rules = "an older build")))
    val left  = scala.collection.mutable.ArrayBuffer.empty[Int]
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned, metrics = applied => left += applied.incomplete, backfillPerApply = 1)
    (1 to 3).foreach { i =>
      stage.apply(model, byKey.get, version = 1).decisions.map(_.film) shouldBe Seq.fill(3)(Some(913760))   // standing meanwhile
      stage.backlog shouldBe (i < 3)
    }
    left shouldBe Seq(2, 1, 0)
  }

  it should "decide again when an answer it read moved, and drop the verdict of a cluster no longer unmatched" in {
    val verdicts = new InMemoryAgreementVerdicts
    new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts, clock = _root_.tools.SpecClock.Pinned)
      .apply(resolution, listingOf, version = 1)
    val dissent = agreeing() ++ Seq(VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    // Filmweb's and RT's answers refiled after the restart (version 1 → 2): only a verdict that read them is decided again.
    val refiled = new AnswerChanges {
      def changedSince(version: Long): Option[Set[String]] =
        Some(if (version < 2) verdicts.all().filter(_.correction.isEmpty).head.reads.keySet.filter(q => q.startsWith("filmweb|") || q.startsWith("rt|")) else Set.empty)
    }
    val again = new AgreementStage(dissent, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)), verdicts,
      clock = _root_.tools.SpecClock.Pinned, changes = refiled)
    again.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)   // the restart trusts what it kept
    again.apply(resolution, listingOf, version = 2).decisions.head shouldBe resolution.decisions.head
    verdicts.all().filter(_.correction.isEmpty).map(_.agreed) shouldBe Seq(None)
    val matchedNow = resolution.copy(decisions = resolution.decisions.map(_.copy(film = Some(1))(services.identity.DecisionTrace.Empty)))
    again.apply(matchedNow, listingOf, version = 2)
    verdicts.all().filter(_.correction.isEmpty) shouldBe empty
  }

  it should "ask TMDB about the agreed IMDb id before taking it as a fallback" in {
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Unknown, new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head shouldBe resolution.decisions.head
    stage.wanted.collect { case Question.Find(tt) => tt } shouldBe Set("tt16315948")
  }

  it should "hand the questions it meets unanswered to the queue at once, and a stale answer's too while it uses it" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    new AgreementStage(agreeing(rtAnswered = false), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, ask = asked += _, clock = _root_.tools.SpecClock.Pinned).apply(resolution, listingOf, version = 1)
    asked.flatten should contain (Question.Family(VoterFamily.RottenTomatoes, "title|Klondike"))
    val staleRt = agreeing() + (VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes,
      Map("klondike_2022" -> SourceRecord(klondike)), stale = true))
    val refreshed = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val stage = new AgreementStage(staleRt, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, ask = refreshed += _, clock = _root_.tools.SpecClock.Pinned)
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)   // the stale answers still count
    refreshed.flatten should contain (Question.Family(VoterFamily.RottenTomatoes, "title|Klondike"))
    refreshed.clear()
    stage.apply(resolution, listingOf, version = 1)                                // a quiet tick reads no answer again…
    refreshed shouldBe empty
    stage.apply(resolution, listingOf, version = 2)                                // …one after a filing re-reads the kept verdict's
    refreshed.flatten.collect { case q @ Question.Family(VoterFamily.RottenTomatoes, _) => q } should not be empty
  }

  /** A family whose answers come as `isAnswered` says, each title search of IMDb's counted in `resolves`. */
  private final class Flipping(val family: VoterFamily, records: Map[String, SourceRecord], isAnswered: => Boolean, resolves: () => Unit = () => ())
      extends FamilyAnswers {
    private def held = new HeldFamilyAnswers(family, records, unanswered = !isAnswered)
    def titled(text: String)     = { resolves(); held.titled(text) }
    def directedBy(name: String) = held.directedBy(name)
    def record(id: String)       = held.record(id)
    override def fresh(question: String) = held.fresh(question)
  }

  // The settle rule (docs/design/identity-questions.md, step 4, the owner's, 2026-10-24): no clock. A cluster settles once
  // every question it waits on was attempted — answered, durably unanswered or failed — and is decided on what answered.
  it should "resolve a waiting cluster again only once all its questions are answered, or settle it once every one is attempted" in {
    var resolves = 0
    var rtAnswered, filmwebAnswered, filmwebFailed = false
    val families = Map(
      VoterFamily.Imdb           -> new Flipping(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike, Map("imdb" -> "tt16315948"))), true, () => resolves += 1),
      VoterFamily.Wiki           -> new Flipping(VoterFamily.Wiki, Map("Q1" -> SourceRecord(klondike, Map("imdb" -> "tt16315948"))), true),
      VoterFamily.Filmweb        -> new Flipping(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike)), filmwebAnswered),
      VoterFamily.RottenTomatoes -> new Flipping(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike)), rtAnswered))
    val clock = new tools.MutableClock(java.time.Instant.parse("2026-10-04T21:00:00Z"))
    val handed = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val stage = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, ask = handed += _, clock = clock,
      attempted = { case Question.Family(VoterFamily.Filmweb, _) => filmwebFailed; case _ => false })
    val model = resolution
    stage.apply(model, listingOf, version = 1).decisions.head.film shouldBe None
    val (afterFirst, handedFirst) = (resolves, handed.size)
    stage.apply(model, listingOf, version = 1)
    (resolves, handed.size) shouldBe ((afterFirst, handedFirst))   // a quiet tick: no resolve, nothing handed
    rtAnswered = true
    stage.apply(model, listingOf, version = 2)                      // one of its questions answered, Filmweb's never asked: no resolve
    resolves shouldBe afterFirst
    clock.advanceSeconds(java.time.Duration.ofDays(7).toSeconds)
    stage.apply(model, listingOf, version = 2).decisions.head.film shouldBe None   // no clock settles it: Filmweb's question holds
    resolves shouldBe afterFirst
    filmwebFailed = true
    // Filmweb asked and failed, though nothing was filed: settled on the families that answered (IMDb, Wikidata, RT agree)
    stage.apply(model, listingOf, version = 2).decisions.head.film shouldBe Some(913760)
    resolves should be > afterFirst
    stage.waitingOn(stage.apply(model, listingOf, version = 2).decisions.head).settled shouldBe true
    handed.clear()
    stage.apply(model, listingOf, version = 3).decisions.head.film shouldBe Some(913760)   // settled: the take stands, its
    handed.flatten.collect { case q @ Question.Family(VoterFamily.Filmweb, _) => q } should not be empty   // question still asked
    // a re-scrape moving its listing reads the settled cluster again: settled again, on the same families, not waiting anew
    val rescraped    = bare.copy(runtime = Some(100))
    val beforeScrape = resolves
    stage.apply(model, Map(bare.key -> rescraped, matched.key -> matched).get, version = 3, republished = None).decisions.head.film shouldBe Some(913760)
    resolves should be > beforeScrape
    stage.unreached.values.flatten.collect { case q @ Question.Family(VoterFamily.Filmweb, _) => q } should not be empty
    filmwebAnswered = true
    val beforeAnswer = resolves
    stage.apply(model, Map(bare.key -> rescraped, matched.key -> matched).get, version = 4).decisions.head.film shouldBe Some(913760)   // its answer came: read again
    resolves should be > beforeAnswer
    stage.unreached shouldBe empty
  }

  // A quiet apply (nothing filed) reads what it waits on again only once the memory behind `attempted` marked something:
  // reading every waiting question's memory on each tick cost the e2e corpus's quiet projection 4.4 MB (3,459 questions).
  it should "read none of what it waits on while the memory marks nothing, and settle on the apply after it marks" in {
    var filmwebFailed = false
    var marks, reads  = 0L
    val families = agreeing() ++ Map(
      VoterFamily.Filmweb -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike)), unanswered = true))
    val stage = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned,
      attempted = { question => reads += 1; question match { case Question.Family(VoterFamily.Filmweb, _) => filmwebFailed; case _ => false } },
      attemptsMarked = Some(() => marks))
    val resolution = this.resolution                                                     // the model hands the same decisions
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe None   // Filmweb's question holds it
    stage.awaiting should not be empty
    reads = 0
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe None   // quiet: nothing marked, nothing read
    reads shouldBe 0L
    marks += 1                                                                           // a mark of another question: read
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe None   // once, none attempted, still held
    reads should be > 0L
    reads = 0
    stage.apply(resolution, listingOf, version = 1)
    reads shouldBe 0L                                                                    // and not again until the next mark
    filmwebFailed = true; marks += 1                                                     // Filmweb asked and failed: settled
    stage.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe Some(913760)
  }

  it should "move a settled take on a later answer, its unanswered questions asked on every pass and after a restart too" in {
    var rtAnswered = false
    val families = agreeing() ++ Map(
      VoterFamily.Filmweb        -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike)), unanswered = true),
      VoterFamily.RottenTomatoes -> new Flipping(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike)), rtAnswered))
    val verdicts = new InMemoryAgreementVerdicts
    val failed: Question => Boolean = {   // Filmweb and RT asked, and failed
      case Question.Family(VoterFamily.Filmweb | VoterFamily.RottenTomatoes, _) => true
      case _                                                                    => false
    }
    val first = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      verdicts, clock = _root_.tools.SpecClock.Pinned, attempted = failed)
    val settled = first.apply(resolution, listingOf, version = 1).decisions.head
    settled.film shouldBe None                                      // settled on IMDb and Wikidata alone: no agreement, no film
    first.waitingOn(settled).settled shouldBe true
    // a worker restarted: the settled verdict stands, and what none answered is asked again on every pass
    val handed = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val again = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      verdicts, ask = handed += _, clock = _root_.tools.SpecClock.Pinned, attempted = failed)
    again.apply(resolution, listingOf, version = 1).decisions.head.film shouldBe None
    handed.flatten.collect { case Question.Family(family, _) => family }.toSet shouldBe Set(VoterFamily.Filmweb, VoterFamily.RottenTomatoes)
    rtAnswered = true                                               // RT answers at last: IMDb, Wikidata and RT agree
    again.apply(resolution, listingOf, version = 2).decisions.head.film shouldBe Some(913760)
  }

  // The owner's choice (docs/design/identity-questions.md, step 3): a cluster with a newly scraped listing has its families
  // asked inline, so its first take reads their answers; a cluster the stage already knew waits on the queue, as before.
  it should "ask a new cluster's families inline, once, and take what they answer in its first take" in {
    var rtAnswered = false
    val rt = new FamilyAnswers {
      val family: VoterFamily = VoterFamily.RottenTomatoes
      private def held = new HeldFamilyAnswers(family, Map("klondike_2022" -> SourceRecord(klondike)), unanswered = !rtAnswered)
      def titled(text: String)     = held.titled(text)
      def directedBy(name: String) = held.directedBy(name)
      def record(id: String)       = held.record(id)
      override def fresh(question: String) = held.fresh(question)
    }
    val asked = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val askNow: AgreementStage.AskNow = (questions, _) => { asked += questions; rtAnswered = true }
    val stage = new AgreementStage(agreeing() + (VoterFamily.RottenTomatoes -> rt), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(Some(913760)), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, askNow = askNow)
    val known = resolution.copy(decisions = resolution.decisions.drop(1), nodes = 1, familyOf = Map(matched.key -> 0))
    stage.apply(known, listingOf, version = 1)                                     // the first pass: nothing new to it
    asked shouldBe empty
    stage.apply(resolution, listingOf, version = 2).decisions.head.film shouldBe Some(913760)   // new: asked, then taken
    asked.flatten.toSet should contain (Question.Family(VoterFamily.RottenTomatoes, "title|Klondike"))
    val once = asked.size
    stage.apply(resolution, listingOf, version = 3)
    asked.size shouldBe once
  }

  // The staging screen sampled the stage's waits every 20 s: a wait begun and ended in between was never seen.
  it should "tell the staging screen each wait as it starts and ends, a new cluster's inline wait too, and nothing on a quiet apply" in {
    import services.identity.staging.StagingEvent
    val clock = new tools.MutableClock(java.time.Instant.parse("2026-10-24T12:00:00Z"))
    val told  = scala.collection.mutable.ArrayBuffer.empty[StagingEvent]
    var id    = ""
    def ofBare = told.toSeq.filter {
      case StagingEvent.Waiting(cluster, _, _) => cluster == id
      case StagingEvent.Settled(cluster, _)    => cluster == id
      case _                                   => false
    }
    var rtAnswered = false
    val rt = new Flipping(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike)), rtAnswered)
    val waits = new AgreementStage(agreeing() + (VoterFamily.RottenTomatoes -> rt), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(Some(913760)), new InMemoryAgreementVerdicts, clock = clock, staging = Some(told ++= _))
    id = waits.keyOf(waits.apply(resolution, listingOf, version = 1).decisions.head)   // RT unanswered: it waits
    ofBare.collect { case StagingEvent.Waiting(_, began, questions) =>
      (began, questions.contains(Question.Family(VoterFamily.RottenTomatoes, "title|Klondike")))
    } shouldBe Seq((clock.millis(), true))
    ofBare should have size 1
    told.clear()
    waits.apply(resolution, listingOf, version = 1)                                // quiet: no wait moved
    told shouldBe empty
    clock.advanceSeconds(30)
    rtAnswered = true
    waits.apply(resolution, listingOf, version = 2).decisions.head.film shouldBe Some(913760)
    ofBare shouldBe Seq(StagingEvent.Settled(id, clock.millis()))

    // a new cluster asked inline waits only inside the apply that meets it: told as a wait begun and ended there
    told.clear()
    var inlineAnswered = false
    val inlineRt = new Flipping(VoterFamily.RottenTomatoes, Map("klondike_2022" -> SourceRecord(klondike)), inlineAnswered)
    val inline = new AgreementStage(agreeing() + (VoterFamily.RottenTomatoes -> inlineRt), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(Some(913760)), new InMemoryAgreementVerdicts, clock = clock,
      askNow = (_, _) => { clock.advanceSeconds(2); inlineAnswered = true }, staging = Some(told ++= _))
    inline.apply(resolution.copy(decisions = resolution.decisions.drop(1), nodes = 1, familyOf = Map(matched.key -> 0)), listingOf, version = 1)
    val asked = clock.millis()
    val taken = inline.apply(resolution, listingOf, version = 2).decisions.head
    taken.film shouldBe Some(913760)
    id = inline.keyOf(taken)
    ofBare.map {
      case StagingEvent.Waiting(_, at, qs) => ("waiting", at, qs.contains(Question.Family(VoterFamily.RottenTomatoes, "title|Klondike")))
      case StagingEvent.Settled(_, at)     => ("settled", at, true)
      case other                           => fail(s"unexpected $other")
    } shouldBe Seq(("waiting", asked, true), ("settled", asked + 2000, true))
  }

  it should "ask nothing inline of a cluster met on its first pass, which meets every cluster" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val stage = new AgreementStage(agreeing(rtAnswered = false), NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, askNow = (q, _) => asked += q)
    stage.apply(resolution, listingOf, version = 1)
    stage.apply(resolution, listingOf, version = 2)
    asked shouldBe empty
    stage.wanted should contain (Question.Family(VoterFamily.RottenTomatoes, "title|Klondike"))   // the queue's, as before
  }

  it should "stay as the model left it while a family has not answered, and name the question" in {
    val stage = new AgreementStage(agreeing(rtAnswered = false), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned)
    val decision = stage.apply(resolution, listingOf, version = 1).decisions.head
    decision shouldBe resolution.decisions.head
    stage.wanted should contain (Question.Family(VoterFamily.RottenTomatoes, "title|Klondike"))
    // what the drift check counts unsettled: the model's take waits on no gap of its own, yet on the family's question
    val waits = stage.waitingOn(decision)
    (waits.modelGaps, waits.settled) shouldBe ((0, false))
    waits.questions should contain (services.identity.Question.Family(VoterFamily.RottenTomatoes, "title|Klondike"))
  }

  // The one settle rule holds for TMDB's find of an agreed IMDb id too: attempted and not answered, it is read as TMDB
  // holding none — the agreed film taken on its IMDb id — and asked still; its answer, filed, takes TMDB's film.
  it should "take an agreed film on its IMDb id once TMDB's find of it was attempted, and TMDB's film once the find answers" in {
    var found: Answer[Option[Int]] = Answer.Unknown
    var attempted = false
    val stage = new AgreementStage(agreeing(), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => found,
      new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, attempted = { case Question.Find(_) => attempted; case _ => false })
    stage.apply(resolution, listingOf, version = 1).decisions.head shouldBe resolution.decisions.head   // the find never asked: held
    attempted = true
    val fallback = stage.apply(resolution, listingOf, version = 1).decisions.head   // attempted, nothing filed: decided without it
    (fallback.film, fallback.fallback.map(_.id)) shouldBe ((None, Some("tt16315948")))
    stage.wanted should contain (Question.Find("tt16315948"))                     // asked still
    found = Answer.Known(Some(913760))
    stage.apply(resolution, listingOf, version = 2).decisions.head.film shouldBe Some(913760)
  }
}
