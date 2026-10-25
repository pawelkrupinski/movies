package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Dependency, IdentityMeasures, Question}

/** A stored agreement verdict reads back as it was kept — the answers it read and the film agreed on, or none. */
class AgreementVerdictsSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {
  private def read(id: String): Dependency = Dependency.parse(id)

  "a stored verdict" should "read back with its reads and its agreed film's families, leaning families, ids, cross-ids, title and year" in {
    val film    = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), None, None, None, None)
    val agreed  = StoredVerdict("k", 42L, Map(read("imdb|title|Klondike") -> 7L, read("rt|record|klondike_2022") -> -3L),
      Some(AgreedFilm(Set(VoterFamily.Imdb, VoterFamily.RottenTomatoes, VoterFamily.Metacritic), SourceRecord(film, Map("imdb" -> "tt16315948")),
        Map(VoterFamily.Imdb -> "tt16315948", VoterFamily.RottenTomatoes -> "klondike_2022", VoterFamily.Metacritic -> "klondike"))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(agreed)) shouldBe agreed
    val leaned = agreed.copy(agreed = agreed.agreed.map(_.copy(families = Set(VoterFamily.Imdb, VoterFamily.RottenTomatoes), leaning = Set(VoterFamily.Wiki), corroborated = Set(Agreement.ListingFacts))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(leaned)) shouldBe leaned
    val none = StoredVerdict("k", 42L, Map(read("imdb|title|Klondike") -> 7L), None)
    AgreementVerdicts.decode(AgreementVerdicts.encode(none)) shouldBe none
    val filled = none.copy(filled = Some(StoredFill("venues.current", None, Some("tt46658626"), "filled by venues.current: …")))
    AgreementVerdicts.decode(AgreementVerdicts.encode(filled)) shouldBe filled
    val refused = none.copy(refused = Some("no agreement on 'When Pigs Fly' (1993): the venue runs it 80′, more than 10′ short of its 94′"))
    AgreementVerdicts.decode(AgreementVerdicts.encode(refused)) shouldBe refused
  }

  it should "read back its agreed film's every fact, which the take reads again after a restart" in {
    // DE "Internationales Gitarrenfestival Tübingen - Eröffnungskonzert - Raíces": the families agree on 'Roots' (1954), whose
    // ORIGINAL title is the billing's last piece. Kept as its title and year alone, the restarted stage no longer saw the
    // piece and took the film the whole resolve refuses (identity-slice --drift, recording 573)
    val film = IdentityMeasures.Film("Roots", Some("Raíces"), Seq("Raices", "Wurzeln"), Some(1954), Some(83), Some(Seq("Benito Alazraki")),
      Some(Seq("MX")), Some(1.5), 47395, Some(java.time.LocalDate.of(1955, 4, 21)), Some("MXUS"), Seq(85, 80), Some("MX1955P"), 123,
      Some("US=PG"), Some("es"), 18, services.identity.FilmCast.ofPairs(Seq(Seq(1, 2), Seq(3))))
    val agreed = StoredVerdict("r", 7L, Map(read("imdb|title|Raíces") -> 1L),
      Some(AgreedFilm(Set(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb), SourceRecord(film, Map("imdb" -> "tt0047395", "tmdb" -> "122769")),
        Map(VoterFamily.Imdb -> "tt0047395", VoterFamily.Wiki -> "Q7366720", VoterFamily.Filmweb -> "100"))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(agreed)) shouldBe agreed
  }

  // HardClusterConvergenceIntegrationSpec on land-all (2026-10-25): a receipt naming a TMDB search by its query's sort
  // key ("a\u0000Pat Garrett & Billy the Kid") was no field name Mongo writes — a NUL ends a BSON cstring
  it should "write to Mongo with a receipt naming TMDB's searches, and read them again" in {
    val normalizer = services.movies.SingleCountryNormalizer.titleNormalizer
    val tour     = IdentityMeasures.Film("Seventeen World Tour (New_)", None, Nil, Some(2026), None, None, None, None)
    val families = Map[VoterFamily, FamilyAnswers](VoterFamily.Imdb -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt46658626" -> SourceRecord(tour, Map("imdb" -> "tt46658626"))))) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    val listings = Seq(models.KinoMuza, models.Multikino, models.KinoPalacowe).map(services.identity.FilmTable.listing(_, "SEVENTEEN World Tour 'NEW_'"))
    val model = services.identity.Resolution(Seq(services.identity.ResolverDecision(listings.map(_.key), None, 0.2,
      services.identity.ResolverDecision.Basis.BelowThreshold, Nil)()), listings.size, listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    val stored = new InMemoryAgreementVerdicts
    new AgreementStage(families, NoVenueDetails, normalizer, services.identity.IdentityCalibration.resolver, tmdbOf = _ => services.identity.Answer.Known(None),
      stored, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(NoVenueDetails),
      rules = services.identity.UnifiedRules("t", services.identity.UnifiedEvidence.Guards, Seq("venues.current")))
      .apply(model, listings.map(l => l.key -> l).toMap.get, version = 1)
    val verdict = stored.all().loneElement
    verdict.reads.keys.exists { case Dependency.Lookup(Dependency.Via.Tmdb, _, Question.Candidate(_)) => true; case _ => false } shouldBe true
    // as the driver writes it: every field name a BSON cstring
    val written = new org.bson.RawBsonDocument(AgreementVerdicts.encode(verdict), new org.bson.codecs.BsonDocumentCodec())
    AgreementVerdicts.decode(written.toBsonDocument) shouldBe verdict
  }

  "a stored correction of a model take" should "read back with its film, its line, its receipt and whether it waits" in {
    val reads     = Map(read("imdb|title|Klondike") -> 7L, Dependency.Asked(Question.Find("tt1")) -> 0L,
      Dependency.Asked(Question.Poster(AgreementStage.PosterQuestion.Venue("https://v/p.jpg"))) -> 0L)
    val corrected = StoredVerdict(AgreementStage.correctionId("k"), 42L, reads, None, "r",
      correction = Some(StoredCorrection(Some(30497), Some("corrected from … by families and poster"), waiting = false)))
    AgreementVerdicts.decode(AgreementVerdicts.encode(corrected)) shouldBe corrected
    val standing  = corrected.copy(correction = Some(StoredCorrection(None, None, waiting = true)))
    AgreementVerdicts.decode(AgreementVerdicts.encode(standing)) shouldBe standing
  }

  // The NUL receipt passed every unit spec because the in-memory store kept the verdict it was handed: it writes as Mongo
  // does. A receipt names its dependencies as values now, never field names: one naming a NUL writes and reads back.
  "the in-memory verdicts" should "write a receipt naming a NUL, and read it back" in {
    val nul = StoredVerdict("k", 42L, Map(Dependency.Unparsed("tmdb|candidates|a\u0000Pat Garrett & Billy the Kid") -> 7L), None)
    val kept = new InMemoryAgreementVerdicts()
    kept.replace(Set.empty, Seq(nul))
    kept.all() shouldBe Seq(nul)
  }

  "a receipt" should "name every dependency by the one codec, and read each back as it was" in {
    val key = services.movies.ListingKey.Published("Kino Muza", "Lalka | x", Some(2025), Seq("Maciej Kawalski"))
    val every = Seq[Dependency](
      Dependency.Answer(Question.Family(VoterFamily.Filmweb, "title|Lalka")),
      Dependency.Answer(Question.Family(VoterFamily.RottenTomatoes, "record|lalka|x")),
      Dependency.Attempted(Question.Record(913760)),
      Dependency.Asked(Question.Find("tt0064570")),
      Dependency.Asked(Question.Catalogue(services.identity.CatalogueQuestion.Page("https://a/b|c"))),
      Dependency.Lookup(Dependency.Via.Venues, Dependency.Aspect.Answer, Question.VenuePage(key)),
      Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.Candidate(services.identity.CandidateQuery.Title("Pat Garrett & Billy the Kid"))),
      Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Release, Question.FilmRecord(7)),
      Dependency.Lookup(Dependency.Via.Venues, Dependency.Aspect.Answer, Question.FilmRecord(7)),
      Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Cast, Question.FilmRecord(7)),
      Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Overview, Question.FilmRecord(7)),
      Dependency.Year)
    every.foreach(dependency => Dependency.parse(Dependency.encode(dependency)) shouldBe dependency)
    every.map(Dependency.encode).distinct should have size every.size.toLong
    val receipt = every.zipWithIndex.map { case (dependency, i) => dependency -> i.toLong }.toMap
    val written = new org.bson.RawBsonDocument(new org.bson.BsonDocument("reads", AgreementVerdicts.receiptOf(receipt)), new org.bson.codecs.BsonDocumentCodec())
    AgreementVerdicts.receipt(written.toBsonDocument.get("reads")) shouldBe receipt
  }

  it should "read one stored before it was typed, its field names the dependencies' ids, an id it no longer names never standing" in {
    val legacy = new org.bson.BsonDocument("imdb|title|Klondike", org.bson.BsonInt64(7L)).append("clock", org.bson.BsonInt64(1L))
      .append("tmdb|film|9", org.bson.BsonInt64(2L)).append("venues|detail|0", org.bson.BsonInt64(3L))
    AgreementVerdicts.receipt(legacy) shouldBe Map(read("imdb|title|Klondike") -> 7L, Dependency.Year -> 1L,
      Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.FilmRecord(9)) -> 2L, Dependency.Unparsed("venues|detail|0") -> 3L)
  }

  "a dependency's filing" should "be the id its answer is filed under on the answers' changes" in {
    read("imdb|title|Klondike").filing shouldBe "imdb|title|Klondike"
    Dependency.Lookup(Dependency.Via.Venues, Dependency.Aspect.Cast, Question.FilmRecord(7)).filing shouldBe AgreementStage.recordReadId(7)
    Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.Candidate(services.identity.CandidateQuery.Title("x"))).filing shouldBe Dependency.QueryFiled
    Dependency.Asked(Question.Find("tt1")).filing shouldBe Question.Find("tt1").id.get
  }
}
