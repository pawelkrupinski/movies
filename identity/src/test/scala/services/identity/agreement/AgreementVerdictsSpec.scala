package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures

/** A stored agreement verdict reads back as it was kept — the answers it read and the film agreed on, or none. */
class AgreementVerdictsSpec extends AnyFlatSpec with Matchers {
  "a stored verdict" should "read back with its reads and its agreed film's families, leaning families, ids, cross-ids, title and year" in {
    val film    = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), None, None, None, None)
    val agreed  = StoredVerdict("k", 42L, Map("imdb|title|Klondike" -> 7L, "rt|record|klondike_2022" -> -3L),
      Some(AgreedFilm(Set(VoterFamily.Imdb, VoterFamily.RottenTomatoes, VoterFamily.Metacritic), SourceRecord(film, Map("imdb" -> "tt16315948")),
        Map(VoterFamily.Imdb -> "tt16315948", VoterFamily.RottenTomatoes -> "klondike_2022", VoterFamily.Metacritic -> "klondike"))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(agreed)) shouldBe agreed
    val leaned = agreed.copy(agreed = agreed.agreed.map(_.copy(families = Set(VoterFamily.Imdb, VoterFamily.RottenTomatoes), leaning = Set(VoterFamily.Wiki), corroborated = Set(Agreement.ListingFacts))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(leaned)) shouldBe leaned
    val none = StoredVerdict("k", 42L, Map("imdb|title|Klondike" -> 7L), None)
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
    val agreed = StoredVerdict("r", 7L, Map("imdb|title|Raíces" -> 1L),
      Some(AgreedFilm(Set(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb), SourceRecord(film, Map("imdb" -> "tt0047395", "tmdb" -> "122769")),
        Map(VoterFamily.Imdb -> "tt0047395", VoterFamily.Wiki -> "Q7366720", VoterFamily.Filmweb -> "100"))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(agreed)) shouldBe agreed
  }

  "a stored correction of a model take" should "read back with its film, its line, its reads' digests and whether it waits" in {
    val reads     = scala.collection.immutable.ArraySeq(-5L, 3L, 9L)
    val corrected = StoredVerdict(AgreementStage.correctionId("k"), 42L, Map.empty, None, "r",
      correction = Some(StoredCorrection(Some(30497), Some("corrected from … by families and poster"), reads, waiting = false)))
    AgreementVerdicts.decode(AgreementVerdicts.encode(corrected)) shouldBe corrected
    val standing  = corrected.copy(correction = Some(StoredCorrection(None, None, reads, waiting = true)))
    AgreementVerdicts.decode(AgreementVerdicts.encode(standing)) shouldBe standing
  }
}
