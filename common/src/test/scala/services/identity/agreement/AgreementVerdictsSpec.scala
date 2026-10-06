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
