package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures

/** A stored agreement verdict reads back as it was kept — the answers it read and the film agreed on, or none. */
class AgreementVerdictsSpec extends AnyFlatSpec with Matchers {
  "a stored verdict" should "read back with its reads and its agreed film's families, ids, cross-ids, title and year" in {
    val film    = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), None, None, None, None)
    val agreed  = StoredVerdict("k", 42L, Map("imdb|title|Klondike" -> 7L, "rt|record|klondike_2022" -> -3L),
      Some(AgreedFilm(Set(VoterFamily.Imdb, VoterFamily.RottenTomatoes, VoterFamily.Metacritic), SourceRecord(film, Map("imdb" -> "tt16315948")),
        Map(VoterFamily.Imdb -> "tt16315948", VoterFamily.RottenTomatoes -> "klondike_2022", VoterFamily.Metacritic -> "klondike"))))
    AgreementVerdicts.decode(AgreementVerdicts.encode(agreed)) shouldBe agreed
    val none = StoredVerdict("k", 42L, Map("imdb|title|Klondike" -> 7L), None)
    AgreementVerdicts.decode(AgreementVerdicts.encode(none)) shouldBe none
  }
}
