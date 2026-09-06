package services.resolution

import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators._
import services.movies.SamePerson

/** `Verdict.of` weighs SETS of evidence: order-free, silent when both sides are
 *  silent, and settled by any credit the two sides share. */
class VerdictPropertySpec extends IdentityPropertySpec {

  "Verdict.of" should "not depend on the order of the directors, runtimes or crew" in {
    forAll(genFilmEvidence, genCandidate, Gen.long) { (evidence, candidate, seed) =>
      val permutedEvidence  = evidence.copy(
        directors = permute(seed, evidence.directors),
        runtimes  = permute(seed + 1, evidence.runtimes))
      val permutedCandidate = candidate.copy(crew = permute(seed + 2, candidate.crew))
      Verdict.of(permutedEvidence, permutedCandidate) shouldBe Verdict.of(evidence, candidate)
    }
  }

  it should "never reject when neither side published a credit or a runtime" in {
    forAll(genFilmEvidence, genCandidate) { (evidence, candidate) =>
      val silentEvidence  = evidence.copy(directors = Nil, runtimes = Nil)
      val silentCandidate = candidate.copy(crew = Nil, runtime = None)
      Verdict.of(silentEvidence, silentCandidate) shouldBe Verdict.Insufficient
    }
  }

  it should "accept on crew whenever a cinema's credit appears verbatim on the candidate's crew" in {
    // Verbatim AND readable: a credit the fold cannot tokenise (a CJK name) is
    // compared as nothing, by design — see the example below.
    forAll(genFilmEvidence, genCandidate, genLatinName) { (evidence, candidate, name) =>
      Verdict.of(evidence.withDirectors(Seq(name)), candidate.copy(crew = candidate.crew :+ name)) shouldBe
        Verdict.Accept(Support.Crew)
    }
  }

  it should "abstain, not accept, on a verbatim credit the fold cannot read" in {
    val unreadable = "王家衛"
    SamePerson.tokens(unreadable) shouldBe empty
    Verdict.of(FilmEvidence.empty.withDirectors(Seq(unreadable)), Candidate(1, crew = Seq(unreadable))) shouldBe
      Verdict.Insufficient
  }
}
