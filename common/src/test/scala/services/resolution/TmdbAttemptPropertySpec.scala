package services.resolution

import org.scalacheck.Gen
import services.IdentityPropertySpec
import services.IdentityGenerators._

/** A no-match's fingerprint identifies the INPUTS: the same terms in any order
 *  or multiplicity fingerprint alike, and a new director is a new input. */
class TmdbAttemptPropertySpec extends IdentityPropertySpec {

  private val genSearchTerms: Gen[Seq[String]] =
    Gen.choose(0, 4).flatMap(Gen.listOfN(_, Gen.oneOf("Dune", "Diuna", "Tangled", "Zaplątani")))

  "TmdbAttempt.fingerprint" should "not depend on the order or multiplicity of the search terms" in {
    forAll(genFilmEvidence, withPermutation(genSearchTerms), Gen.choose(0, 4)) { case (evidence, (terms, permuted), repeat) =>
      TmdbAttempt.fingerprint(evidence, permuted ++ terms.take(repeat)) shouldBe TmdbAttempt.fingerprint(evidence, terms)
    }
  }

  it should "change when a director the cinemas had not named is added" in {
    forAll(genFilmEvidence, genSearchTerms, genName) { (evidence, terms, director) =>
      whenever(!evidence.directors.contains(director)) {
        TmdbAttempt.fingerprint(evidence.withDirectors(Seq(director)), terms) should not be
          TmdbAttempt.fingerprint(evidence, terms)
      }
    }
  }
}
