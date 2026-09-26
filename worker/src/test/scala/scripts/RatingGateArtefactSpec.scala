package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityCalibration

import java.security.MessageDigest

/** The live rating gate reads a PINNED artefact that no resolver refit can move: a refit writes the
 *  resolver's artefact only, and the gate's bytes are the ones it was measured and switched on with. */
class RatingGateArtefactSpec extends AnyFlatSpec with Matchers {

  private def sha256(path: String): String = {
    val in = getClass.getClassLoader.getResourceAsStream(path)
    try MessageDigest.getInstance("SHA-256").digest(in.readAllBytes()).map("%02x".format(_)).mkString finally in.close()
  }

  "a resolver refit" should "write the resolver's artefact, never the rating gate's" in {
    IdentityCalibrate.ResolverArtefact.getFileName.toString shouldBe IdentityCalibration.ResolverResourcePath
    IdentityCalibration.RatingGateResourcePath should not be IdentityCalibration.ResolverResourcePath
  }

  "the rating gate's artefact" should "be the one the gate was measured and switched on with (DE, ES)" in {
    // Changing it is a deliberate copy after `scripts.IdentityGateImpact --weights <candidate>`:
    // update this pin in the same commit, with the measured false hides per country.
    IdentityCalibration.ratingGate.version shouldBe "calibration-2026-09-26"
    sha256(IdentityCalibration.RatingGateResourcePath) shouldBe "e47c8ee07221ebc071fcc3094d5c23ad0d60f0a0b95228b85a038e5b672da26e"
  }
}
