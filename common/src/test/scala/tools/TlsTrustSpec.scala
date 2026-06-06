package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.MessageDigest

class TlsTrustSpec extends AnyFlatSpec with Matchers {

  // The published SHA-256 fingerprint of Certum Trusted Root CA. Asserting it
  // proves the EXACT right root is bundled (not some other cert that happens to
  // parse), independent of whatever the running JDK's cacerts contains.
  private val CertumRootSha256 =
    "FE7696573855773E37A95E7AD4D9CC96C30157C15D31765BA9B15704E1AE78FD"

  // SHA-256 of the `home pl DV TLS G2 R35 CA` intermediate — artmuseum.pl's leaf
  // issuer, bundled so its broken chain validates without an AIA network fetch.
  private val HomePlIntermediateSha256 =
    "CA1A6E0BC3B1C2ED099D42D7030577578D13F63B4B9680A6BAA274DF17C9DE58"

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map("%02X".format(_)).mkString

  private def bySubject(fragment: String) =
    TlsTrust.bundledCerts.find(_.getSubjectX500Principal.getName.contains(fragment))

  "TlsTrust" should "bundle the Certum Trusted Root CA the JDK omits" in {
    val c = bySubject("Certum Trusted Root CA")
      .getOrElse(fail("Certum root not bundled"))
    sha256(c.getEncoded) shouldBe CertumRootSha256
  }

  it should "bundle the home.pl intermediate so artmuseum.pl's broken chain needs no AIA fetch" in {
    val root = bySubject("Certum Trusted Root CA")
      .getOrElse(fail("Certum root not bundled"))
    val intermediate = bySubject("home pl DV TLS G2 R35 CA")
      .getOrElse(fail("home.pl intermediate not bundled"))

    sha256(intermediate.getEncoded) shouldBe HomePlIntermediateSha256
    // The load-bearing property: the bundled intermediate is signed by the
    // bundled root, so leaf -> intermediate -> root is a complete OFFLINE path.
    // verify() throws on a bad signature; passing proves the chain link holds.
    noException should be thrownBy intermediate.verify(root.getPublicKey)
  }

  it should "expose a trust manager that accepts the Certum root AND keeps the JDK defaults" in {
    val accepted = TlsTrust.augmentedTrustManager.getAcceptedIssuers.toSeq
    val subjects = accepted.map(_.getSubjectX500Principal.getName)
    subjects.exists(_.contains("Certum Trusted Root CA")) shouldBe true
    // Still the full default store underneath, not a replacement of it.
    subjects.size should be > 50
  }

  it should "enable AIA intermediate fetching for servers that ship a broken chain" in {
    TlsTrust // force object init
    System.getProperty("com.sun.security.enableAIAcaIssuers") shouldBe "true"
  }
}
