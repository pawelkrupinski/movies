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

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map("%02X".format(_)).mkString

  "TlsTrust" should "bundle the Certum Trusted Root CA the JDK omits" in {
    val certs = TlsTrust.bundledCerts
    certs should have size 1
    val c = certs.head
    c.getSubjectX500Principal.getName should include ("Certum Trusted Root CA")
    sha256(c.getEncoded) shouldBe CertumRootSha256
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
