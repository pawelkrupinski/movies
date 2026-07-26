package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The version-gate contract. These assertions are the specification the Kotlin and
 * Swift gates are written against, so they are deliberately about BEHAVIOUR ("1.10
 * is newer than 1.9") rather than the current config's values — which change every
 * time support is dropped.
 */
class ClientSupportSpec extends AnyFlatSpec with Matchers {

  "ClientVersion.isAtLeast" should "compare component-wise, not as strings" in {
    // The one that a naive string compare gets backwards, and the reason this
    // exists at all rather than `version >= minimum`.
    ClientVersion.isAtLeast("1.10", "1.9") shouldBe Some(true)
    ClientVersion.isAtLeast("1.9", "1.10") shouldBe Some(false)
    ClientVersion.isAtLeast("2.0", "10.0") shouldBe Some(false)
  }

  it should "treat a missing component as zero" in {
    // Android ships "1.0" and iOS "1.0.0" for the same release; they must compare
    // equal or one store's users get gated and the other's don't.
    ClientVersion.isAtLeast("1.0", "1.0.0") shouldBe Some(true)
    ClientVersion.isAtLeast("1.0.0", "1.0") shouldBe Some(true)
    ClientVersion.isAtLeast("1.0.1", "1.0") shouldBe Some(true)
    ClientVersion.isAtLeast("1.0", "1.0.1") shouldBe Some(false)
  }

  it should "accept its own minimum" in {
    // "minimum" is inclusive: the version named is still supported.
    ClientVersion.isAtLeast("1.4.0", "1.4.0") shouldBe Some(true)
  }

  it should "reject anything non-numeric rather than guessing" in {
    ClientVersion.parse("1.2.beta") shouldBe None
    ClientVersion.parse("") shouldBe None
    ClientVersion.parse("1..2") shouldBe None
    ClientVersion.isAtLeast("1.2.beta", "1.0") shouldBe None
  }

  "a gated platform" should "turn away versions below the minimum" in {
    val gated = ClientSupport.Platform(Some("1.4.0"), Some("https://example.test/app"))
    ClientVersion.isSupported("1.3.9", gated) shouldBe false
    ClientVersion.isSupported("1.4.0", gated) shouldBe true
    ClientVersion.isSupported("2.0", gated)   shouldBe true
  }

  it should "let an unparseable client version through" in {
    // Failing OPEN is the deliberate choice: a malformed version is our bug or a
    // bad build, and locking someone out of a working app over it is the worse of
    // the two failures.
    val gated = ClientSupport.Platform(Some("1.4.0"), Some("https://example.test/app"))
    ClientVersion.isSupported("nightly", gated) shouldBe true
  }

  "a platform missing either half" should "gate nobody" in {
    // A minimum with nowhere to send people strands them in a dead app; a store URL
    // with no minimum gates nobody. Both are "no rule".
    val noUrl = ClientSupport.Platform(Some("9.9.9"), None)
    val noMin = ClientSupport.Platform(None, Some("https://example.test/app"))
    noUrl.isGated shouldBe false
    noMin.isGated shouldBe false
    ClientVersion.isSupported("0.1", noUrl) shouldBe true
    ClientVersion.isSupported("0.1", noMin) shouldBe true
  }

  "the published payload" should "name both platforms even when one has no rule" in {
    // A client must be able to tell "no rule" from "I failed to parse this", since
    // the answer decides whether its app is usable.
    ClientSupport.json should include(""""android":""")
    ClientSupport.json should include(""""ios":""")
    ClientSupport.json should include("minimumVersion")
    ClientSupport.json should include("storeUrl")
  }

  it should "be stable, so the ETag can identify it" in {
    ClientSupport.json shouldBe ClientSupport.json
    ClientSupport.etag shouldBe ClientSupport.etag
    ClientSupport.etag should startWith("\"")
  }

  it should "keep the configured Android rule usable" in {
    // Not asserting the VALUE (it moves every time support is dropped) — asserting
    // that whatever is configured is well-formed enough to gate with.
    ClientSupport.android.minimumVersion.foreach(v => ClientVersion.parse(v) should not be None)
    ClientSupport.android.storeUrl.foreach(_ should startWith("https://"))
  }
}
