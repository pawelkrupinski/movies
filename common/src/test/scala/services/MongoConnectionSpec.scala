package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The boot-failure contract: with `required = true` (production), a missing
 * or unreachable Mongo must throw out of construction so the app refuses to
 * start. With `required = false` (dev / tests) the same conditions disable
 * the connection and `database` is `None`.
 *
 * Both failure cases use inputs that fail synchronously (absent URI, or a
 * malformed connection string the driver rejects on parse), so no test here
 * touches the network or waits on the connect probe.
 */
class MongoConnectionSpec extends AnyFlatSpec with Matchers {

  // `mongodb://`-less string → the driver rejects it on parse, before any IO.
  private val MalformedUri = "not-a-mongo-uri"

  "MongoConnection with required = true" should "throw when MONGODB_URI is absent" in {
    val ex = intercept[IllegalStateException] {
      new MongoConnection(uri = None, dbName = "kinowo", required = true)
    }
    ex.getMessage should include ("required")
  }

  it should "throw when the connection can't be established" in {
    val ex = intercept[IllegalStateException] {
      new MongoConnection(uri = Some(MalformedUri), dbName = "kinowo", required = true)
    }
    ex.getMessage should include ("required")
  }

  "MongoConnection with required = false" should "disable (database None) when MONGODB_URI is absent" in {
    val conn = new MongoConnection(uri = None, dbName = "kinowo", required = false)
    conn.database shouldBe None
    conn.close()  // idempotent no-op when nothing was opened
  }

  it should "disable (database None) when the connection can't be established" in {
    val conn = new MongoConnection(uri = Some(MalformedUri), dbName = "kinowo", required = false)
    conn.database shouldBe None
    conn.close()
  }

  // fromUri builds a SECOND connection from an explicit URI (the /debug local
  // read-mirror, MONGODB_MOVIES_MIRROR_URI) rather than MONGODB_URI. Wiring
  // builds it with required = false so a bad/unreachable mirror degrades to the
  // prod connection (database None → fall back) instead of blocking boot — only
  // /debug needs it.
  "MongoConnection.fromUri with required = false" should "disable (database None) on an unusable URI instead of throwing" in {
    val conn = MongoConnection.fromUri(MalformedUri, required = false)
    conn.database shouldBe None
    conn.close()
  }

  "MongoConnection.fromUri with required = true" should "throw on an unusable URI" in {
    val ex = intercept[IllegalStateException] {
      MongoConnection.fromUri(MalformedUri, required = true)
    }
    ex.getMessage should include ("required")
  }

  // Wire compression: the payload over the wire is uncompressed BSON, so on a
  // slow link (the local flyctl tunnel, or the prod boot hydrate) transfer
  // bytes dominate. We default to zlib — built into the JDK, no extra
  // dependency — unless the URI explicitly chose its own compressors.
  private val ValidUri = "mongodb://user:pass@127.0.0.1:27017/kinowo?authSource=kinowo"

  "MongoConnection.clientSettings" should "default to zlib wire compression when the URI names none" in {
    val names = MongoConnection.clientSettings(ValidUri).getCompressorList.asScala.map(_.getName)
    names should contain ("zlib")
  }

  it should "respect compressors the URI specifies rather than forcing zlib" in {
    val names = MongoConnection
      .clientSettings(ValidUri + "&compressors=snappy")
      .getCompressorList.asScala.map(_.getName)
    names should contain ("snappy")
    names should not contain "zlib"
  }

  // The mode/opt-out → required policy used by Wiring.
  "MongoConnection.isRequired" should "require Mongo outside test mode by default" in {
    MongoConnection.isRequired(testMode = false, optedOut = false) shouldBe true
  }

  it should "never require Mongo in test mode" in {
    MongoConnection.isRequired(testMode = true, optedOut = false) shouldBe false
  }

  it should "let a local opt-out disable the requirement outside tests" in {
    MongoConnection.isRequired(testMode = false, optedOut = true) shouldBe false
  }

  // Boot-probe timeout. The old value was a hard-coded 10s; a slow/recovering
  // Mongo blew past it, crash-looped the boot, and Fly stopped the web machine
  // (2026-06-06). It's now `DefaultProbeTimeout` (30s), overridable via
  // `MONGODB_PROBE_TIMEOUT_SECONDS` and parsed by `parseProbeTimeout`.
  "MongoConnection.DefaultProbeTimeout" should "stay above the old 10s ceiling that crash-looped the boot" in {
    MongoConnection.DefaultProbeTimeout.toSeconds should be > 10L
  }

  "MongoConnection.parseProbeTimeout" should "default when the override is absent" in {
    MongoConnection.parseProbeTimeout(None) shouldBe MongoConnection.DefaultProbeTimeout
  }

  it should "honour a positive second count from the override" in {
    MongoConnection.parseProbeTimeout(Some("45")) shouldBe 45.seconds
  }

  it should "fall back to the default on non-numeric input" in {
    MongoConnection.parseProbeTimeout(Some("soon")) shouldBe MongoConnection.DefaultProbeTimeout
  }

  it should "fall back to the default on a non-positive count" in {
    MongoConnection.parseProbeTimeout(Some("0"))  shouldBe MongoConnection.DefaultProbeTimeout
    MongoConnection.parseProbeTimeout(Some("-5")) shouldBe MongoConnection.DefaultProbeTimeout
  }
}
