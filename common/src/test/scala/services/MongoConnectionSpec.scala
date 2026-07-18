package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit.MILLISECONDS
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
    val exception = intercept[IllegalStateException] {
      new MongoConnection(uri = None, dbName = "kinowo", required = true)
    }
    exception.getMessage should include ("required")
  }

  it should "throw when the connection can't be established" in {
    val exception = intercept[IllegalStateException] {
      new MongoConnection(uri = Some(MalformedUri), dbName = "kinowo", required = true)
    }
    exception.getMessage should include ("required")
  }

  // A MISCONFIGURED Mongo and an UNREACHABLE one are different failures and must
  // be handled differently. Aborting boot is right for the first (nothing will
  // ever fix itself) and actively harmful for the second: the 2026-07-18 outage
  // was four web machines crash-looping against an overloaded Mongo, each restart
  // re-running the heavy boot hydrates and keeping the cluster too busy to
  // recover. Refusing to start is what made a slow dependency a total outage —
  // the process never binds port 9000, so nothing serves, and the loop is
  // self-sustaining. Binding the port and serving degraded lets the cluster
  // recover and the retry reconnect.
  "the boot-failure classifier" should "treat a server-selection timeout as transient" in {
    val timeout = new com.mongodb.MongoTimeoutException("Timed out while waiting for a server")
    MongoConnection.isTransient(timeout).shouldBe(true)
  }

  it should "treat a socket read timeout as transient" in {
    val socketTimeout = new com.mongodb.MongoSocketReadTimeoutException(
      "Timeout while receiving message",
      new com.mongodb.ServerAddress("kinowo-mongo.internal", 27017),
      new java.io.IOException("read timed out"))
    MongoConnection.isTransient(socketTimeout).shouldBe(true)
  }

  it should "treat a malformed connection string as PERMANENT (nothing will fix it)" in {
    MongoConnection.isTransient(new IllegalArgumentException("not a mongo uri")).shouldBe(false)
  }

  it should "treat an authentication failure as PERMANENT" in {
    val security = new com.mongodb.MongoSecurityException(
      com.mongodb.MongoCredential.createCredential("u", "kinowo", "p".toCharArray),
      "auth failed",
      new java.io.IOException("bad credentials"))
    MongoConnection.isTransient(security).shouldBe(false)
  }

  // The behavioural gate: an unreachable-but-well-formed Mongo must NOT abort boot
  // even when `required`. Port 1 is reserved and refuses/blackholes immediately, so
  // the probe fails fast without touching a real cluster.
  "MongoConnection with required = true against an UNREACHABLE server" should
    "start degraded rather than refuse to boot" in {
    val connection = new MongoConnection(
      uri                    = Some("mongodb://127.0.0.1:1/?connectTimeoutMS=150&socketTimeoutMS=150"),
      dbName                 = "kinowo",
      required               = true,
      probeTimeout           = 3.seconds,
      serverSelectionTimeout = Some(200.millis))
    withClue("an unreachable Mongo must leave the app bootable (port bound, degraded) — not crash-loop: ") {
      connection.database shouldBe None
    }
    connection.close()
  }

  "MongoConnection with required = false" should "disable (database None) when MONGODB_URI is absent" in {
    val connection = new MongoConnection(uri = None, dbName = "kinowo", required = false)
    connection.database shouldBe None
    connection.close()  // idempotent no-op when nothing was opened
  }

  it should "disable (database None) when the connection can't be established" in {
    val connection = new MongoConnection(uri = Some(MalformedUri), dbName = "kinowo", required = false)
    connection.database shouldBe None
    connection.close()
  }

  // fromUri builds a SECOND connection from an explicit URI (the /debug local
  // read-mirror, MONGODB_MOVIES_MIRROR_URI) rather than MONGODB_URI. Wiring
  // builds it with required = false so a bad/unreachable mirror degrades to the
  // prod connection (database None → fall back) instead of blocking boot — only
  // /debug needs it.
  "MongoConnection.fromUri with required = false" should "disable (database None) on an unusable URI instead of throwing" in {
    val connection = MongoConnection.fromUri(MalformedUri, required = false)
    connection.database shouldBe None
    connection.close()
  }

  "MongoConnection.fromUri with required = true" should "throw on an unusable URI" in {
    val exception = intercept[IllegalStateException] {
      MongoConnection.fromUri(MalformedUri, required = true)
    }
    exception.getMessage should include ("required")
  }

  // The mirror's database comes from its URI's own path, NOT MONGODB_DB — so the
  // /debug mirror (`…/kinowo_prod_mirror`) can live in a different database than
  // the app's working db (used by the prod connection).
  "MongoConnection.databaseFromUri" should "take the database from the URI path" in {
    MongoConnection.databaseFromUri(
      "mongodb://127.0.0.1:28017/kinowo_prod_mirror?directConnection=true") shouldBe "kinowo_prod_mirror"
  }

  it should "fall back to MONGODB_DB (then kinowo) when the URI names no database" in {
    MongoConnection.databaseFromUri(
      "mongodb://127.0.0.1:28017/?directConnection=true") shouldBe tools.Env.get("MONGODB_DB").getOrElse("kinowo")
  }

  // Wire compression earns its CPU only on a SLOW link (the local flyctl proxy
  // tunnel / a loopback mirror), where transfer bytes dominate. Over a direct 6PN
  // link (prod worker/web → `*.internal` mongo) the hop is fast + already
  // encrypted, so zlib would only burn CPU on the driver's async-IO threads for
  // bandwidth we don't need. So: zlib for loopback, none for a remote host, and a
  // URI-named `compressors=` always wins.
  private val ValidUri  = "mongodb://user:pass@127.0.0.1:27017/kinowo?authSource=kinowo"
  private val RemoteUri = "mongodb://user:pass@kinowo-mongo.internal:27017/kinowo?authSource=kinowo"

  "MongoConnection.clientSettings" should "force zlib for a loopback link (the local proxy tunnel) when the URI names none" in {
    val names = MongoConnection.clientSettings(ValidUri).getCompressorList.asScala.map(_.getName)
    names should contain ("zlib")
  }

  it should "leave compression OFF over a direct 6PN link (fast + encrypted, so zlib would only burn CPU)" in {
    MongoConnection.clientSettings(RemoteUri).getCompressorList.asScala.map(_.getName) shouldBe empty
  }

  it should "respect compressors the URI specifies rather than forcing zlib" in {
    val names = MongoConnection
      .clientSettings(ValidUri + "&compressors=snappy")
      .getCompressorList.asScala.map(_.getName)
    names should contain ("snappy")
    names should not contain "zlib"
  }

  // Socket I/O uses the driver's DEFAULT transport (JDK NIO2), so no TransportSettings
  // is applied. We briefly routed it through Netty native-epoll to escape a suspected
  // `sun.nio.ch` selector spin, but that was a red herring — the CPU-credit floor was
  // read-model projection cost, not an epoll spin (the fix was offloading projection off
  // the I/O threads + making it cheap). Reverting to the default drops the netty dep;
  // `getTransportSettings` is now null.
  "MongoConnection.clientSettings" should "leave the driver-default transport (no Netty TransportSettings)" in {
    MongoConnection.clientSettings(ValidUri).getTransportSettings  shouldBe null
    MongoConnection.clientSettings(RemoteUri).getTransportSettings shouldBe null
  }

  "MongoConnection.isLoopbackLink" should "recognise loopback hosts (localhost / 127.x / [::1]) and reject a 6PN host" in {
    import com.mongodb.ConnectionString
    def loopback(uri: String) = MongoConnection.isLoopbackLink(new ConnectionString(uri))
    loopback("mongodb://127.0.0.1:27017/db")               shouldBe true
    loopback("mongodb://localhost:27017/db")               shouldBe true
    loopback("mongodb://[::1]:27017/db")                   shouldBe true
    loopback("mongodb://kinowo-mongo.internal:27017/db")   shouldBe false
    loopback("mongodb://fdaa-0-1-2.flycast:27017/db")      shouldBe false
  }

  // Server-selection cap: the loopback /debug read-mirror passes a short timeout
  // so a down mirror fails fast instead of wedging every read on the driver's
  // 30s default. Prod passes none and keeps that default (the 2026-06-06 incident
  // wants prod tolerant of a slow/recovering node).
  it should "cap server-selection at the given timeout (the /debug mirror) when one is passed" in {
    val timeoutMs = MongoConnection
      .clientSettings(ValidUri, serverSelectionTimeout = Some(3.seconds))
      .getClusterSettings.getServerSelectionTimeout(MILLISECONDS)
    timeoutMs shouldBe 3000L
  }

  it should "leave the driver-default server-selection timeout (30s) when none is passed (prod)" in {
    val timeoutMs = MongoConnection
      .clientSettings(ValidUri)
      .getClusterSettings.getServerSelectionTimeout(MILLISECONDS)
    timeoutMs shouldBe 30000L
  }

  "MongoConnection.LocalMirrorTimeout" should "be far below the prod default so a dead mirror fails fast" in {
    MongoConnection.LocalMirrorTimeout should be < MongoConnection.DefaultProbeTimeout
    MongoConnection.LocalMirrorTimeout.toSeconds should be <= 5L
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
