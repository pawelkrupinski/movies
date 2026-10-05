package clients.tools

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, GetOnlyHttpFetch, HttpFetch}

import java.io.File
import java.nio.file.Files

/**
 * Guards `RecordAllDataToFixture`'s capture of the paid-egress cinemas (Multikino,
 * Kino Kameralne / biletyna). Those sit behind a WAF that blocks our datacenter
 * IP, so they're fetched through a residential proxy → `direct` chain whose
 * proxy leg tunnels through its OWN clients. A `RecordingHttpFetch` wired as the
 * chain's inner `direct` fallback therefore never sees a proxy-served response —
 * the scrape succeeds but the corpus silently lacks every `www.multikino.pl`
 * fixture. The recorder fixes this by wrapping the WHOLE chain in recording.
 *
 * Both halves are pinned here with a hermetic stand-in for the proxy leg (a
 * fetch that returns a body without delegating to `direct`) — no network, no
 * proxy credentials needed.
 */
class RecorderChainCaptureSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val temporaryRoot = new File("test/resources/fixtures/recorder-chain-capture-spec")
  // The recorder script's wiring, built for this spec — forcing its lazy fetches builds the chains only
  // (no Mongo, no network, no `run()`) — over an empty configuration, so nothing the process sets reaches it.
  private lazy val recordingWiring = new RecordAllDataToFixture(new _root_.settings.ProcessConfiguration(_root_.tools.Env.of()))
  private val MultikinoFilmsUrl =
    "https://www.multikino.pl/api/microservice/showings/cinemas/0011/films"

  /** Stand-in for the proxy leg: serves `body` for any URL without consulting a
   *  fallback — the same shape as a proxy tunnel that succeeds. */
  private def proxyServing(body: String): HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = body
  }

  private def filmsFixture(directory: String): File =
    new File(s"test/resources/fixtures/recorder-chain-capture-spec/$directory/" +
      "www.multikino.pl/api/microservice/showings/cinemas/0011/films")

  "Recording wired as the chain's inner `direct` fallback (the old wiring)" should
    "miss a proxy-served response — the bug this fix closes" in {
    val recorderAsDirect = new RecordingHttpFetch(
      "recorder-chain-capture-spec/inner-direct", proxyServing("unused"))
    // The proxy serves first, so the `direct` recorder is never consulted.
    val chain = new FallbackHttpFetch(Seq(
      "proxy"  -> proxyServing("films-json-from-proxy"),
      "direct" -> recorderAsDirect))

    chain.get(MultikinoFilmsUrl) shouldBe "films-json-from-proxy"
    filmsFixture("inner-direct").exists shouldBe false
  }

  "Recording wrapped around the whole chain (the new wiring)" should
    "capture the proxy-served response keyed by the target URL" in {
    val chain: HttpFetch = new FallbackHttpFetch(Seq(
      "proxy"  -> proxyServing("films-json-from-proxy"),
      "direct" -> proxyServing("unused")))
    // Compiles only because RecordingHttpFetch's delegate was widened from
    // RealHttpFetch to HttpFetch — the change that lets the recorder wrap the
    // proxy chain at all.
    val recorder = new RecordingHttpFetch("recorder-chain-capture-spec/outer", chain)

    recorder.get(MultikinoFilmsUrl) shouldBe "films-json-from-proxy"
    val f = filmsFixture("outer")
    f.exists shouldBe true
    new String(Files.readAllBytes(f.toPath), "UTF-8") shouldBe "films-json-from-proxy"
  }

  "The recorder's wiring" should
    "wrap the Multikino and biletyna chains in recording from the OUTSIDE" in {
    // The actual fix: these must be RecordingHttpFetch (recording the whole
    // proxy chain), not a bare FallbackHttpFetch with recording buried inside as
    // `direct`. Forcing these lazy vals builds the chains only — no Mongo, no
    // network, no `main()`.
    recordingWiring.multikinoFetch shouldBe a[RecordingHttpFetch]
    recordingWiring.biletynaFetch  shouldBe a[RecordingHttpFetch]
  }

  /** A leg that counts what reaches it and serves `body`, or fails like a dead tunnel. */
  private final class Leg(body: Option[String]) extends GetOnlyHttpFetch {
    val calls = new java.util.concurrent.atomic.AtomicInteger
    override def get(url: String): String = {
      calls.incrementAndGet()
      body.getOrElse(throw new java.io.IOException("proxy: Tunnel failed, got: 503"))
    }
  }

  "The recorder's paid-egress chain" should "be `direct` alone when there is no residential proxy" in {
    val direct = new Leg(Some("from-direct"))
    modules.wiring.EgressWiring.paidEgressChain(None, direct, _root_.tools.SpecClock.Pinned) should be theSameInstanceAs direct
  }

  it should "ask the proxy first and leave `direct` unasked when the proxy answers" in {
    val proxy  = new Leg(Some("from-proxy"))
    val direct = new Leg(Some("from-direct"))
    modules.wiring.EgressWiring.paidEgressChain(Some(IndexedSeq(proxy)), direct, _root_.tools.SpecClock.Pinned)
      .get(MultikinoFilmsUrl) shouldBe "from-proxy"
    direct.calls.get shouldBe 0
  }

  it should "fall straight back to `direct` behind a proxy that failed" in {
    val direct = new Leg(Some("from-direct"))
    modules.wiring.EgressWiring.paidEgressChain(Some(IndexedSeq(new Leg(None))), direct, _root_.tools.SpecClock.Pinned)
      .get(MultikinoFilmsUrl) shouldBe "from-direct"
    direct.calls.get shouldBe 1
  }

  "The recorder's capture directory" should
    "default to `today` (the daily artifact directory), overridable via KINOWO_FIXTURE_DIR" in {
    // No KINOWO_FIXTURE_DIR in the test env → the dateless `today` directory the daily
    // country-fixture job + local sync key off (refresh-fixtures.yml overrides
    // it to a dd-MM-yyyy directory). Pins that the recorder no longer hard-codes a
    // date literal a workflow must sed.
    recordingWiring.captureDate shouldBe "today"
  }

  it should "build httpFetch's fixture tree under that directory (init-order safe)" in {
    // Regression: when captureDate was a runtime `val`, httpFetch (a lazy val
    // forced during super-construction) captured it as null → the corpus went to
    // `test/resources/fixtures/null` while only CAPTURE_DATE landed in `today`
    // (the 323-byte artifact). The lazy val caches that, so this asserts the
    // cached instance is keyed off the right directory.
    recordingWiring.httpFetch
      .asInstanceOf[RecordingHttpFetch].fixtureRoot shouldBe "test/resources/fixtures/today"
  }

  override def afterAll(): Unit = {
    deleteRecursively(temporaryRoot)
    super.afterAll()
  }

  private def deleteRecursively(f: File): Unit = {
    if (f.isDirectory) Option(f.listFiles).foreach(_.foreach(deleteRecursively))
    f.delete()
    ()
  }
}
