package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.concurrent.duration._

class RateLimitedHttpFetchSpec extends AnyFlatSpec with Matchers {

  private val Paced    = "https://www.filmstarts.de/_/showtimes/theater-A0416/d-2026-07-18/p-1/"
  private val Paced2   = "https://www.filmstarts.de/kinoprogramm/kino/A0006/"
  private val Unpaced  = "https://api.themoviedb.org/3/movie/42"

  private class CountingFetch extends GetOnlyHttpFetch {
    var calls = 0
    def get(url: String): String = { calls += 1; "body" }
  }

  /** A clock the test advances by hand, so pacing is asserted on the recorded
   *  sleeps rather than on wall-clock timing (which would make this flaky). */
  private class TestClock {
    private val instant = new AtomicReference(Instant.EPOCH)
    def now(): Instant = instant.get()
    def advance(by: FiniteDuration): Unit = instant.updateAndGet(_.plusMillis(by.toMillis))
  }

  private def fixture(interval: Option[FiniteDuration] = Some(250.millis)) = {
    val delegate = new CountingFetch
    val slept    = mutable.ListBuffer.empty[Long]
    val clock    = new TestClock
    val paced    = new RateLimitedHttpFetch(
      delegate,
      intervalFor = url => if (url.contains("filmstarts.de")) interval else None,
      now         = () => clock.now(),
      // Sleeping IS the passage of time here: record it and advance the clock,
      // so a second call sees the state a real sleep would have left behind.
      sleep       = ms => { slept += ms; clock.advance(ms.millis) }
    )
    (delegate, paced, slept, clock)
  }

  "RateLimitedHttpFetch" should "not delay the first call to a paced host" in {
    val (_, paced, slept, _) = fixture()
    paced.get(Paced) shouldBe "body"
    slept shouldBe empty
  }

  it should "space successive calls to a paced host by the configured interval" in {
    val (_, paced, slept, _) = fixture()
    paced.get(Paced)
    paced.get(Paced)
    paced.get(Paced)
    slept shouldBe List(250L, 250L)   // calls 2 and 3 each wait a full interval
  }

  it should "pace across DIFFERENT paths on the same host" in {
    val (_, paced, slept, _) = fixture()
    paced.get(Paced)
    paced.get(Paced2)                 // same origin, different path
    slept shouldBe List(250L)
  }

  it should "not pace a host with no configured interval" in {
    val (_, paced, slept, _) = fixture()
    paced.get(Unpaced)
    paced.get(Unpaced)
    paced.get(Unpaced)
    slept shouldBe empty
  }

  it should "keep each host's queue independent" in {
    val (_, paced, slept, _) = fixture()
    paced.get(Paced)
    paced.get(Unpaced)                // must not park on filmstarts' slot
    slept shouldBe empty
  }

  it should "not wait when the interval has already elapsed naturally" in {
    val (_, paced, slept, clock) = fixture()
    paced.get(Paced)
    clock.advance(400.millis)         // slower than the pace all by itself
    paced.get(Paced)
    slept shouldBe empty
  }

  it should "still deliver every request it paced" in {
    val (delegate, paced, _, _) = fixture()
    (1 to 5).foreach(_ => paced.get(Paced))
    delegate.calls shouldBe 5         // pacing delays calls, never drops them
  }

  it should "pace getBytes as well as get" in {
    val (_, paced, slept, _) = fixture()
    paced.get(Paced)
    paced.getBytes(Paced)
    slept shouldBe List(250L)
  }

  it should "delegate getAsync unpaced, rather than blocking a pool thread" in {
    // Deliberate hole, documented on the override: pacing here would either park
    // a pool thread or collapse RealHttpFetch's real async path onto the common
    // pool. No production caller uses getAsync; the override logs if one does.
    val (delegate, paced, slept, _) = fixture()
    paced.getAsync(Paced).join() shouldBe "body"
    paced.getAsync(Paced).join() shouldBe "body"
    slept shouldBe empty
    delegate.calls shouldBe 2
  }

  it should "read the pace for a real paced host off the HostPolicies table" in {
    // The production lookup, not the fixture's stub — guards the wiring between
    // the policy row and the decorator.
    // 1000ms: pace-report measurement showed 250ms and 500ms both throttled
    // under German daytime load (Filmstarts tolerates ~1 req/s), so the pace
    // stepped down to ~1 req/s for 0 throttled. See the policy's comment.
    RateLimitedHttpFetch.configuredInterval(Paced) shouldBe Some(1000.millis)
    RateLimitedHttpFetch.configuredInterval(Unpaced) shouldBe None
  }

  it should "pace Flicks (UK) so its 843-venue fan-out stops drawing 429s" in {
    // 200ms (~5 req/s) serialises the UK sweep so the concurrent bursts that trip
    // Flicks' limiter (Retry-After: 300-600s) never form. Coupled to the UK
    // cadence; see the policy comment and WorkerScrapeCadenceConfigSpec.
    RateLimitedHttpFetch.configuredInterval("https://www.flicks.co.uk/cinema/sessions/x/2026-07-31/") shouldBe Some(200.millis)
  }

  it should "let KINOWO_FLICKS_PACE_MS retune the Flicks pace without a restart" in {
    val flicks = "https://www.flicks.co.uk/cinema/sessions/x/2026-07-31/"
    withProperty("KINOWO_FLICKS_PACE_MS", "350") {
      RateLimitedHttpFetch.configuredInterval(flicks) shouldBe Some(350.millis)
    }
    RateLimitedHttpFetch.configuredInterval(flicks) shouldBe Some(200.millis)
  }

  it should "let KINOWO_FILMSTARTS_PACE_MS retune the pace without a restart" in {
    // The point of the knob: Webedia publishes no rate limit, so the pace is
    // found empirically. Resolving per request means an /admin/config flip
    // applies to the very next request, with no worker restart or cold JVM.
    withProperty("KINOWO_FILMSTARTS_PACE_MS", "900") {
      RateLimitedHttpFetch.configuredInterval(Paced) shouldBe Some(900.millis)
    }
    RateLimitedHttpFetch.configuredInterval(Paced) shouldBe Some(1000.millis)
  }

  it should "ignore a non-positive knob rather than unpacing the host" in {
    // 0 would read as "no gap at all" — the burst behaviour that drew the 429s
    // in the first place. Env.positiveLong drops it back to the compiled default.
    withProperty("KINOWO_FILMSTARTS_PACE_MS", "0") {
      RateLimitedHttpFetch.configuredInterval(Paced) shouldBe Some(1000.millis)
    }
    withProperty("KINOWO_FILMSTARTS_PACE_MS", "not-a-number") {
      RateLimitedHttpFetch.configuredInterval(Paced) shouldBe Some(1000.millis)
    }
  }

  /** Env reads system properties as well as the process env, so a property is
   *  how a test drives a knob. Cleared in `finally` to avoid cross-test leakage. */
  private def withProperty[A](key: String, value: String)(body: => A): A =
    try { System.setProperty(key, value); body } finally System.clearProperty(key)
}
