package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.collection.mutable
import scala.concurrent.duration._

class ThrottledHttpFetchSpec extends AnyFlatSpec with Matchers {

  private val Tmdb   = "https://api.themoviedb.org/3/search/movie?query=x"
  private val Tmdb2  = "https://api.themoviedb.org/3/movie/42"
  private val Filmweb = "https://www.filmweb.pl/api/v1/film/9"

  /** A delegate that plays back a per-URL queue of outcomes (a thunk that
   *  returns a body or throws). An empty/absent queue yields "default" — so a
   *  spurious retry that drains the queue is visible as a wrong return value. */
  private class ScriptedFetch extends HttpFetch {
    private val script = mutable.Map.empty[String, mutable.Queue[() => String]]
    def queue(url: String, outcomes: (() => String)*): Unit =
      script(url) = mutable.Queue(outcomes*)
    def get(url: String): String =
      script.get(url).filter(_.nonEmpty).map(_.dequeue()()).getOrElse("default")
    def post(url: String, body: String, contentType: String): String = get(url)
  }

  private def http429(retryAfter: Option[FiniteDuration]) =
    () => throw new HttpStatusException(429, "GET", "u", retryAfter)

  private def fixture(maxAttempts: Int = 4) = {
    val delegate = new ScriptedFetch
    val slept    = mutable.ListBuffer.empty[Long]
    val throttle = new ThrottledHttpFetch(
      delegate, maxAttempts = maxAttempts,
      jitterMillis = () => 0, now = () => Instant.EPOCH, sleep = ms => { slept += ms; () })
    (delegate, throttle, slept)
  }

  "ThrottledHttpFetch" should "retry a 429 and pace the retry by Retry-After" in {
    val (delegate, throttle, slept) = fixture()
    delegate.queue(Tmdb, http429(Some(2.seconds)), () => "ok")

    throttle.get(Tmdb) shouldBe "ok"
    slept should contain (2000L)   // the gate paused ~Retry-After before the retry
  }

  it should "pause a SUBSEQUENT call to the same host while its gate is active" in {
    val (delegate, throttle, slept) = fixture()
    delegate.queue(Tmdb, http429(Some(3.seconds)), () => "ok")
    throttle.get(Tmdb)             // trips the gate (pausedUntil = EPOCH + 3s)
    slept.clear()

    delegate.queue(Tmdb2, () => "ok2")
    throttle.get(Tmdb2) shouldBe "ok2"   // same host (api.themoviedb.org)
    slept should contain (3000L)         // the still-active gate paused this call too
  }

  it should "NOT pause a different host after a 429 on another" in {
    val (delegate, throttle, slept) = fixture()
    delegate.queue(Tmdb, http429(Some(5.seconds)), () => "oka")
    throttle.get(Tmdb)
    slept.clear()

    delegate.queue(Filmweb, () => "okb")
    throttle.get(Filmweb) shouldBe "okb"
    slept shouldBe empty
  }

  it should "propagate a non-429 status immediately without retrying" in {
    val (delegate, throttle, _) = fixture()
    delegate.queue(Tmdb, () => throw new HttpStatusException(404, "GET", Tmdb, None))
    a [HttpStatusException] should be thrownBy throttle.get(Tmdb)
    // queue is now empty; a retry would have returned "default" instead of throwing
  }

  it should "exhaust maxAttempts on a persistent 429 then propagate" in {
    val (delegate, throttle, slept) = fixture(maxAttempts = 3)
    delegate.queue(Tmdb, http429(Some(1.second)), http429(Some(1.second)), http429(Some(1.second)))
    a [HttpStatusException] should be thrownBy throttle.get(Tmdb)
    slept.size shouldBe 2   // waits before attempts 2 and 3; attempt 3's 429 propagates
  }

  it should "use the default pause when the server sends no Retry-After" in {
    val (delegate, throttle, slept) = fixture()
    val t = new ThrottledHttpFetch(delegate, defaultPause = 7.seconds,
      jitterMillis = () => 0, now = () => Instant.EPOCH, sleep = ms => { slept += ms; () })
    delegate.queue(Tmdb, http429(None), () => "ok")
    t.get(Tmdb) shouldBe "ok"
    slept should contain (7000L)
  }

  it should "report a host's 429 RATE, not just a bare count, once per summary interval" in {
    // A raw 429 count can't be judged: 3,000 of them is either catastrophic or
    // noise depending on the total, and the per-429 warning never carried one.
    // Tuning the pace empirically needs the denominator, so the summary reports
    // requests, throttles and the clean-rate together.
    val delegate = new ScriptedFetch
    val reports  = mutable.ListBuffer.empty[String]
    var clockMs  = 0L
    val throttle = new ThrottledHttpFetch(
      delegate, maxAttempts = 1, jitterMillis = () => 0,
      now = () => Instant.ofEpochMilli(clockMs), sleep = _ => (),
      summaryInterval = 5.minutes, report = Some(msg => { reports += msg; () }))

    // Three clean calls, then one 429 — 4 requests, 1 throttled = 75% clean.
    throttle.get(Tmdb) shouldBe "default"
    throttle.get(Tmdb) shouldBe "default"
    throttle.get(Tmdb) shouldBe "default"
    delegate.queue(Tmdb, http429(None))
    a [HttpStatusException] should be thrownBy throttle.get(Tmdb)

    withClue("no summary before the interval elapses: ") { reports shouldBe empty }

    clockMs = 5.minutes.toMillis
    throttle.get(Tmdb) shouldBe "default"

    reports should have size 1
    reports.head should include ("api.themoviedb.org")
    reports.head should include ("throttled (429)")
    reports.head should include ("75.0% clean")
  }

  it should "reset its tally each interval so a recovered host stops reporting failures" in {
    // Otherwise a burst of 429s during tuning would drag the clean-rate down
    // forever and the next pace could never be seen to have worked.
    val delegate = new ScriptedFetch
    val reports  = mutable.ListBuffer.empty[String]
    var clockMs  = 0L
    val throttle = new ThrottledHttpFetch(
      delegate, maxAttempts = 1, jitterMillis = () => 0,
      now = () => Instant.ofEpochMilli(clockMs), sleep = _ => (),
      summaryInterval = 5.minutes, report = Some(msg => { reports += msg; () }))

    delegate.queue(Tmdb, http429(None))
    a [HttpStatusException] should be thrownBy throttle.get(Tmdb)
    clockMs = 5.minutes.toMillis
    throttle.get(Tmdb)                       // flushes window 1 (0% clean)
    clockMs = 10.minutes.toMillis
    throttle.get(Tmdb)                       // flushes window 2 — clean only

    reports should have size 2
    reports(0) should include ("0.0% clean")
    reports(1) should include ("100.0% clean")
  }
}
