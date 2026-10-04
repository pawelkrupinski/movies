package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/**
 * Specs do not wait out time by sleeping, and they share ONE hand-moved clock.
 *
 * The class of failure: about ten specs slept and then asserted — 100 ms "to let the stream
 * start", 150 ms "for the reaper to block", 1100 ms past a one-second `Last-Modified`, 5 ms past
 * a 1 ms lease. Under load a sleep is too short and the spec flakes; when the code is merely slow
 * the "nothing happened yet" assertion passes without testing anything. The repair is always one
 * of: a [[MutableClock]] the code under test reads (a TTL, an age, a stamp), a [[ManualScheduler]]
 * it schedules on (a delay or a period, stepped with `advance`), a latch/counter it signals (a
 * thread reached a point), or [[Eventually]] (a condition that will come true).
 *
 * Rules, each naming file:line:
 *
 *  1. No `Thread.sleep(…)` / `TimeUnit.X.sleep(…)` in test sources, nor an `Await.ready`/`result`
 *     bounded by a sub-second literal (a timed window, not a timeout), outside the allowlist.
 *  2. No test-side `extends Clock` except testkit's [[MutableClock]] — five specs had each grown a
 *     private copy of it.
 *
 * Runnable programs under the test trees (a non-spec file with `def main`) talk to the live world
 * and are out of scope.
 */
class NoSleepInTestsSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{TestRoots, argumentsAt, codeOf, read, scalaFiles, topLevelParts}

  // Why a file may still sleep. A "TODO(sleep-backlog)" entry is debt this lint only lets shrink:
  // migrate the file and its entry must go (the stale-entry test fails until it does).
  private val Overlap =
    "the sleep is the WORK of a fake (a slow fetch, write or render) so concurrent callers overlap; the " +
      "assertion is on what overlapped (peak in-flight, one coalesced call), and it waits on futures, not on time"
  private val RaceWindow =
    "widens a real race window on purpose; the assertion then waits on a happens-before (latch, close, future), " +
      "never on the sleep having been long enough"
  private val RealMongo =
    "TODO(sleep-backlog): the Mongo repository stamps with the system clock (no Clock seam on that path), so rows " +
      "need real milliseconds between them; give the Mongo path a Clock and step it instead"
  private val Browser =
    "TODO(sleep-backlog): asserts that something does NOT happen in a real Chrome (no PUT past the debounce, no " +
      "reload after a session check or past midnight) by waiting a window out; the page exposes no settled signal " +
      "for those paths to wait on instead"

  private val SelfTest = "its self-test feeds the matcher these shapes as string literals"

  private val Allowlist: Map[String, String] = Map(
    "worker/src/test/scala/tools/NoSleepInTestsSpec.scala" -> SelfTest,
    "worker/src/it/scala/ExperimentCacheFetch.scala" ->
      "the local agreement replay backs off a real site's 429/503 before asking again: a network wait, no clock to step",
    "worker/src/it/scala/IdentityShadow.scala" ->
      "the local live-gap replay backs off a real TMDB/IMDb 429 before retrying: a network wait, no clock to step",
    "testkit/src/main/scala/tools/Eventually.scala" ->
      "THE polling primitive: it sleeps between probes of a condition, never instead of one",
    "testkit/src/test/scala/tools/EventuallySpec.scala" ->
      "stalls one probe past Eventually's deadline to prove the deadline still gets its own try",
    "testkit/src/main/scala/tools/ExecutorProbes.scala"                      -> Overlap,
    "worker/src/test/scala/tools/ParallelDetailFetchSpec.scala"              -> Overlap,
    "worker/src/test/scala/tools/FilmwebDiffParallelFetchSpec.scala"         -> Overlap,
    "worker/src/test/scala/tools/SharedLiveAnswersSpec.scala"                -> Overlap,
    "worker/src/test/scala/tools/IdentityLookupSweepSpec.scala"              -> Overlap,
    "worker/src/test/scala/modules/WorkerWiringSpec.scala"                   -> Overlap,
    "worker/src/test/scala/services/identity/IdentityProjectionSpec.scala"   -> Overlap,
    "worker/src/test/scala/services/identity/TmdbDocumentsSpec.scala"        -> Overlap,
    "worker/src/test/scala/services/identity/TmdbStoreSpec.scala"            -> Overlap,
    "common/src/test/scala/services/resolution/ResolutionCacheSpec.scala"    -> Overlap,
    "common/src/test/scala/tools/PosterDecodeSpec.scala"                     -> Overlap,
    "e2e/src/test/scala/services/movies/CorpusComparisonSpec.scala"          -> Overlap,
    "common/src/test/scala/services/movies/MovieChangeStreamSpec.scala"      -> RaceWindow,
    "common/src/test/scala/services/identity/CoalescedTraceWritesSpec.scala" ->
      "a seeded 0-1 ms jitter that reorders two writers' interleaving per seed; the property holds for every order",
    "worker/src/test/scala/modules/wiring/ScrapeWiringFallbackSpec.scala" ->
      "a fetch that hangs far past the client's own timeout, so the spec proves the timeout fires (it never waits it out)",
    "worker/src/fixtures/scala/tools/FixpointPass.scala" ->
      "waits for real change streams (another thread, fed by Mongo) to go quiet between fixpoint passes; bounded",
    "web/src/it/scala/UserStateAcrossPodsIntegrationSpec.scala" ->
      "holds a real Mongo transaction open for a set time from another thread — the outage being simulated",
    "worker/src/it/scala/MovieRepositoryIntegrationSpec.scala"               -> RealMongo,
    "web/src/page/scala/tools/CdpDriver.scala" ->
      "THE browser polling primitive (waitFor / pollUntil) and Chrome's port-file wait: sleeps between probes, never instead of one",
    "web/src/page/scala/tools/CdpWaitForSpec.scala" ->
      "SIGSTOPs Chrome's renderer for a set time — the freeze being simulated; the assertion is waitFor's verdict",
    "web/src/page/scala/views/HiddenFilmsSyncModelSpec.scala" -> (
      "a fake server that answers one DELETE late, widening the sync race on purpose, and a quiesce that needs the page " +
        "idle across consecutive polls (quiet is a span, not an instant); every assertion waits on that quiet"),
    "web/src/page/scala/views/PageJsBehaviourSpec.scala"                     -> Browser)

  /** Files whose own `extends Clock` is allowed, and why. */
  private val ClockAllowlist: Map[String, String] = Map(
    "testkit/src/main/scala/tools/MutableClock.scala"     -> "the one shared hand-moved clock",
    "worker/src/test/scala/tools/NoSleepInTestsSpec.scala" -> SelfTest)

  private val Sleep       = """\bThread\.sleep\(|\bTimeUnit\.\w+\.sleep\(|\b(?:NANOSECONDS|MICROSECONDS|MILLISECONDS|SECONDS)\.sleep\(""".r
  private val AwaitCall   = """\bAwait\.(?:ready|result)\(""".r
  private val SubSecond   =
    """^\s*(?:\d[\d_]*\s*\.?\s*(?:nanos?|nanoseconds?|micros?|microseconds?|millis?|milliseconds?)\b|Duration\(\s*\d+\s*,\s*(?:"(?:ms|millis|milliseconds|nanos)"|(?:\w+\.)?(?:MILLISECONDS|NANOSECONDS|MICROSECONDS)))""".r
  private val ClockSubclass = """\bextends\s+(?:java\.time\.)?Clock\b""".r

  /** The 1-based lines of `src` (comment-free) holding a sleep or a sub-second Await window. */
  private[tools] def sleepLines(src: String): Seq[Int] = {
    def lineOf(at: Int) = src.substring(0, at).count(_ == '\n') + 1
    val sleeps  = Sleep.findAllMatchIn(src).map(m => lineOf(m.start))
    val windows = AwaitCall.findAllMatchIn(src).collect {
      case m if topLevelParts(argumentsAt(src, m.end - 1)).lastOption.exists(SubSecond.findFirstIn(_).isDefined) => lineOf(m.start)
    }
    (sleeps ++ windows).toSeq.distinct.sorted
  }

  private def clockLines(src: String): Seq[Int] =
    ClockSubclass.findAllMatchIn(src).map(m => src.substring(0, m.start).count(_ == '\n') + 1).toSeq

  private def isProgram(path: Path): Boolean = !path.toString.endsWith("Spec.scala") && read(path).contains("def main(")

  private lazy val sources: Seq[(Path, String)] =
    scalaFiles(TestRoots).filterNot(isProgram).map(p => p -> codeOf(p))

  private def offenders(lines: String => Seq[Int], allow: Map[String, String]): Seq[String] =
    sources.filterNot { case (p, _) => allow.contains(p.toString) }.flatMap { case (p, src) =>
      val raw = read(p).linesIterator.toIndexedSeq
      lines(src).map(n => s"$p:$n: ${raw(n - 1).trim}")
    }

  "the sleep matcher" should "catch every sleeping shape and pass the waits that are not timing" in {
    sleepLines("Thread.sleep(100)") shouldBe Seq(1)
    sleepLines("java.util.concurrent.TimeUnit.MILLISECONDS.sleep(5)") shouldBe Seq(1)
    sleepLines("MILLISECONDS.sleep(5)") shouldBe Seq(1)
    sleepLines("Await.ready(f, 50.millis)") shouldBe Seq(1)
    sleepLines("Await.result(\n  f,\n  Duration(200, MILLISECONDS))") shouldBe Seq(1)
    sleepLines("Await.result(f, 5.seconds)") shouldBe empty            // a timeout, not a window
    sleepLines("Await.ready(Future.sequence(Seq(a, b)), Duration.Inf)") shouldBe empty
    sleepLines("retrySleep: Long => Unit = Thread.sleep,") shouldBe empty   // a seam default, not a call
    sleepLines("latch.await(5, TimeUnit.SECONDS)") shouldBe empty
  }

  "test sources" should "not sleep or open sub-second Await windows outside the allowlist" in {
    sources.size should be > 100
    val found = offenders(sleepLines, Allowlist)
    withClue("These test lines wait out time. Step a tools.MutableClock / tools.ManualScheduler the code reads, " +
      "wait on a latch or counter it signals, or poll a condition with tools.Eventually — or allowlist the " +
      "file with a reason:\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a file that still sleeps" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot { file =>
      val path = Paths.get(file)
      Files.exists(path) && sleepLines(codeOf(path)).nonEmpty
    }
    withClue("Allowlisted but no longer sleeping — drop the entry (the backlog only shrinks):\n" + stale.mkString("\n") + "\n") {
      stale shouldBe empty
    }
  }

  "test sources" should "move time with the one shared MutableClock, not a private Clock subclass" in {
    clockLines("private final class StepClock(var now: Instant) extends Clock {") shouldBe Seq(1)
    clockLines("final class Ticking extends java.time.Clock") shouldBe Seq(1)
    clockLines("val clock: Clock = Clock.fixed(t, UTC)") shouldBe empty
    val found = offenders(clockLines, ClockAllowlist)
    withClue("Use tools.MutableClock (advance / setTo / ticker) instead:\n" + found.mkString("\n") + "\n")(found shouldBe empty)
    ClockAllowlist.keys.foreach(file => clockLines(codeOf(Paths.get(file))) should not be empty)
  }

  "the stepped-time doubles" should "exist once, in testkit" in {
    Seq("MutableClock", "ManualScheduler").foreach { name =>
      val declaring = sources.collect { case (p, src) if s"""\\bclass\\s+$name\\b""".r.findFirstIn(src).isDefined => p.toString }
      declaring shouldBe Seq(s"testkit/src/main/scala/tools/$name.scala")
    }
  }
}
