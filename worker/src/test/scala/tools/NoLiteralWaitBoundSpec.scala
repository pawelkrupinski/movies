package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/**
 * Every bound a test puts on a wait comes from [[SpecTimeouts]], never a literal.
 *
 * The class of failure: a spec bounded a wait by whatever looked comfortable on an idle laptop —
 * `Await.result(f, 10.seconds)`, `latch.await(5, SECONDS)`, `eventually(…, timeoutMs = 2000)`,
 * `timeout(Span(5, Seconds))` — and under a loaded machine or CI runner the wait ran out although
 * nothing was wrong: `MongoTtlIndexIntegrationSpec` timed out at 10 s inside a full `itAll` and
 * passed alone, and the same week HardClusterConvergence's staging write, the auth exchange-code
 * store, StagingFold's in-flight commit and MovieChangeStream's debounce did too. A positive
 * wait returns the moment its thing happens, so a generous bound costs nothing when green;
 * [[SpecTimeouts]] holds those bounds, scaled by `KINOWO_SPEC_TIME_SCALE` for a slower runner.
 *
 * A literal is flagged in each of a wait's bound positions, over every test-side tree (comments
 * and string literals stripped; runnable programs, which talk to the live world, out of scope):
 *
 *  1. the last argument of `Await.result` / `Await.ready`;
 *  2. a Java timed wait — `.await` / `.get` / `.poll` / `.offer` / `.tryAcquire` / `.tryLock` /
 *     `.awaitTermination` / `.waitFor` with a `(n, TimeUnit)` pair, or `.join(n)`;
 *  3. [[Eventually]]'s deadline (`timeoutMs =`, `poll(n)`), a helper's `budgetMs` / `joinTimeout`;
 *  4. a ScalaTest patience: `timeout(Span(n, …))`, `Timeout(Span(…))`, `PatienceConfig(Span(…), …)`;
 *     or an HTTP request's `.timeout(Duration.ofSeconds(n))`;
 *  5. a `val` named `…Timeout` / `…Budget` / `…Patience` holding a literal — the same literal one
 *     hop away.
 *
 * A window watched for something NOT to happen is not a timeout: it says so with
 * `SpecTimeouts.quiet(window)`, which this lint reads as named. A spec asserting that a wait
 * DOES time out keeps its literal on the allowlist below, with why, and should bound the
 * elapsed time from above too.
 */
class NoLiteralWaitBoundSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{TestRoots, argumentsAt, codeOf, read, scalaFiles, topLevelParts}

  // (file, a substring of the flagged line) → why that bound is a deliberate literal.
  private val Allowlist: Map[(String, String), String] = Map(
    ("testkit/src/main/scala/tools/SpecTimeouts.scala", "") -> "THE policy: the one place the bounds are literals",
    ("testkit/src/test/scala/tools/EventuallySpec.scala", "timeoutMs =") ->
      "tests Eventually's own deadline handling, so it sets deadlines it then watches run out (each bounded from above)",
    ("web/src/page/scala/tools/CdpWaitForSpec.scala", "timeoutMs = 2000") ->
      "the budget a 2.5 s renderer freeze must NOT be charged against — the literal IS what is under test",
    ("web/src/page/scala/tools/CdpWaitForSpec.scala", "timeoutMs = 500") ->
      "asserts waitFor DOES time out, and names the 500 ms in its message; the elapsed time is bounded from above",
    ("web/src/page/scala/tools/CdpWaitForSpec.scala", "timeoutMs = 1000") ->
      "pollUntil's own unit cases: each returns or throws on its first or second check, the bound never reached",
    ("web/src/page/scala/tools/CdpSocketLossSpec.scala", "timeoutMs = 5000") ->
      ("races a lost DevTools connection against the poll's own deadline: the connection error must win, so the deadline " +
        "is the thing raced (and the elapsed time is bounded from above at 10 s)"),
    ("web/src/page/scala/tools/CdpDriver.scala", "process.waitFor(3, TimeUnit.SECONDS)") ->
      "a graceful-exit grace before destroyForcibly: a longer bound would only delay tearing down a hung Chrome, never pass anything",
    ("web/src/page/scala/views/PageJsBehaviourSpec.scala", "timeoutMs = 150") ->
      "a max-duration assertion: the hide's request must fire within 150 ms, well under the retired 400 ms debounce",
  )

  private val DurationLiteral =
    """^\s*(?:\d[\d_]*(?:\.\d+)?\s*\.?\s*(?:nanos?|nanoseconds?|micros?|microseconds?|millis?|milliseconds?|seconds?|minutes?|hours?|days?)\b|(?:Finite)?Duration\s*\(\s*\d|(?:java\.time\.)?Duration\.(?:of|from)\w*\(\s*\d)""".r
  private val NumberLiteral = """^\s*\d[\d_]*L?\s*$""".r
  private val TimeUnitArgument =
    """^\s*(?:[\w.]+\.)?(?:NANOSECONDS|MICROSECONDS|MILLISECONDS|SECONDS|MINUTES|HOURS|DAYS)\s*$""".r

  private val AwaitCall    = """\bAwait\.(?:ready|result)\(""".r
  private val JavaTimedWait = """\.(await|get|poll|offer|tryAcquire|tryLock|awaitTermination|waitFor|join)\(""".r
  private val EventuallyCall = """(?:^|[^\w.])eventually\(""".r
  private val Literal = """(?:\d|(?:Finite)?Duration\s*\(\s*\d|[\w.]*Span\(\s*\d)"""
  private val TimeLiteral =
    """(?:\d[\d_]*\s*\.?\s*(?:nanos?|micros?|millis?|milliseconds?|seconds?|minutes?|hours?)\b|(?:Finite)?Duration\s*\(\s*\d|[\w.]*Span\(\s*\d)"""
  private val BoundName = """\b(?:val|var)\s+\w*(?:Timeout|Budget|Patience|Bound|Deadline)"""
  private val DirectShapes = Seq(
    s"""\\b(?:timeoutMs|budgetMs|joinTimeout)\\s*(?::\\s*[\\w.\\[\\]]+\\s*)?=(?![=>])\\s*$Literal""",
    """(?:^|[^\w.])poll\(\s*\d""",
    """\bEventually\.poll\(\s*\d""",
    """\b(?:timeout|Timeout|scaled)\(\s*(?:[\w.]*Span\(\s*\d|\d)""",
    """\bPatienceConfig\(\s*(?:timeout\s*=\s*)?(?:[\w.]*Span\(\s*\d|\d)""",
    """\.timeout\(\s*(?:java\.time\.)?Duration\.of\w+\(\s*\d""",
    s"""$BoundName\\s*(?::\\s*[\\w.]+\\s*)?=(?![=>])\\s*$TimeLiteral""",
    s"""$BoundName(?:Ms|Millis)\\s*(?::\\s*[\\w.]+\\s*)?=(?![=>])\\s*\\d""",
  ).map(_.r)

  private val StringLiteral = """"(?:[^"\\\n]|\\.)*"""".r

  /** The 1-based lines of `src` (comments already dropped) bounding a wait by a literal. */
  private[tools] def literalBoundLines(code: String): Seq[Int] = {
    val src = StringLiteral.replaceAllIn(code, "\"\"")
    def lineOf(at: Int) = src.substring(0, at).count(_ == '\n') + 1
    def args(at: Int)   = topLevelParts(argumentsAt(src, at))
    val awaits = AwaitCall.findAllMatchIn(src).collect {
      case m if args(m.end - 1).lastOption.exists(DurationLiteral.findFirstIn(_).isDefined) => lineOf(m.start)
    }
    val javaWaits = JavaTimedWait.findAllMatchIn(src).collect {
      case m if {
        val parts = args(m.end - 1)
        if (m.group(1) == "join") parts.size == 1 && NumberLiteral.matches(parts.head)
        else parts.sliding(2).exists {
          case Seq(amount, unit) => NumberLiteral.matches(amount) && TimeUnitArgument.matches(unit)
          case _                 => false
        }
      } => lineOf(m.start)
    }
    val eventuallyPositional = EventuallyCall.findAllMatchIn(src).collect {
      case m if args(m.end - 1).drop(1).exists(NumberLiteral.matches) => lineOf(m.start)
    }
    val direct = DirectShapes.iterator.flatMap(_.findAllMatchIn(src)).map(m => lineOf(m.start))
    (awaits ++ javaWaits ++ eventuallyPositional ++ direct).toSeq.distinct.sorted
  }

  private def isProgram(path: Path): Boolean = !path.toString.endsWith("Spec.scala") && read(path).contains("def main(")

  private lazy val sources: Seq[(Path, String)] =
    scalaFiles(TestRoots).filterNot(isProgram).map(p => p -> codeOf(p))

  /** Each flagged line as (file, raw line text, "file:line: text"). */
  private lazy val flagged: Seq[(String, String, String)] = sources.flatMap { case (p, src) =>
    val raw = read(p).linesIterator.toIndexedSeq
    literalBoundLines(src).map(n => (p.toString, raw(n - 1), s"$p:$n: ${raw(n - 1).trim}"))
  }

  private def allowed(file: String, line: String): Boolean =
    Allowlist.keys.exists { case (f, snippet) => f == file && line.contains(snippet) }

  "the literal-bound matcher" should "catch every bound shape with a literal in it" in {
    literalBoundLines("Await.result(f, 10.seconds)") shouldBe Seq(1)
    literalBoundLines("Await.ready(\n  f,\n  Duration(5, SECONDS))") shouldBe Seq(1)
    literalBoundLines("x\nAwait.result(store.find(id), 2.minutes).map(_.size)") shouldBe Seq(2)
    literalBoundLines("latch.await(5, TimeUnit.SECONDS) shouldBe true") shouldBe Seq(1)
    literalBoundLines("done.await(30, java.util.concurrent.TimeUnit.SECONDS)") shouldBe Seq(1)
    literalBoundLines("futures.foreach(_.get(10, SECONDS))") shouldBe Seq(1)
    literalBoundLines("queue.poll(5, TimeUnit.SECONDS)") shouldBe Seq(1)
    literalBoundLines("pool.awaitTermination(1, TimeUnit.MINUTES)") shouldBe Seq(1)
    literalBoundLines("lock.tryAcquire(1, 2, TimeUnit.SECONDS)") shouldBe Seq(1)
    literalBoundLines("worker.join(5000)") shouldBe Seq(1)
    literalBoundLines("eventually(seen shouldBe 2, timeoutMs = 5000)") shouldBe Seq(1)
    literalBoundLines("eventually(seen shouldBe 2, 5000)") shouldBe Seq(1)
    literalBoundLines("eventually(current shouldBe Some(0L), timeoutMs = 10.seconds.toMillis)") shouldBe Seq(1)
    literalBoundLines("Eventually.poll(30000)(rows.nonEmpty) shouldBe true") shouldBe Seq(1)
    literalBoundLines("poll(1000)(cache.lastChangeAt(warm).isDefined)") shouldBe Seq(1)
    literalBoundLines("def settleUntil(target: Int, budgetMs: Long = 60000)(accounted: => Int)") shouldBe Seq(1)
    literalBoundLines("race(ops, Some(round), joinTimeout = 2.minutes)") shouldBe Seq(1)
    literalBoundLines("eventually(timeout(Span(5, Seconds)), interval(Span(150, Millis))) {") shouldBe Seq(1)
    literalBoundLines("eventually(PatienceConfiguration.Timeout(Span(5, Seconds)))(x)") shouldBe Seq(1)
    literalBoundLines("PatienceConfig(timeout = Span(30, Seconds), interval = Span(1, Seconds))") shouldBe Seq(1)
    literalBoundLines("PatienceConfig(Span(40, Seconds), Span(1, Seconds))") shouldBe Seq(1)
    literalBoundLines("private val Timeout = 30.seconds") shouldBe Seq(1)
    literalBoundLines("HttpRequest.newBuilder(uri).timeout(Duration.ofSeconds(5))") shouldBe Seq(1)
    literalBoundLines("private val SpecBudget: FiniteDuration = 2.minutes") shouldBe Seq(1)
    literalBoundLines("private val StreamBoundMs = 10000L") shouldBe Seq(1)
    literalBoundLines("if (!process.waitFor(120, TimeUnit.SECONDS)) process.destroyForcibly()") shouldBe Seq(1)
  }

  it should "pass the policy's bounds, named quiet windows, waits with no bound, and non-wait durations" in {
    literalBoundLines("Await.result(f, SpecTimeouts.Io)") shouldBe empty
    literalBoundLines("Await.ready(Future.sequence(Seq(a, b)), Duration.Inf)") shouldBe empty
    literalBoundLines("latch.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)") shouldBe empty
    literalBoundLines("written.await(SpecTimeouts.quiet(1.second).toMillis, MILLISECONDS) shouldBe false") shouldBe empty
    literalBoundLines("third.join(SpecTimeouts.quiet(200.millis).toMillis)") shouldBe empty
    literalBoundLines("eventually(timeout(SpecTimeouts.Settle), interval(Span(150, Millis)))") shouldBe empty
    literalBoundLines("latch.await()") shouldBe empty
    literalBoundLines("map.get(5)") shouldBe empty                                    // a lookup, not a wait
    literalBoundLines("clock.advance(5.seconds)") shouldBe empty
    literalBoundLines("new UptimeMonitor(db, ttl = 30.seconds)") shouldBe empty
    literalBoundLines("ReadModelProjector.awaitStreamApplied(l, grace = 0.seconds, timeout = 200.millis)") shouldBe empty
    literalBoundLines("timeoutMs == 5000") shouldBe empty
    literalBoundLines("sleepLines(\"latch.await(5, TimeUnit.SECONDS)\") shouldBe empty") shouldBe empty   // a string
    literalBoundLines("val DefaultBudget = 200") shouldBe empty                        // a count, not a time
  }

  "test sources" should "bound every wait by SpecTimeouts outside the allowlist" in {
    sources.size should be > 100
    val found = flagged.collect { case (file, line, label) if !allowed(file, line) => label }
    withClue("These test waits are bounded by a literal, which a loaded runner outlasts. Use tools.SpecTimeouts " +
      "(Io for one operation, Settle for eventual consistency, Run for a whole run, Pace for a re-trigger cadence, " +
      "quiet(window) for an absence window) — or allowlist the line with why:\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a line that still bounds a wait by a literal" in {
    val stale = Allowlist.keys.toSeq.filterNot { case (file, snippet) =>
      snippet.isEmpty && Files.exists(Paths.get(file)) ||
        flagged.exists { case (f, line, _) => f == file && line.contains(snippet) }
    }
    withClue("Allowlisted but no longer flagged — drop the entry:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
  }
}
