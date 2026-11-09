package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.Using

/**
 * A test's verdict does not depend on how busy the machine is.
 *
 * About half of Main's reds over 2026-10-24..26 were tests that assumed an idle machine: a 50 ms wall-clock limit
 * (MovieRecordCitiesSpec, now an allocation bound, 14fd40aca), a fixed 0.3 s sleep before a check (agent-procs-test,
 * now a wait-until, 7ee4b8714), script tests under one 600 s bound (now per-case bounds with a stall report, b028e15e1).
 * On a machine ten times slower each passes on a laptop and fails on the runner — or, a sleep too short, passes having
 * tested nothing. NoSleepInTestsSpec and NoLiteralWaitBoundSpec hold the Scala specs' waits; this holds the rest:
 *
 *  1. Scala specs assert no wall-clock duration from above (`elapsed should be < 2.seconds`). Bound the WORK instead:
 *     `tools.costs.AllocationMeter` / `CpuMeter` against a budget; a call that must not block is
 *     `tools.Answered.at` (it answers at all while what it must not wait on is shut); a wait is `Eventually`.
 *  2. Shell script tests (`*-test.sh`) never sleep a guessed interval to wait for something — neither `sleep 0.3; touch
 *     go` nor a poll loop counting `sleep 0.1`s that falls through silently when the count runs out. They
 *     `wait_until "<what>" <condition>` (scripts/shell-spec.sh): it returns the moment the condition holds and fails the
 *     case, named, at a deadline only a broken case reaches. A sleep of 5 s or more is a process that must stay alive (a
 *     hung stub, a decoy), not a wait, and is not flagged. Nor do they compute an elapsed time (`$((SECONDS - t0))`).
 *  3. Python tests (`*_test.py`) neither `time.sleep(…)` to wait nor take a clock difference to assert on.
 *
 * Outside the reviewed [[Allowlist]] (each entry `file: the line's text` with why), every hit fails, as file:line.
 * Playwright's own lint (page-tests-playwright/lint-flake-patterns.mjs: `wait-for-timeout`, `widened-timeout`) holds
 * the browser tests.
 */
class LoadIndependentTestsSpec extends AnyFlatSpec with Matchers {
  import ScalaSourceScan.{TestRoots, code, isProgram, read, scalaFiles}

  private val Timer =
    "asserts a timeout FIRES: the bound is the hang it cuts short, a margin of a whole timeout over its budget; " +
      "a bound that never fired would leave no upper limit to assert"

  private def cutShort(timeout: String, bound: String) =
    s"asserts a call returns long before a timeout it must not wait out ($timeout), where nothing the case holds shows " +
      s"it did not: $bound is a fraction of that timeout and orders of magnitude over the call"

  private val HangStub    = "a stub that must never end on its own (HUNG STUBS, scripts/shell-spec.sh): its loop waits on nothing"
  private val OwnDeadline =
    "a deadline loop in a spec with its own harness (no shell-spec): it waits for the condition, bounded by a deadline, never a count"

  /** `file: trimmed line` → why the line may stay. Keep each reason specific enough to check. */
  private val Allowlist: Map[String, String] = Map(
    "web/src/page/scala/tools/CdpProxyAuthSpec.scala: (System.nanoTime() / 1000000 - t0) should be < 15000L" ->
      "a real Chrome's HTTPS proxy-auth round trip: a broken challenge answer retries until Chrome gives up; 15 s tells the two apart",
    "worker/src/it/scala/deploy/FixtureServerBootSpec.scala: withClue(\"the relaunch waited out the ceiling instead of noticing the exit: \")(elapsedSeconds should be < 30.0)" ->
      cutShort("the boot script's ceiling", "30 s"),
    "worker/src/it/scala/deploy/FixtureServerBootSpec.scala: withClue(\"a dead process should be noticed on the next poll, not at the ceiling: \")(elapsedSeconds should be < 30.0)" ->
      cutShort("a 600 s ceiling", "30 s"),
    "worker/src/test/scala/modules/wiring/ScrapeWiringFallbackSpec.scala: (System.nanoTime() - started).nanos should be < 3.seconds" ->
      cutShort("a fetch that stalls, cut only by the adaptive ceiling", "3 s"),
    "worker/src/test/scala/services/identity/adapters/InlineFamilyAsksSpec.scala: (System.nanoTime() - started).nanos should be < 5.seconds" ->
      cutShort("a family source held for SpecTimeouts.Io", "5 s against a 200 ms deadline"),
    "worker/src/test/scala/services/movies/ChangeStreamLivenessSpec.scala: (System.nanoTime() - started).nanos should be < 5.seconds" ->
      cutShort("the 10 s grace and timeout", "5 s"),
    "worker/src/test/scala/services/tasks/TaskWorkerSpec.scala: ((System.nanoTime() - started) / 1000000L) should be < 1000L" ->
      cutShort("the doorbell's 60 s park", "1 s"),
    "worker/src/test/scala/services/tasks/TaskWorkerSpec.scala: elapsed should be < 2000L // but it did return" -> Timer,
    // ── script tests: what remains is no wait
    s"infra/bin/propose-pr-test.sh: command sleep 0.05" ->
      "the pause of the test's own deadline loop, which fails naming what it waited on (no shell-spec harness there)",
    s"scripts/agent-procs-test.sh: for _ in $$(seq 1 100); do kill -0 \"$$1\" 2>/dev/null || { echo no; return; }; command sleep 0.1; done" ->
      "a SPARED process must still be alive: an absence check, which a slow machine can only make vacuous, never red",
    s"scripts/ci/bounded-step-test.sh: hung=$$(step \"bash -c 'while :; do sleep 1; done' '$$work' & echo \\$$! > '$$work/child'; while :; do sleep 1; done\")" ->
      HangStub,
    s"scripts/ci/bounded-step-test.sh: escaped=$$(step \"setsid bash -c 'while :; do sleep 1; done' '$$work' & echo \\$$! > '$$work/child'; while :; do sleep 1; done\")" ->
      HangStub,
    s"scripts/identity-slice-test.sh: sleep 0.5" ->
      "an absence check: the keeper must still hold on after its JVM exec'd; a slow machine makes it vacuous, never red",
    s"scripts/jvm-gate-test.sh: until jvm_lock \"$$work/race$$round\"; do sleep 0.01; done" ->
      "the race itself: each racer retries the lock in its own subshell, which must hold it",
    s"scripts/jvm-gate-test.sh: echo in >> \"$$work/inside$$round\"; sleep 0.05; echo out >> \"$$work/inside$$round\"; jvm_unlock \"$$work/race$$round\" ) &" ->
      "a critical section held open on purpose, so racers overlap: what the case provokes, not a wait",
    s"scripts/jvm-gate-test.sh: echo \"+ $$JVM_SLOT\" >> \"$$work/slot-log\"; sleep 0.2; echo \"- $$JVM_SLOT\" >> \"$$work/slot-log\"; jvm_slot_release ) &" ->
      "a slot held open on purpose, so racers overlap: what the case provokes, not a wait",
    s"scripts/jvm-gate-test.sh: for _ in $$(seq 1 200); do jvm_slot_take_if_free && { echo \"took $${JVM_SLOT##*/}\"; break; }; sleep 0.05; done ) > \"$$work/got\"" ->
      "takes the slot in its own subshell, which must hold it; the case asserts which slot it took, not when",
    s"scripts/local-mirror/mirror-resilience-test.sh: while kill -0 \"$$child\" 2>/dev/null && [ \"$$SECONDS\" -lt \"$$deadline\" ]; do command sleep 0.1; done" ->
      OwnDeadline,
    s"scripts/local-mirror/mirror-resilience-test.sh: sleep()                   { command sleep 0.05; }" ->
      "the tool's `sleep` stubbed shorter: the tool's back-off, not the spec's wait",
    s"scripts/local-mirror/mirror-resilience-test.sh: while [ ! -s \"$$work/tailer.pid\" ] && [ \"$$SECONDS\" -lt \"$$deadline\" ]; do command sleep 0.05; done" ->
      OwnDeadline,
    s"scripts/local-mirror/mirror-resilience-test.sh: while [ -n \"$$tailer\" ] && kill -0 \"$$tailer\" 2>/dev/null && [ \"$$SECONDS\" -lt \"$$deadline\" ]; do command sleep 0.05; done" ->
      OwnDeadline,
    s"scripts/local-mirror/mirror-resilience-test.sh: sleep() { command sleep 0.02; }" ->
      "the tool's `sleep` stubbed shorter: the tool's back-off, not the spec's wait",
    s"scripts/local-mirror/mirror-resilience-test.sh: probe() { command sleep 0.5; }" ->
      "the slow command `bounded` must outlast: its poll has to run at least once, which is where bash 3.2 exec'd the supervisor away",
    s"scripts/local-mirror/mirror-resilience-test.sh: command sleep 0.5" ->
      "an absence check: the watch must not return while both pids live; a slow machine makes it vacuous, never red",
    s"scripts/local-mirror/mirror-resilience-test.sh: while kill -0 \"$$watch\" 2>/dev/null && [ \"$$SECONDS\" -lt \"$$deadline\" ]; do command sleep 0.05; done" ->
      OwnDeadline,
    s"scripts/local-mirror/mirror-test.sh: sleep() { [ \"$$1\" = 999 ] && { command sleep 0.05; return 0; }; echo \"backed off $$1\" > \"$$work/expired\"; exit 0; }" ->
      "the tool's `sleep` stubbed: the tool's back-off, not the spec's wait",
    s"scripts/local-mirror/mirror-test.sh: [ \"$$1\" = 999 ] && { command sleep 0.05; return 0; }" ->
      "the tool's `sleep` stubbed: the tool's back-off, not the spec's wait",
    s"scripts/local-mirror/prod-tunnel-test.sh: sleep 1; kill -0 $$PROD_TUNNEL_PID && echo alive >> \"'\"$$scratch\"'/b\"" ->
      "an absence check inside a stub: the first tunnel must outlive a second one's start; vacuous when slow, never red",
    s"scripts/tool-conventions-test.sh: sleep 0.1" ->
      "paces the loop feeding a pseudo-terminal until the command is done or gone (its end, not a count, stops it)",
    s"scripts/wait-for-test.sh: ( sleep 0.3; echo \"compiling\"; echo \"exit=0 done\" ) > \"$$work/late.log\" &" ->
      "the subject: a log written late in the background, which the tool under test (wait-for) must wait out",
    s"scripts/wait-for-test.sh: ( sleep 0.3; echo \"LANDABLE (local)\" > \"$$work/missing.log\" ) &" ->
      "the subject: a log that appears late, which wait-for must wait for",
    s"scripts/wait-for-test.sh: ( echo \"[error] compile failed\"; sleep 0.2 ) > \"$$work/dead.log\" &" ->
      "the subject: a writer that lingers after its error line, which wait-for must not wait out",
    s"scripts/ci/ci_timings_test.py: time.sleep(0.1)" ->
      "the pause inside the test's wait_until helper, which polls to a deadline and fails naming what it waited on",
    "common/src/test/scala/tools/ManagedResourcesSpec.scala: stopwatch.elapsed should be < (grace * 2)" ->
      ("the claim IS a duration: one grace for every stuck pool, not one each (3 pools: 3 x grace against < 2 x); the " +
        "pools spin deaf to their interrupt, so nothing but the grace ends the stop"),
    "http/src/test/scala/tools/RealHttpFetchSpec.scala: elapsed.compareTo(deadline.plusSeconds(3)) should be < 0" -> Timer
  )

  // ── 1. Scala ───────────────────────────────────────────────────────────────

  private val UpperBound = """\bshould\s*\(?\s*be\s*(?:<|<=|lessThan)|\bshouldBe\s*<|\bassert\s*\(.*\s<=?\s""".r
  /** A duration MEASURED on the wall: a clock read, or what one was read into. A configured interval is no measure. */
  private val TimeWord   = """(?i)nanotime|currenttimemillis|\w*elapsed\w*|\btook\b|stopwatch|\bstarted\b""".r
  /** A bound on the work measured as CPU or allocation, which a loaded machine does not stretch. */
  private val WorkMeter  = """\b(?:CpuMeter|AllocationMeter)\b""".r

  private[tools] def wallClockAssertion(line: String): Boolean =
    UpperBound.findFirstIn(line).isDefined && TimeWord.findFirstIn(line).isDefined && WorkMeter.findFirstIn(line).isEmpty

  "the Scala matcher" should "see a wall-clock upper bound, and not a work bound, a lower bound or a count" in {
    Seq("(System.nanoTime() - started).nanos should be < 1.second", "stopwatch.elapsed should be < 2.seconds",
      "elapsed should be < 2000L", "(System.nanoTime() - began) / 1000000 should be < 100L", "elapsedMs should be < pacedMs",
      "elapsed.compareTo(deadline.plusSeconds(3)) should be < 0", "assert(took <= budget.toMillis)")
      .filterNot(wallClockAssertion) shouldBe empty
    Seq("tools.costs.CpuMeter.fastest()(shapes(long)) should be < 100.millis", "elapsed.compareTo(requestBudget) should be >= 0",
      "calls should be < 5", "stats.deadlineFor(host) shouldBe 120.millis", "AuthExchangeCodes.Ttl should be <= Duration.ofMinutes(5)")
      .filter(wallClockAssertion) shouldBe empty
  }

  "Scala specs" should "assert no wall-clock duration from above" in {
    val offenders = scalaFiles(TestRoots).filterNot(isProgram).filterNot(_.endsWith("LoadIndependentTestsSpec.scala")).flatMap(path => hits(path, read(path).linesIterator.map(code).toSeq, wallClockAssertion))
    withClue("These assert a wall-clock duration, which a loaded machine stretches. Bound the work (tools.costs.CpuMeter / " +
      "AllocationMeter), assert the call answers at all (tools.Answered.at), or allowlist the line with why:\n" +
      offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }

  // ── 2. shell ───────────────────────────────────────────────────────────────

  private val Sleep   = """(?:^|[;&|({]|\b(?:do|then|else)\b)\s*(?:command\s+)?sleep\s+([0-9.]+)\b(?!\s*&(?!&))""".r
  private val Elapsed = """\$\(\(\s*\$?SECONDS\s*-|\bdate \+%s\)?\s*-\s*\$""".r
  private val Heredoc = """<<-?\s*['"]?(\w+)['"]?""".r

  /** The lines of a shell script as a scan reads them: comments, single-quoted text and heredoc bodies (a stub's own
   *  script) blanked, line numbers kept. */
  private[tools] def shellCode(src: String): Seq[String] = {
    var closing: Option[String] = None
    src.linesIterator.map { line =>
      closing match {
        case Some(end) => if (line.trim == end) closing = None; ""
        case None =>
          val bare = line.replaceAll("'[^']*'", "''").replaceAll("""(^|\s)#.*$""", "")
          closing = Heredoc.findFirstMatchIn(line.replaceAll("""(^|\s)#.*$""", "")).map(_.group(1))
          bare
      }
    }.toSeq
  }

  private[tools] def shellWait(line: String): Boolean =
    Sleep.findAllMatchIn(line).exists(m => m.group(1).toDouble < 5) || Elapsed.findFirstIn(line).isDefined

  "the shell matcher" should "see a sleep that waits and an elapsed time, not a process kept alive" in {
    Seq("sleep 0.3; touch \"$work/go\"", "for _ in $(seq 1 50); do [ -e f ] && break; sleep 0.1; done",
      "  sleep 1", "kill \"$p\"; command sleep 0.2", "took=$(( SECONDS - start ))")
      .filterNot(l => shellWait(shellCode(l).head)) shouldBe empty
    Seq("sleep 300 &", "exec sleep 86400", "echo $$ >> \"$HUNG\"; exec sleep 86400", "sleep 30", "deadline=$((SECONDS + 5))",
      "bash -c 'sleep 0.3; echo hi'", "# sleep 0.3 here", "wait_until \"it ran\" '[ -e f ]'")
      .filter(l => shellWait(shellCode(l).head)) shouldBe empty
    shellCode("cat > stub <<'EOF'\nsleep 0.5\nEOF\nsleep 0.5").map(shellWait) shouldBe Seq(false, false, false, true)
  }

  private lazy val scriptTests: Seq[Path] = repoFiles(name => name.endsWith("-test.sh"))
  private lazy val pythonTests: Seq[Path] = repoFiles(name => name.endsWith("_test.py"))

  "Shell script tests" should "wait for what they wait for, never a guessed interval, and time nothing" in {
    scriptTests.nonEmpty shouldBe true
    val offenders = scriptTests.flatMap(path => hits(path, shellCode(read(path)), shellWait))
    withClue("These script-test lines sleep to wait, or time something. `wait_until \"<what>\" <condition>` " +
      "(scripts/shell-spec.sh) waits for the condition itself and fails the case at a deadline; or allowlist the line " +
      "with why:\n" + offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }

  // ── 3. python ──────────────────────────────────────────────────────────────

  private val PythonWait = """\btime\.sleep\(|\b(?:time\.)?(?:monotonic|perf_counter|time)\(\)\s*-""".r

  "Python tests" should "neither sleep to wait nor time anything" in {
    PythonWait.findFirstIn("            time.sleep(0.5)") shouldBe defined
    PythonWait.findFirstIn("elapsed = time.monotonic() - t0") shouldBe defined
    PythonWait.findFirstIn("deadline = time.monotonic() + 30") shouldBe None
    val offenders = pythonTests.flatMap { path =>
      hits(path, read(path).linesIterator.map(_.replaceAll("""(^|\s)#.*$""", "")).toSeq, l => PythonWait.findFirstIn(l).isDefined)
    }
    withClue("These Python-test lines sleep to wait or time something: poll the condition to a deadline and fail with " +
      "what was waited on, or allowlist the line with why:\n" + offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }

  it should "keep every allowlist entry pointing at a line still there" in {
    val lines = (scalaFiles(TestRoots) ++ scriptTests ++ pythonTests).iterator
      .flatMap(p => read(p).linesIterator.map(l => s"$p: ${l.trim}")).toSet
    (Allowlist.keySet -- lines).toSeq.sorted shouldBe empty
  }

  /** Each line of `path` the matcher hits and the allowlist does not hold, as `file:line: text`. */
  private def hits(path: Path, scanned: Seq[String], hit: String => Boolean): Seq[String] = {
    val raw = read(path).linesIterator.toVector
    scanned.zipWithIndex.collect {
      case (line, i) if hit(line) && !Allowlist.contains(s"$path: ${raw(i).trim}") => s"$path:${i + 1}: ${raw(i).trim}"
    }
  }

  /** Every file under the repository whose name `wanted` picks, outside build output and installed packages. */
  private def repoFiles(wanted: String => Boolean): Seq[Path] =
    Using.resource(Files.walk(Paths.get("."))) { paths =>
      paths.iterator.asScala.map(p => Paths.get(".").relativize(p))
        .filter(p => wanted(p.getFileName.toString))
        .filterNot(p => p.iterator.asScala.map(_.toString).exists(Set("node_modules", "target", ".git", ".claude")))
        .toVector.sortBy(_.toString)
    }
}
