package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

import ScalaSourceScan.{MainRoots, code, definesMain, read, scalaFiles}

/**
 * Production code takes its threads and its waits from the composition root: a service asks the `threadPools` it
 * was handed for its scheduler, and sleeps on the `sleep` it was handed.
 *
 * The class of bug: fifteen services each built their own `DaemonExecutors.scheduler(…)` in a `private val`, and the
 * read-model sweeper, the web backstop, the change stream and the Mongo reconnect each called `Thread.sleep` inline.
 * A pool built inside a service is the service's alone to shut, so one whose `stop()` no wiring called ticked on past
 * the root's stop; and a spec could neither step the scheduler nor skip the wait without a seam cut for that one class.
 *
 * Rules, each naming file:line, over the main sources (`ScalaSourceScan.MainRoots`):
 *
 *  1. [[Rule.PoolBuilt]]: no `DaemonExecutors.<builder>(…)` outside a composition root (the `modules` trees: the
 *     roots and their wiring traits) or a program's `main`. Take a `threadPools: ThreadPools` (or the executor
 *     itself) and let the wiring build it: `NoUnmanagedWiringExecutorSpec` then holds it to `managedResources`.
 *  2. [[Rule.ThreadSleep]] / [[Rule.TimeUnitSleep]]: no `Thread.sleep` (called or passed) and no `TimeUnit.X.sleep(…)`
 *     outside a root or a program. Take `sleep: FiniteDuration => Unit` (and a `stopwatch: Stopwatch` to bound the
 *     wait), passed down as the root's `sleep` ([[Sleep.Blocking]]).
 *
 * `System.nanoTime` is the process time `NoWallClockInTestsSpec` rule 5 already refuses outside its `MainTimeSources`.
 * Where a site is rightly not the wiring's, add it to [[Allowlist]] with WHY; an entry naming a site that no longer
 * offends fails until it is dropped.
 */
class NoUnwiredThreadsAndWaitsSpec extends AnyFlatSpec with Matchers {
  import NoUnwiredThreadsAndWaitsSpec.*

  /** The seams themselves: where the process's pools and its one sleep are defined. */
  private val Seams = Set(
    "common/src/main/scala/tools/DaemonExecutors.scala",
    "common/src/main/scala/tools/ThreadPools.scala",
    "base/src/main/scala/tools/Sleep.scala")

  private val Scoped =
    "a scoped pool: built, used and shut inside one call (a fan-out that returns when its work is done), so there is " +
      "nothing long-lived for a root to own"
  private val SleepSeamDefault =
    "the default of a `sleep: Long => Unit` seam the class already takes; every spec passes its own, and the roots pass " +
      "theirs where they build it"
  private val Workstream =
    "TODO(executor-backlog): its constructor is being reshaped by a parallel workstream; take `threadPools` once that lands"

  /** (file, rule) → why that file may still offend that rule. */
  private val Allowlist: Map[(String, Rule), String] = Map(
    ("common/src/main/scala/tools/BoundedParallel.scala", Rule.PoolBuilt)                          -> Scoped,
    ("worker/src/main/scala/tools/ConcurrentCandidateProbe.scala", Rule.PoolBuilt)                 -> Scoped,
    ("scrapers/src/main/scala/tools/ParallelDetailFetch.scala", Rule.PoolBuilt)                    ->
      (Scoped + "; reached from the catalog's diagnostic door, which the identity tools compile into older refs"),
    ("identity/src/main/scala/services/identity/PopulationMonotonicity.scala", Rule.PoolBuilt)     -> Scoped,
    ("identity/src/main/scala/services/identity/FamilyIndex.scala", Rule.PoolBuilt)                ->
      (Scoped + "; and the resolver's closure: touching it re-resolves every worker on deploy"),
    ("identity/src/main/scala/services/identity/Regions.scala", Rule.PoolBuilt)                    ->
      (Scoped + "; and the resolver's closure: touching it re-resolves every worker on deploy"),
    ("scrapers/src/main/scala/services/cinemas/roster/CopiedFeedDetector.scala", Rule.PoolBuilt)   -> Workstream,
    ("common/src/main/scala/services/movies/FilmRereads.scala", Rule.PoolBuilt)                    ->
      (Workstream + " (the debounce's scheduler, built inside MovieChangeStream, in MongoMovieChangeFeed, the repository workstream's)"),
    ("common/src/main/scala/services/movies/FilmRereads.scala", Rule.ThreadSleep)                  ->
      (Workstream + " (the re-read back-off; built inside MovieChangeStream, in MongoMovieChangeFeed, the repository workstream's)"),
    ("common/src/main/scala/services/movies/ChangeStreamReopen.scala", Rule.PoolBuilt)             ->
      (Workstream + " (built inside TaskInsertStream, the task-queue workstream's)"),
    ("common/src/main/scala/tools/PacedHttpFetch.scala", Rule.ThreadSleep)                         -> SleepSeamDefault,
    ("http/src/main/scala/tools/RetryWithBackoff.scala", Rule.ThreadSleep)                         -> SleepSeamDefault,
    ("worker/src/main/scala/tools/ThrottledHttpFetch.scala", Rule.ThreadSleep)                     -> SleepSeamDefault,
    ("common/src/main/scala/tools/MongoIndex.scala", Rule.ThreadSleep)                          ->
      ("an index conversion's retry back-off, reached from every repository's index setup with no wiring between; " +
        "the retry itself takes its sleep (`retryRaced`), which its spec drives")
  )

  private def exempt(file: String, source: String): Boolean =
    Seams.contains(file) || file.contains("/src/main/scala/modules/") || definesMain(source)

  private lazy val found: Seq[(String, Int, Rule, String)] = for {
    path                 <- scalaFiles(MainRoots)
    file                  = path.toString
    source                = read(path)
    if !exempt(file, source)
    (line, rule, text)   <- offenses(source)
  } yield (file, line, rule, text)

  "Main sources" should "take their pools and their sleep from the composition root, outside the allowlist" in {
    val unexplained = found.filterNot { case (file, _, rule, _) => Allowlist.contains(file -> rule) }
    withClue(unexplained.map { case (file, line, rule, text) => s"$file:$line: [$rule] $text" }.mkString(
      "\nThese lines build their own pool or sleep on the process's time. Take a `threadPools: ThreadPools` (or the " +
        "executor) and a `sleep: FiniteDuration => Unit`, passed from the wiring (`threadPools`, `sleep`), or add " +
        "(file, rule) -> WHY to Allowlist:\n", "\n", "\n"))(unexplained shouldBe empty)
  }

  "The allowlist" should "name only files that still offend that rule" in {
    val stale = Allowlist.keySet -- found.map { case (file, _, rule, _) => file -> rule }.toSet
    withClue("drop these Allowlist entries — the file no longer offends: ")(stale shouldBe empty)
    Allowlist.keys.map(_._1).filterNot(f => Files.exists(Paths.get(f))) shouldBe empty
  }

  "The lint" should "flag what builds a pool or sleeps, and nothing that only uses one" in {
    val src =
      """private val scheduler = DaemonExecutors.scheduler("x")
        |val ec = tools.DaemonExecutors.virtualThreadEC("y")
        |  executor.shutdownNow().forEach(DaemonExecutors.cancelUnrun)
        |private val scheduler = threadPools.scheduler("x")
        |Thread.sleep(50)
        |  sleep: Long => Unit = Thread.sleep,
        |TimeUnit.SECONDS.sleep(1)
        |java.util.concurrent.TimeUnit.MILLISECONDS.sleep(5)
        |sleep(grace)
        |// Thread.sleep(1) in a comment
        |val pool = DaemonExecutors.boundedPool("p", 2, 8, WhenFull.RunOnCaller)
        |""".stripMargin
    offenses(src).map { case (line, rule, _) => line -> rule } shouldBe Seq(
      1 -> Rule.PoolBuilt, 2 -> Rule.PoolBuilt, 5 -> Rule.ThreadSleep, 6 -> Rule.ThreadSleep,
      7 -> Rule.TimeUnitSleep, 8 -> Rule.TimeUnitSleep, 11 -> Rule.PoolBuilt)
  }

  // the mutation proof: each rule catches a shape no other rule does, so dropping any one lets that shape through
  "every rule" should "be the only one catching its planted shape" in {
    val planted = Map(
      Rule.PoolBuilt     -> """private val scheduler = DaemonExecutors.scheduler("detail-reaper")""",
      Rule.ThreadSleep   -> """Thread.sleep(waitSeconds * 1000L)""",
      Rule.TimeUnitSleep -> """TimeUnit.SECONDS.sleep(5)""")
    planted.keySet shouldBe Rule.values.toSet
    planted.foreach { case (rule, shape) =>
      withClue(s"$rule: ")(offenses(shape).map(_._2) shouldBe Seq(rule))
      withClue(s"without $rule: ")(offenses(shape, Rule.values.toSet - rule) shouldBe empty)
    }
  }

  it should "see the migrated services' former shapes, so the migration cannot quietly come back" in {
    Seq(
      """  private val reaper: ScheduledExecutorService = DaemonExecutors.scheduler("task-reaper")""",
      """  scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("scrape-reaper"),""",
      """  executionContext: ExecutionContext = DaemonExecutors.virtualThreadEC("bulk-refresh")""",
      """      Thread.sleep(driftSettle.value.toMillis)""",
      """      while (!liveness.appliedThrough(handedOff) && System.nanoTime() < deadline) Thread.sleep(50)""")
      .foreach(line => withClue(line)(offenses(line) should not be empty))
  }
}

object NoUnwiredThreadsAndWaitsSpec {

  enum Rule { case PoolBuilt, ThreadSleep, TimeUnitSleep }

  /** The `DaemonExecutors` members that build a pool — not `cancelUnrun`, which only handles a dropped task. */
  private val PoolBuilder =
    """\bDaemonExecutors\s*\.\s*(?:virtualThreadEC|boundedEC|scheduler|boundedPool|singleThreadExecutor|directExecutor|virtualThreadExecutor)\b""".r
  private val ThreadSleepRead   = """\bThread\s*\.\s*sleep\b""".r
  private val TimeUnitSleepRead = """\bTimeUnit\s*\.\s*[A-Z]+\s*\.\s*sleep\s*\(""".r

  private def patternOf(rule: Rule) = rule match {
    case Rule.PoolBuilt     => PoolBuilder
    case Rule.ThreadSleep   => ThreadSleepRead
    case Rule.TimeUnitSleep => TimeUnitSleepRead
  }

  /** Each (1-based line, rule, trimmed line) of `source` that `rules` refuse; comments ignored. */
  private[tools] def offenses(source: String, rules: Set[Rule] = Rule.values.toSet): Seq[(Int, Rule, String)] =
    source.linesIterator.zipWithIndex.toSeq.flatMap { case (line, index) =>
      val text = code(line)
      Rule.values.toSeq.filter(rules.contains).filter(rule => patternOf(rule).findFirstIn(text).isDefined)
        .map(rule => (index + 1, rule, line.trim))
    }
}
