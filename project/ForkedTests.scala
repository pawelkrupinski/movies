import sbt._
import sbt.Keys._

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters._

/**
 * Runs a module's unit specs in a test JVM of their own, forked by the sbt server, with a heap sized for that module.
 *
 * WHY. Unforked, every module's specs ran inside the one kept sbt server (`scripts/sbt-warm`), and what a spec leaves
 * behind stayed there: a scheduler a wiring started and never stopped ("tmdb-ask", "venue-closure", "pool-N-thread-N")
 * keeps its run's test classloader alive, and with it every class and every static that run created. Measured
 * 2026-10-19 in one server: each `worker/Test/test` added ~260 MB of live heap and ~200 MB of metaspace (37k classes)
 * that no GC could take back, so the second or third `testUnit` an agent ran in a warm server died — "sbt server
 * disconnected", an `oom-*.hprof` in target/, or a HotSpot crash with 1.4 GB of metaspace. A forked JVM ends with its
 * run and takes all of it along; the server keeps only the build.
 *
 * THE JVM SLOT (scripts/jvm-gate.sh). A forked test JVM is a child of the server, which holds the checkout's slot; it
 * runs under that slot rather than taking one, which holds because the forks run ONE AT A TIME ([[forkSlots]],
 * sbt's own default made explicit) while the server waits on them: a slot is the server plus one test JVM sized for
 * the same cores (the server's `-XX:ActiveProcessorCount`, which sbt-warm sets, is passed on) with a heap capped per
 * module. Heaps measured 2026-10-19: worker's live set peaks at ~1.15 GB (a 1.5 GB run needed one full compaction),
 * identity's ~0.5 GB, web's ~0.3 GB, the rest under 150 MB. Serial forks cost `testUnit` ~12% wall time (331 s against 294 s).
 *
 * The working directory is the repository root, as it was in the server: specs read `test/resources/...` by a
 * root-relative path.
 */
object ForkedTests {

  /** The flags every test JVM is pinned to — the same ones `.jvmopts` gives the server. `TestJvmContract` guards the
   *  headless and locale pins from inside each forked JVM. */
  val pinnedOptions: Seq[String] = Seq(
    // AWT rendering in a non-headless JVM makes it a foreground macOS app that steals focus.
    "-Djava.awt.headless=true",
    // Twirl's `f"…%.1f"` formats with the default locale; prod's image is en_US.
    "-Duser.language=en",
    "-Duser.country=US",
    // The worker's small-Integer cache (infra/jvm/worker.options): specs box the model's ids the way production does.
    "-XX:AutoBoxCacheMax=16384",
  )

  /** The server's own `-XX:ActiveProcessorCount`, when it has one (sbt-warm sizes it for the JVM slot). */
  private def serverProcessorCount: Seq[String] =
    ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.filter(_.startsWith("-XX:ActiveProcessorCount=")).toSeq

  /** How many forked test JVMs run at once. ONE in a person's or an agent's sbt: see THE JVM SLOT above. On a CI runner
   *  (`CI=true`), which runs one job and nothing else, THREE: every module of a unit row in a fork of its own at once (a
   *  row is at most three modules: worker, web and scrapers; common, identity and testkit). A module's specs leave cores
   *  idle behind their longest suites, and the other modules' fill them. Measured on main-lane, the two multi-module rows
   *  side by side in each of three runs at host load 82-213 (measure runs 3468, 3475, 3480), test step in s:
   *  `worker` 368/371/338 at one fork, 288/276/274 at two, 262/241/253 at three; `rest` 284/263/268, 274/243/259,
   *  273/244/251. Three heaps of at most 2 GB, 1 GB and 512 MB beside the server fit a runner. `KINOWO_TEST_FORKS=<n>`
   *  overrides either. */
  val forksAtOnce: Int =
    sys.env.get("KINOWO_TEST_FORKS").flatMap(n => scala.util.Try(n.trim.toInt).toOption).filter(_ >= 1)
      .getOrElse(if (sys.env.get("CI").contains("true")) 3 else 1)

  /** At most [[forksAtOnce]] forked test JVMs at a time, in every module and configuration. IN PLACE OF sbt's own
   *  `Tags.limit(Tags.ForkedTestGroup, 1)` (Defaults.defaultRestrictions), not beside it: every rule applies, so with
   *  both the tighter one won and CI ran its test forks one at a time after 5a4cb6949 too (`show Global /
   *  concurrentRestrictions` listed "Limit forked-test-group to 1" beside "to 2"; the `worker` row ran worker's,
   *  scrapers' and web's forks one after another, 2026-10-28). */
  val forkSlots: Setting[_] = Global / concurrentRestrictions := {
    // sbt's rules are opaque predicates over the running tasks' tag counts: its fork limit is the one that refuses a
    // second forked test group alone.
    val forkLimit: Tags.Rule => Boolean = rule =>
      rule(Map(Tags.ForkedTestGroup -> 1)) && !rule(Map(Tags.ForkedTestGroup -> 2))
    // sbt's limit on all tasks at once (its default: the cores), the one that also caps how many unforked suites run
    // side by side. `KINOWO_SBT_TASKS=<n>` replaces it: the integration rows' suites mostly wait on Mongo.
    val allLimit: Tags.Rule => Boolean = rule => rule(Map(Tags.All -> 1)) && !rule(Map(Tags.All -> 100000))
    val base = (Global / concurrentRestrictions).value.filterNot(forkLimit) :+ Tags.limit(Tags.ForkedTestGroup, forksAtOnce)
    tasksAtOnce.fold(base)(n => base.filterNot(allLimit) :+ Tags.limitAll(n))
  }

  /** `KINOWO_SBT_TASKS`, when set: how many sbt tasks run at once, in place of sbt's one per core. */
  val tasksAtOnce: Option[Int] =
    sys.env.get("KINOWO_SBT_TASKS").flatMap(n => scala.util.Try(n.trim.toInt).toOption).filter(_ >= 1)

  /** `config`'s specs in a forked JVM with at most `heap` (an `-Xmx` value, e.g. "2g"). */
  def settings(config: Configuration, heap: String): Seq[Setting[_]] = Seq(
    config / fork := true,
    // The SPECS fork; a tool run from the configuration (`worker/Test/runMain scripts.UnmatchedClustersRatchet`, the
    // recorder) does not. Forked, `run` took the module's directory as its working directory -- `run / forkOptions`
    // is not the specs' -- so every repo-relative path a tool reads went missing (identity-judgements.tsv: the takes
    // check's base ratchet, Forgejo identity-model-convergence run 195), and its heap was the specs' cap, not the
    // -J-Xmx its caller gives sbt. In the server, as before the specs forked.
    config / run / fork     := false,
    config / runMain / fork := false,
    // Suites in parallel inside the fork, as they ran in the server — unless the order-independence run made them
    // sequential (TEST_ORDER_SEED sets parallelExecution false).
    config / testForkedParallel := (config / parallelExecution).value,
    config / forkOptions := (config / forkOptions).value.withWorkingDirectory((LocalRootProject / baseDirectory).value),
    config / javaOptions ++= options(config, heap).value,
  )

  /** A forked test JVM's options: the pins, the server's core count, the heap cap, and a dump on OOM named for the
   *  module. Web's Test JVM, which Play forks, takes these alone. */
  def options(config: Configuration, heap: String): Def.Initialize[Seq[String]] = Def.setting {
    pinnedOptions ++ serverProcessorCount ++ Seq(
      s"-Xmx$heap",
      "-XX:+ExitOnOutOfMemoryError",
      "-XX:+HeapDumpOnOutOfMemoryError",
      s"-XX:HeapDumpPath=target/oom-${thisProject.value.id}-${config.name}-%p.hprof",
    )
  }
}
