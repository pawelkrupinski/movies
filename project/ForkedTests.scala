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
 * runs under that slot rather than taking one, which holds because the forks run ONE AT A TIME ([[oneForkAtATime]],
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

  /** At most one forked test JVM at a time, in every module and configuration: see THE JVM SLOT above. */
  val oneForkAtATime: Setting[_] = Global / concurrentRestrictions += Tags.limit(Tags.ForkedTestGroup, 1)

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
