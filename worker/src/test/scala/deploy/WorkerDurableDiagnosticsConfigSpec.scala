package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the DURABLE-diagnostics launch wiring for the worker's chronic ~5 h
 * exit-code-3 native OOM: the JVM's dying stderr (the `ExitOnOutOfMemoryError`
 * malloc-failed line + the `PrintNMTStatistics` dump) and any hard-crash
 * `hs_err` file must land on the /data volume so they SURVIVE the restart —
 * log retention rolls them away first. This is config/launch, not reachable by a
 * running-JVM test layer, so we guard the rendered `Dockerfile` CMD instead: a
 * future edit that drops the redirect fails here. The actual redirect is only
 * verifiable on a real box (boot the container, kill -SIGTERM, read the file).
 *
 * The other half — `-XX:ErrorFile=` pointing at the same volume — is a JAVA_OPTS
 * flag, and JAVA_OPTS is per tier+country in the k3s overlays now that the
 * `fly.worker*.toml` configs are gone. `NodeMemoryBudgetSpec` guards it there,
 * for every overlay rather than for the one country that had a toml.
 *
 * Tests run with the repo root as CWD (the fixture specs load
 * `test/resources/...` the same way), so the top-level files resolve directly.
 */
class WorkerDurableDiagnosticsConfigSpec extends AnyFlatSpec with Matchers {
  private lazy val dockerfile = RepoFile.read("Dockerfile")

  "the Dockerfile CMD" should "redirect the worker JVM's stderr to a durable /data/logs file" in {
    // Appended so the pre-death readout survives across the restart.
    dockerfile should include ("2>> /data/logs/worker-stderr.log")
  }

  it should "keep the exec so the JVM stays PID-adjacent and receives SIGTERM directly" in {
    // The graceful NMT dump depends on SIGTERM reaching the JVM, not a shell wrapper.
    dockerfile should include ("exec bin/$BIN")
  }

  it should "prune old hs_err crash logs so they can't fill /data" in {
    dockerfile should include ("/data/logs/hs_err_*.log")
  }

  /**
   * THE HEAP-DUMP BOOT STEPS, both delegated to `bin/heap-dumps.sh` (infra/nix/files/heap-dumps.sh,
   * whose rules are pinned by infra/test/test_heap_dumps.sh). What this guards is the WIRING: that
   * the CMD still calls it, in the right order, and hands its file name to the JVM.
   */
  it should "prune this pod's heap-dump directory before the JVM starts, without letting a prune failure block the boot" in {
    dockerfile should include ("bin/heap-dumps.sh prune /data/heapdumps || true")
  }

  it should "give every JVM start its own -XX:HeapDumpPath FILE, not the directory" in {
    // `-XX:HeapDumpPath=/data/heapdumps` names a DIRECTORY, so the JVM chooses the
    // filename: `java_pid<pid>.hprof`. A containerised JVM is always pid 1, so the
    // name never varies — the dump is written once and every later OOM dies with
    // "Unable to create /data/heapdumps/java_pid1.hprof: File exists", silently
    // losing the evidence (worker-us, 2026-09-03). A per-start file named for the pod,
    // country and start time cannot collide, and appending it to JAVA_OPTS overrides the
    // ConfigMap's directory-form flag because the last -XX occurrence wins.
    dockerfile should include ("dump=$(bin/heap-dumps.sh dump-file /data/heapdumps)")
    dockerfile should include ("""export JAVA_OPTS="$JAVA_OPTS -XX:HeapDumpPath=$dump"""")
  }

  it should "prune BEFORE naming the new dump, and do both before the JVM is launched" in {
    // Pruning after the JVM started would race its own OOM; and the export must precede the
    // launch or the launcher never sees the file.
    val prune  = dockerfile.indexOf("bin/heap-dumps.sh prune")
    val name   = dockerfile.indexOf("bin/heap-dumps.sh dump-file")
    val launch = dockerfile.indexOf("launch() {")
    prune  should be >= 0
    name   should be > prune
    launch should be > name
  }

  "build.sbt" should "ship the heap-dump script in both the web and the worker dist" in {
    // The CMD above runs `bin/heap-dumps.sh` in BOTH images; a dist without it would log
    // "not found" and boot with the ConfigMap's directory-form flag, i.e. the pid-1 collision.
    val build = RepoFile.read("build.sbt")
    build should include (""""infra" / "nix" / "files" / "heap-dumps.sh") -> "bin/heap-dumps.sh"""")
    "heapDumpScript\\)".r.findAllIn(build).size shouldBe 2
  }
}
