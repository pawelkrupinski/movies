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

  it should "rotate the fixed-name heap dump before pruning, so a second OOM can still write one" in {
    // `-XX:HeapDumpPath=/data/heapdumps` names a DIRECTORY, so the JVM chooses the
    // filename: `java_pid<pid>.hprof`. A containerised JVM is always pid 1, so the
    // name never varies — the dump is written once and every later OOM dies with
    // "Unable to create /data/heapdumps/java_pid1.hprof: File exists", silently
    // losing the evidence. worker-us's 2026-09-03T03:40 heap OOM was diagnosed from
    // the container's stdout precisely because its dump had been swallowed this way.
    // Renaming on boot restores the dump AND feeds the keep-3-newest prune the
    // distinct filenames it was written to assume.
    dockerfile should include ("mv /data/heapdumps/java_pid1.hprof")
  }

  it should "rotate that dump BEFORE the prune runs, not after" in {
    // Order is the whole point: pruning first would still leave the fixed name in
    // place (it is the only dump, so `tail -n +4` never selects it) and the next OOM
    // would fail to write exactly as before.
    val rotate = dockerfile.indexOf("mv /data/heapdumps/java_pid1.hprof")
    val prune  = dockerfile.indexOf("ls -1t /data/heapdumps/*.hprof")
    rotate should be >= 0
    prune  should be >= 0
    rotate should be < prune
  }
}
