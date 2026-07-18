package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the DURABLE-diagnostics launch wiring for the worker's chronic ~5 h
 * exit-code-3 native OOM: the JVM's dying stderr (the `ExitOnOutOfMemoryError`
 * malloc-failed line + the `PrintNMTStatistics` dump) and any hard-crash
 * `hs_err` file must land on the /data volume so they SURVIVE the restart —
 * `flyctl logs` retention rolls them away first. This is config/launch, not
 * reachable by a running-JVM test layer, so we guard the rendered `Dockerfile`
 * CMD and `fly.worker.toml` JAVA_OPTS instead: a future edit that drops the
 * redirect or the ErrorFile flag fails here. The actual redirect is only
 * verifiable on a real box (boot the container, kill -SIGTERM, read the file).
 *
 * Tests run with the repo root as CWD (the fixture specs load
 * `test/resources/...` the same way), so the top-level files resolve directly.
 */
class WorkerDurableDiagnosticsConfigSpec extends AnyFlatSpec with Matchers {
  private lazy val dockerfile  = RepoFile.read("Dockerfile")
  private lazy val workerToml  = RepoFile.read("fly.worker.toml")

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

  "fly.worker.toml JAVA_OPTS" should "point the hard-crash ErrorFile at the /data volume" in {
    workerToml should include ("-XX:ErrorFile=/data/logs/hs_err_%p.log")
  }
}
