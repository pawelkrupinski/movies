package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*

/**
 * A FIXTURE SERVER THAT DIES BEFORE WRITING ITS PORT IS STARTED AGAIN, NOT
 * WAITED FOR. That is the whole of this spec.
 *
 * Every Playwright row and the mobile LocalServer job boot `FixtureServerMain`
 * under sbt in the background and poll for the port file it writes. On
 * 2026-09-06 one WebKit shard's sbt launcher hit a `Connection timed out`
 * fetching a boot jar from Maven Central and exited within seconds; the inline
 * wait loop only ever looked for the port file, so it sat out its entire 600s
 * ceiling on a dead process and failed a run that nineteen sibling shards had
 * passed. The loop is now `.github/scripts/fixture-server.sh`, and this spec
 * drives it with a fake server that dies on command.
 */
class FixtureServerBootSpec extends AnyFlatSpec with Matchers {

  private val Script = ".github/scripts/fixture-server.sh"

  /** Poll fast: the assertions below are about behaviour, not the 2s cadence CI uses. */
  private val FastPolling = Seq("FIXTURE_POLL_SECONDS" -> "0.2")

  private def run(args: String*)(env: (String, String)*): (Int, String) = {
    val out  = new StringBuilder
    val code = Process(Seq("bash", Script) ++ args, None, env*).!(ProcessLogger(line => out.append(line).append('\n')))
    (code, out.toString)
  }

  /**
   * A stand-in for `sbt runMain FixtureServerMain <port-file>`: exits non-zero
   * for its first `failures` launches, then writes a port and stays up like a
   * server would. `exec` so the recorded pid IS the long-lived process and
   * `stop` can be checked against it.
   */
  private def fakeServer(dir: Path, failures: Int): Path = {
    val script = dir.resolve("fake-server.sh")
    Files.writeString(
      script,
      s"""#!/usr/bin/env bash
         |n=$$(cat "$dir/launches" 2>/dev/null || echo 0)
         |n=$$((n + 1))
         |echo $$n >"$dir/launches"
         |if [ $$n -le $failures ]; then echo "boom on launch $$n"; exit 1; fi
         |echo 4321 >"$$1"
         |exec sleep 60
         |""".stripMargin,
    )
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))
    script
  }

  private def withState(test: Path => Unit): Unit = {
    val dir = Files.createTempDirectory("fixture-server-boot")
    try test(dir)
    finally {
      run("stop", dir.toString)()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
    }
  }

  private def pidIsAlive(state: Path): Boolean =
    Seq("kill", "-0", Files.readString(state.resolve("pid")).trim).! == 0

  "a launch that exits before writing its port" should "be started again, and the port come from the relaunch" in withState { dir =>
    val server = fakeServer(dir, failures = 1)
    run("launch", dir.toString, server.toString, dir.resolve("port").toString)() shouldBe (0, "")

    val started = System.nanoTime()
    val (code, out) = run("await", dir.toString)(FastPolling*)
    val elapsedSeconds = (System.nanoTime() - started) / 1e9

    withClue(out)(code shouldBe 0)
    out should include("relaunching (2 of 3)")
    Files.readString(dir.resolve("port")).trim shouldBe "4321"
    Files.readString(dir.resolve("launches")).trim shouldBe "2"
    withClue("the relaunch waited out the ceiling instead of noticing the exit: ")(elapsedSeconds should be < 30.0)
  }

  it should "leave `stop` pointing at the relaunched server, not the dead first launch" in withState { dir =>
    val server = fakeServer(dir, failures = 1)
    run("launch", dir.toString, server.toString, dir.resolve("port").toString)()
    run("await", dir.toString)(FastPolling*)._1 shouldBe 0

    pidIsAlive(dir) shouldBe true
    run("stop", dir.toString)()._1 shouldBe 0
    Thread.sleep(200)
    pidIsAlive(dir) shouldBe false
  }

  "a server that never manages to start" should "fail after the configured launches, with every attempt's log, and well inside the ceiling" in withState { dir =>
    val server = fakeServer(dir, failures = 99)
    run("launch", dir.toString, server.toString, dir.resolve("port").toString)()

    val started = System.nanoTime()
    val (code, out) = run("await", dir.toString)(FastPolling :+ ("FIXTURE_BOOT_CEILING_SECONDS" -> "600")*)
    val elapsedSeconds = (System.nanoTime() - started) / 1e9

    code shouldBe 1
    out should include("exited before writing a port on all 3 launches")
    out should include("boom on launch 1")
    out should include("boom on launch 3")
    Files.readString(dir.resolve("launches")).trim shouldBe "3"
    withClue("a dead process should be noticed on the next poll, not at the ceiling: ")(elapsedSeconds should be < 30.0)
  }

  "a server that is alive but never writes a port" should "still fail at the ceiling — the relaunch must not mask a hang" in withState { dir =>
    val server = dir.resolve("hung-server.sh")
    Files.writeString(server, "#!/usr/bin/env bash\nexec sleep 60\n")
    Files.setPosixFilePermissions(server, PosixFilePermissions.fromString("rwxr-xr-x"))
    run("launch", dir.toString, server.toString, dir.resolve("port").toString)()

    val (code, out) = run("await", dir.toString)(FastPolling :+ ("FIXTURE_BOOT_CEILING_SECONDS" -> "1")*)

    code shouldBe 1
    out should include("didn't write a port within 1s")
    out should not include "relaunching"
    pidIsAlive(dir) shouldBe true
  }

  /** Invoked by path from the workflow, so the mode bit is part of the contract, as for `matches.sh`. */
  "the script" should "be executable on the runner" in {
    Files.isExecutable(Paths.get(Script)) shouldBe true
  }

  /**
   * And both boots must actually go through it. An inline `nohup sbt … &` plus a
   * `for _ in $(seq …)` loop creeping back into either YAML would reinstate the
   * 600s wait on a dead process while every assertion above stayed green.
   */
  it should "be what every FixtureServerMain boot in CI uses" in {
    val pageTestRow = RepoFile.read(".github/actions/run-page-test/action.yml")
    val mobileJob   = RepoFile.block(RepoFile.read(".github/workflows/ci.yml"), "mobile-local-server")
    Seq("page-test row" -> pageTestRow, "mobile-local-server job" -> mobileJob).foreach { case (name, text) =>
      withClue(s"$name boots FixtureServerMain without the relaunching script: ") {
        text should include("fixture-server.sh launch")
        text should include("fixture-server.sh await")
        text should include("fixture-server.sh stop")
        text should not include "nohup sbt"
      }
    }
  }
}
