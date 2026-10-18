package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A `run:` block never reads a command's exit status (`$?`, `${PIPESTATUS[..]}`) on the line after the command while
 * `-e` is on.
 *
 * GitHub runs a `run:` block under `bash -e` (`shell: bash`: `bash --noprofile --norc -eo pipefail`, a composite
 * action's included), and a script's own `set -uo pipefail` leaves `-e` on. So in
 *
 *     good=$(convergence-bisect.sh last-green); found=$?
 *     if [ "$found" -eq 1 ]; then ...
 *
 * a failing command ends the step on its own line: the branches that read its status never run. convergence-bisect.yml's
 * "no green leg, nothing to bisect" (exit 0) failed the job instead, nix-stage-closures' and hard-clusters-ratchet's
 * annotations of what failed were never printed (2026-10-18). The status is read with `cmd || status=$?` (after
 * `status=0`), or after a `set +e`.
 */
class NoExitStatusReadUnderErrexitSpec extends AnyFlatSpec with Matchers {
  import NoExitStatusReadUnderErrexitSpec.unguardedStatusReads

  "every run: block CI runs" should "read an exit status only where -e cannot have ended the step first" in {
    val found = for {
      path <- RepoFile.ciFiles()
      step <- RepoFile.runSteps(RepoFile.read(path))
      line <- unguardedStatusReads(step.script)
    } yield s"$path: $line"
    withClue("read the status as `cmd || status=$?` (after `status=0`), or after `set +e`:\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  "the rule" should "flag a status read on the line after its command, or after it on the same line" in {
    unguardedStatusReads("set -uo pipefail\nrun-it\nstatus=$?") shouldBe Seq("status=$?")
    unguardedStatusReads("good=$(find-it); found=$?") shouldBe Seq("good=$(find-it); found=$?")
    unguardedStatusReads("a | tee log\nstatus=${PIPESTATUS[0]}") shouldBe Seq("status=${PIPESTATUS[0]}")
  }

  it should "pass a status read the command's failure cannot skip" in {
    unguardedStatusReads("found=0\ngood=$(find-it) || found=$?") shouldBe empty
    unguardedStatusReads("set +e\nrun-it\nstatus=$?") shouldBe empty
    unguardedStatusReads("set +e\na | tee log\nstatus=${PIPESTATUS[0]}\nset -e") shouldBe empty
  }
}

object NoExitStatusReadUnderErrexitSpec {
  private val StatusRead = """(?:^|;\s*)[A-Za-z_][A-Za-z_0-9]*=(?:\$\?|\$\{PIPESTATUS\[)""".r

  /** The lines of a `run:` script (trimmed, comments dropped) reading an exit status that `-e` can have skipped: no
   *  `set +e` before them, and no `||` guarding the command on their own line. */
  def unguardedStatusReads(script: String): Seq[String] = {
    val lines = script.linesIterator.map(_.trim).toVector
    lines.indices.collect {
      case at if StatusRead.findFirstMatchIn(lines(at)).exists { m =>
            val guardedHere = lines(at).take(m.start).contains("||")
            val errexitOff = lines.take(at).exists(_.matches("""set\s+\+e\b.*"""))
            !guardedHere && !errexitOff
          } => lines(at)
    }
  }
}
