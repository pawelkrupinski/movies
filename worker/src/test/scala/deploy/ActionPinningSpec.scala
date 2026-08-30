package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every workflow action must name a version tag, never a moving branch.
 *
 * `@master` re-resolves on every run, so the action a workflow executes is
 * whatever upstream pushed last — the build is not reproducible, an upstream
 * regression lands with no commit here, and there is nothing to bisect when a
 * step starts behaving differently. It is also the shape a compromised action
 * rides in on, since a branch can be force-pushed and a release tag usually is
 * not.
 *
 * `superfly/flyctl-actions/setup-flyctl@master` was the only one left in this
 * repo — every other action here is already `@v<major>` — and it is the step
 * that flaked the deploy on 2026-08-30. Its `v1` tag pointed at exactly the
 * commit `master` did (`ed8efb33`) when it was pinned, so the pin changed
 * nothing about what runs; it only stopped the version from drifting silently.
 *
 * A version tag rather than a SHA is deliberate: it matches how the other
 * twenty-odd actions here are pinned, and a major tag still collects compatible
 * upstream fixes. Nothing in this repo bumps a bare SHA, so one would rot.
 *
 * Tests run with the repo root as CWD, so the workflow directory resolves
 * directly.
 */
class ActionPinningSpec extends AnyFlatSpec with Matchers {
  private def workflows: Seq[java.io.File] =
    Option(new java.io.File(".github/workflows").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(f => f.getName.endsWith(".yml") || f.getName.endsWith(".yaml"))
      .sortBy(_.getName)
      .toSeq

  private val MovingRef = """uses:\s*(\S+)@(master|main)\b""".r

  "every workflow action" should "be pinned to a version tag, not a moving branch" in {
    workflows should not be empty

    val floating = for {
      file <- workflows
      line <- RepoFile.read(file.getPath).linesIterator.filterNot(_.trim.startsWith("#"))
      hit  <- MovingRef.findFirstMatchIn(line)
    } yield s"${file.getName}: ${hit.group(1)}@${hit.group(2)}"

    withClue("an action on a moving branch re-resolves on every run: ") {
      floating shouldBe empty
    }
  }
}
