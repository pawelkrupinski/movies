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
  private val MovingRef = """uses:\s*(\S+)@(master|main)\b""".r

  "every workflow action" should "be pinned to a version tag, not a moving branch" in {
    val workflows = RepoFile.workflows()
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

  /** `ubuntu-latest` is a label GitHub moves under us — to 26.04 from 2026-10-19
   *  (actions/runner-images#14748) — so a job on it changes OS with no commit here, and nothing
   *  to bisect when it turns red. Every job, and every runner input's default, names its image. */
  "every CI job" should "run on a named Ubuntu image, not ubuntu-latest" in {
    val floating = for {
      path <- RepoFile.ciFiles()
      line <- RepoFile.withoutComments(RepoFile.read(path)).linesIterator
      if line.contains("ubuntu-latest")
    } yield s"$path: ${line.trim}"

    withClue("`ubuntu-latest` moves to a new Ubuntu with no commit here: ") {
      floating shouldBe empty
    }
  }

  /** `swift-actions/setup-swift@v2` refuses any Ubuntu outside its own list
   *  (`Version "26.04" of Ubuntu is not supported`), and swift.org publishes no 5.10 build for
   *  26.04 — so a job that installs Swift stays on an image setup-swift knows. */
  private val SetupSwiftUbuntus = Set("20.04", "22.04", "24.04")

  "every job installing Swift via setup-swift" should "run on an Ubuntu setup-swift supports" in {
    // `ubuntu-24.04`, or ci.yml's `${{ inputs.lane || 'ubuntu-24.04' }}`, whose lane is Forgejo-only (MainLaneSpec).
    val RunsOn = """runs-on:\s*(?:\$\{\{ inputs\.lane \|\| ')?ubuntu-(\d\d\.\d\d)[\s']""".r
    val unsupported = for {
      file        <- RepoFile.workflows()
      (name, job) <- RepoFile.jobs(RepoFile.read(file.getPath))
      code         = RepoFile.withoutComments(job)
      if code.contains("swift-actions/setup-swift@")
      if !RunsOn.findFirstMatchIn(code).exists(m => SetupSwiftUbuntus(m.group(1)))
    } yield s"${file.getName}: $name"

    withClue(s"setup-swift@v2 runs only on Ubuntu ${SetupSwiftUbuntus.toSeq.sorted.mkString(" / ")}: ") {
      unsupported shouldBe empty
    }
  }

  /** Playwright's browser builds and the apt debs `--with-deps` pulls are per Ubuntu release
   *  (`chromium-<rev>` is the same directory name on 24.04 and 26.04). Keyed on `runner.os`
   *  alone, a 24.04 cache would restore onto a 26.04 runner and be reused forever. */
  "the Playwright browser and apt-deb caches" should "key on the runner image" in {
    val action = RepoFile.read(".github/actions/run-page-test/action.yml")
    val keys   = """(?m)^\s*key:\s*(playwright-.*)$""".r.findAllMatchIn(action).map(_.group(1)).toSeq
    keys should have size 2
    all(keys) should include ("steps.browser-slug.outputs.image")
  }
}
