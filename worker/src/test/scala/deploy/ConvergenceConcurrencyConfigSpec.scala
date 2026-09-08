package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks BOTH convergence workflows to the same lane shape: one constant group,
 * `cancel-in-progress: false`.
 *
 * They agree because they answer the same question — a queued-not-cancelled push
 * finishes what is running, holds at most one newer run pending, and drops
 * everything queued in between. Until 2026-09-08 `country-convergence.yml` used
 * `cancel-in-progress: true` instead, on the reasoning that its four legs were
 * cheap enough to throw away (12-73 warm minutes) that one authoritative answer
 * per push beat several competing ones. That stopped being true once cancelling
 * meant destroying real progress on a suite frequent pushes could otherwise starve
 * indefinitely — the same failure `us-convergence.yml` was split out to avoid for
 * the United States, whose leg has no warm tree to fall back on. Both workflows
 * now take the trade the US always took, so this spec asserts one shape twice.
 *
 * The group MUST stay a constant, with nothing per-run in it — GitHub collapses
 * runs only when the group string is EQUAL, and every expression added to it
 * silently buys another lane:
 *
 *   - `${{ github.sha }}`/`${{ github.event_name }}` — the original keying, which
 *     gave every deploy, nightly and manual run its own lane, so nothing ever
 *     cancelled or queued behind anything.
 *   - `${{ github.ref }}` — looks like it only splits main from the odd feature
 *     branch, but a co-agent dispatching from its own worktree branch is the
 *     normal way this suite gets run by hand. On 2026-08-01 a
 *     `convergence-imdb-ladder` dispatch and a main deploy dispatch ran side by
 *     side for an hour, six heavyweight legs at once, because
 *     `refs/heads/convergence-imdb-ladder` != `refs/heads/main`.
 */
class ConvergenceConcurrencyConfigSpec extends AnyFlatSpec with Matchers {
  private def concurrencyOf(path: String) = RepoFile.block(RepoFile.read(path), "concurrency")

  private val Files = Seq(".github/workflows/country-convergence.yml", ".github/workflows/us-convergence.yml")

  "both convergence workflows" should "queue behind the run in flight rather than cancel it" in {
    Files.foreach { path =>
      withClue(s"$path: ")(concurrencyOf(path) should include("cancel-in-progress: false"))
    }
  }

  it should "not key their lane on anything that varies per run" in {
    Files.foreach { path =>
      val group = concurrencyOf(path).linesIterator.find(_.trim.startsWith("group:")).getOrElse("")
      withClue(s"$path: ")(group should not include "${{")
    }
  }
}
