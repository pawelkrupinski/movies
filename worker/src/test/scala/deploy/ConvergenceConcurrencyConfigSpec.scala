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
 * indefinitely — the failure the United States, then cold, hit first (it ran from a
 * workflow of its own until its legs went hermetic; it is a row of this one since
 * 2026-09-30).
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
 *
 * ONE exception, by the user's choice (2026-10-15): main's runs share the one constant lane, and every run from any
 * OTHER branch -- a measurement's measure/ branch, a co-agent's own -- gets a lane of its own, keyed by its run id, so
 * several from one branch run at the same time and none queues behind or replaces another (five agents' measurements
 * had waited about an hour each on main's lane). The cost is the 2026-08-01 one above, accepted: branch runs side by
 * side with main's, past the 20-runner allowance, wait for runners rather than lanes.
 */
class ConvergenceConcurrencyConfigSpec extends AnyFlatSpec with Matchers {
  private def concurrencyOf(path: String) = RepoFile.block(RepoFile.read(path), "concurrency")

  private val Files = Seq(".github/workflows/identity-model-convergence.yml")

  "the convergence workflow" should "queue behind the run in flight rather than cancel it" in {
    Files.foreach { path =>
      withClue(s"$path: ")(concurrencyOf(path) should include("cancel-in-progress: false"))
    }
  }

  // keyed by the branch, never the run: Forgejo evaluates `concurrency:` before the run has an id, so `github.run_id` is
  // empty there and every measurement fell into one lane, ~35 runs queued behind each other (2026-10-21); each
  // measure/ branch is a run's own
  it should "key main's lane on nothing that varies per run, and give every other branch a lane of its own" in {
    Files.foreach { path =>
      val group = concurrencyOf(path).linesIterator.find(_.trim.startsWith("group:")).getOrElse("").trim
      withClue(s"$path: ")(group shouldBe
        "group: identity-model-convergence${{ github.ref != 'refs/heads/main' && format('-{0}', github.ref_name) || '' }}")
    }
  }
}

/**
 * `convergence-bisect.yml` once ran on EVERY completion of a convergence run — green, red, and
 * the pending runs the lanes above cancel whenever a newer push queues — and with one group per
 * suite and `cancel-in-progress: true`, each no-op completion cancelled a bisect up to 90 minutes
 * into its replays. Only a newer RED run may supersede a bisect (its range contains the older
 * one). It is now dispatched by the convergence run only when a leg on main failed
 * (ConvergenceBisectTriggerSpec), so every run of it is a red run's, and one cancelling lane per
 * suite is exactly that rule.
 */
class ConvergenceBisectConcurrencySpec extends AnyFlatSpec with Matchers {
  private val Workflow = RepoFile.read(".github/workflows/convergence-bisect.yml")
  private val concurrency = RepoFile.block(Workflow, "concurrency")

  "the convergence bisect" should "take one cancelling lane per suite" in {
    concurrency should include("group: convergence-bisect-${{ inputs.suite }}\n")
    concurrency should include("cancel-in-progress: true")
  }

  it should "be started only for a red leg on main, so only a newer red run can supersede it" in {
    Seq(".github/workflows/identity-model-convergence.yml").foreach { path =>
      val jobs = RepoFile.jobs(RepoFile.read(path))
      withClue(s"$path: ") {
        jobs("request-bisect") should include(
          "if: ${{ !cancelled() && needs.leg.result == 'failure' && needs.preflight.outputs.on-main == 'true' }}")
        RepoFile.block(jobs("preflight"), "outputs") should include("on-main: ${{ github.ref == 'refs/heads/main' }}")
      }
    }
  }
}
