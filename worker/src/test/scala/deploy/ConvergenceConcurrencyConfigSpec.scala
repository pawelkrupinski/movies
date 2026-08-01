package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the country-convergence suite to ONE global concurrency lane.
 *
 * The suite is three legs of up to 135 minutes each, and only the newest run's
 * result is ever read — so a run still in flight when a newer one starts is pure
 * runner-pool cost. GitHub only collapses runs whose `concurrency.group` string
 * is EQUAL, and every expression that varies per run silently buys another lane:
 *
 *   - `${{ github.sha }}`/`${{ github.event_name }}` — the original keying, which
 *     gave every deploy, nightly and manual run its own lane, so nothing ever
 *     cancelled anything.
 *   - `${{ github.ref }}` — looks like it only splits main from the odd feature
 *     branch, but the manual dispatch from a worktree branch is not rare here (a
 *     co-agent dispatching from its own branch is the normal way this suite gets
 *     run by hand). On 2026-08-01 a `convergence-imdb-ladder` dispatch and a
 *     main deploy dispatch ran side by side for an hour, six heavyweight legs at
 *     once, because `refs/heads/convergence-imdb-ladder` != `refs/heads/main`.
 *
 * A constant group makes the newest run supersede whatever is in flight, from
 * wherever it was dispatched. The cost is real and deliberate: a hand-dispatched
 * branch run is cancelled by the next push to main. That is the intended
 * trade — one authoritative answer beats several competing ones.
 */
class ConvergenceConcurrencyConfigSpec extends AnyFlatSpec with Matchers {
  private lazy val yml = RepoFile.read(".github/workflows/country-convergence.yml")

  private lazy val concurrency = RepoFile.block(yml, "concurrency")

  "the country convergence workflow" should "run in one lane, so a newer run supersedes the one in flight" in {
    concurrency should include("cancel-in-progress: true")
  }

  it should "not key that lane on anything that varies per run" in {
    val group = concurrency.linesIterator.find(_.trim.startsWith("group:")).getOrElse("")
    group should not include "${{"
  }
}
