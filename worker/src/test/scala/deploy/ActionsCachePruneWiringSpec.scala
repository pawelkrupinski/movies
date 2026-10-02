package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Main's Actions caches are pruned right after the workflows that fill them finish.
 *
 * At 10.8 GB against GitHub's 10 GB cap, GitHub evicted the least recently used entries,
 * and those were the Android jobs' Gradle homes (saved only on the rare android/ pushes).
 * Android's `build` job then restored ci.yml's `mobile-local-server` home instead, which
 * has no dex or R8 outputs, and re-ran both every time. Most of the 10.8 GB was superseded
 * per-commit entries (sbt-target-e2e-Linux-<hash> and friends) that no restore ever reads.
 * scripts/ci/prune-actions-caches.sh deletes those; its own spec,
 * scripts/ci/prune-actions-caches-test.sh, pins which entries, and runs in ci.yml.
 */
class ActionsCachePruneWiringSpec extends AnyFlatSpec with Matchers {

  private lazy val prune = RepoFile.read(".github/workflows/prune-actions-caches.yml")
  private lazy val ci = RepoFile.read(".github/workflows/ci.yml")

  /** `name:` of the workflows that save per-commit sbt-target or gradle-home caches on main and
   *  are started by a person or a cron, so their completion raises a `workflow_run`. The
   *  convergence workflows save caches too, but Main dispatches them with the GITHUB_TOKEN, whose
   *  runs raise none (ConvergenceBisectTriggerSpec); the daily schedule covers them. */
  private val CacheSavers = Seq("Main", "Android", "iOS", "Order independence", "Record scrape fixtures")

  "the prune workflow" should "run after every workflow that saves per-commit caches on main" in {
    prune should include ("workflow_run:")
    prune should include ("types: [completed]")
    prune should include ("branches: [main]")
    for (name <- CacheSavers) withClue(s"$name: ") {
      prune should include (s"- $name\n")
      RepoFile.workflows().exists(f => RepoFile.read(f.getPath).linesIterator.contains(s"name: $name")) shouldBe true
    }
  }

  it should "also run daily, for the savers it does not follow" in {
    prune should include ("schedule:")
  }

  it should "hold the actions:write a cache delete needs, and run one prune at a time" in {
    prune should include ("actions: write")
    prune should include ("group: prune-actions-caches")
    prune should include ("cancel-in-progress: true")
  }

  it should "delete for real, not in dry-run mode" in {
    val run = RepoFile.step(prune, "Delete superseded cache entries")
    run should include ("run: scripts/ci/prune-actions-caches.sh")
    run should not include "--dry-run"
  }

  "ci.yml" should "run the prune script's own spec" in {
    ci should include ("bash scripts/ci/prune-actions-caches-test.sh")
  }
}
