package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A NEW CHECK LANDS GREEN; ITS ALLOW-LIST ONLY SHRINKS (scripts/ci/commit_rules.py, convergence-failures-2026-10-29
 * recommendation 2). The ratchet judges the lists `.github/allow-lists.txt` registers, so every allow-list in the
 * repository must be registered there: a new check's list is under the ratchet from the commit that adds it, whoever
 * writes it and whatever the check is. 15 of that week's 36 red convergence runs came from two checks born red.
 */
class AllowListRatchetSpec extends AnyFlatSpec with Matchers {
  private val Registry = ".github/allow-lists.txt"
  private lazy val registered: Set[String] =
    RepoFile.read(Registry).linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toSet

  private val AllowListName = """(?i).*(-allow|allow-?list|allowlist)[^/]*\.(tsv|txt|csv)""".r
  private lazy val allowLists: Seq[String] =
    RepoFile
      .listed("test/resources/fixtures")(f => f.isFile && AllowListName.matches(f.getName))
      .map(_.getPath.stripPrefix("./"))

  "every allow-list in the fixtures" should "be registered, so the ratchet judges it" in {
    allowLists should not be empty
    allowLists.filterNot(registered) shouldBe empty
  }

  "every registered allow-list" should "exist" in {
    registered.filterNot(RepoFile.exists) shouldBe empty
  }

  "the pre-push hook" should "run the ratchet on a change to a registered list, and the birth check on the gate's files" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("python3 scripts/ci/commit_rules.py \"$1\"")
    hook should include("check_allowlists() { commit_rules allowlists; }")
    hook should include("grep -qxF -- \"$path\" .github/allow-lists.txt && allowlists=1")
    hook should include("scripts/identity-drift-ci --head \"$head_sha\" --against main")
    RepoFile.read(".github/identity-drift-gate-paths.txt") should include("test/resources/fixtures/identity-flips-allow.tsv")
  }

  /** `git push --no-verify` skips the hook, and with it every rule above: Main's CI and the landing check run them again
   *  over the commits, whatever paths they touched. */
  "Main's CI and the landing check" should "run the drift gate's commit rules, unselected by path" in {
    RepoFile.jobs(RepoFile.read(".github/workflows/ci.yml"))("commit-rules") should include("run: scripts/ci/commit-rules-ci.sh")
    RepoFile.read(".github/workflows/precommit-check.yml") should include(
      "scripts/ci/commit-rules-ci.sh --base \"$(git merge-base HEAD origin/main)\"")
    val script = RepoFile.read("scripts/ci/commit-rules-ci.sh")
    Seq("allowlists", "driftguards").foreach(set => script should include(set))
  }
}
