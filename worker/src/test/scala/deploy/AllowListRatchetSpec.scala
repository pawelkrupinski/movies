package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A NEW CHECK LANDS GREEN; ITS ALLOW-LIST ONLY SHRINKS (scripts/ci/allow_list_ratchet.py, convergence-failures-2026-10-29
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
    hook should include("python3 scripts/ci/allow_list_ratchet.py")
    hook should include("grep -qxF -- \"$path\" .github/allow-lists.txt && allowlists=1")
    hook should include("scripts/identity-drift-ci --head \"$head_sha\" --against main")
    RepoFile.read(".github/identity-drift-gate-paths.txt") should include("test/resources/fixtures/identity-flips-allow.tsv")
  }
}
