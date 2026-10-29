package deploy

import tools.ScratchDirectories
import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * identity-refit.yml's `refit` job merges the four shards' round 1 when nothing they measured moved between the
 * triggering commit (what the shards checked out) and main's tip (what the job checks out, `ref: main`, depth 1).
 *
 * The step's `git diff <triggering sha> HEAD` ran in that depth-1 clone, where the triggering commit is absent the
 * moment main has moved past it: the diff died 128, read as "moved", and every such run re-measured round 1 whole on one
 * runner -- also when main had moved only docs. The step runs here for real, in a depth-1 clone like the job's.
 */
class IdentityRefitShardMergeSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private val Step = RepoFile.stepScript(RepoFile.read(".github/workflows/identity-refit.yml"), "Merge the shards' round 1?")

  /** What the step decides in a depth-1 clone of main, the shards having measured `measured`. */
  private def merges(origin: ScratchGitRepository, measured: String): Boolean = {
    val clone = new ScratchGitRepository(tempDirectory)
    clone.git("fetch", "-q", "--depth=1", s"file://${origin.root}", "main")
    clone.git("checkout", "-q", "FETCH_HEAD")
    clone.git("remote", "add", "origin", s"file://${origin.root}")
    val output = tempFile("github-output", ".txt")
    val script = clone.script("merge.sh", Step.replace("${{ github.sha }}", measured))
    clone.process(Seq("bash", "-eo", "pipefail", script.toString), "GITHUB_OUTPUT" -> output.toString).!
    Files.readString(output).contains("merge=shards")
  }

  "the shard merge" should "merge when main moved only what the shards did not measure with" in {
    val origin = new ScratchGitRepository(tempDirectory)
    val measured = origin.commit("measured", "common/A.scala" -> "a", "README" -> "r")
    origin.commit("docs only", "README" -> "r2")
    merges(origin, measured) shouldBe true
  }

  it should "re-measure when main moved the code the shards measured" in {
    val origin = new ScratchGitRepository(tempDirectory)
    val measured = origin.commit("measured", "common/A.scala" -> "a")
    origin.commit("code", "common/A.scala" -> "b")
    merges(origin, measured) shouldBe false
  }

  it should "merge when main is the commit the shards measured" in {
    val origin = new ScratchGitRepository(tempDirectory)
    merges(origin, origin.commit("measured", "common/A.scala" -> "a")) shouldBe true
  }
}
