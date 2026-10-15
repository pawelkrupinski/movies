package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml's unit tests run as two rows — `worker` alone and every other module together — because one
 * runner running all five modules' suites at once on its four cores was the build's long pole. A row per
 * alias makes a module easy to drop in the halfway state: added to `testUnitNoE2e` (what a local run and
 * the order-independence suite use) but to neither row, its specs would stop gating deploys unnoticed.
 */
class UnitTestSplitSpec extends AnyFlatSpec with Matchers {
  private def tasks(alias: String): Set[String] =
    RepoFile.commandAlias(alias).stripPrefix("all ").split("\\s+").filter(_.nonEmpty).toSet

  private lazy val testJob = RepoFile.jobs(RepoFile.read(".github/workflows/ci.yml"))("test")

  "ci.yml's unit rows" should "together run exactly the modules testUnitNoE2e runs, each once" in {
    val rows = RepoFile.matrixRows(testJob).map(_("cmd"))
    rows should contain theSameElementsAs Seq("testUnitWorker", "testUnitRest")
    val split = rows.map(tasks)
    split.reduce(_ intersect _) shouldBe empty
    split.reduce(_ ++ _) shouldBe tasks("testUnitNoE2e")
    RepoFile.withoutComments(testJob) should include("run: sbt ${{ matrix.cmd }}")
  }

  /** The deploy specs that read the gitops manifests are worker specs; checking them out on the other
   *  row would leave the worker row's specs reading nothing. */
  they should "check the gitops manifests out on the row that runs the worker's deploy specs" in {
    val checkout = testJob.linesIterator.toVector
    val at = checkout.indexWhere(_.contains("repository: pawelkrupinski/movies-gitops"))
    checkout.slice(at - 3, at).mkString("\n") should include("if: matrix.shard == 'worker'")
  }
}
