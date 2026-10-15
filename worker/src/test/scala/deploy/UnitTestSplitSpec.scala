package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml spreads the unit and integration specs over several rows, because one runner running a whole layer's
 * suites on its four cores was the build's long pole. A row per selection makes a spec easy to drop in the halfway
 * state — a module added to `testUnitNoE2e` (what a local run and the order-independence suite use) but to no
 * row, a package matched by neither of the worker's two halves, a suite filtered out of both integration rows —
 * and its specs would stop gating deploys unnoticed. Every split here is a selection and its exclusion, so
 * anything new lands in exactly one row.
 */
class UnitTestSplitSpec extends AnyFlatSpec with Matchers {
  private lazy val ciYml    = RepoFile.read(".github/workflows/ci.yml")
  private lazy val testJob  = RepoFile.jobs(ciYml)("test")
  private lazy val itJob    = RepoFile.jobs(ciYml)("integration-test")
  private lazy val buildSbt = RepoFile.read("build.sbt")

  private def tasks(alias: String): Set[String] =
    RepoFile.commandAlias(alias).stripPrefix("all ").split("\\s+").filter(_.nonEmpty).toSet

  "ci.yml's unit rows" should "run testUnitNoE2e's other modules on one row and the worker on two" in {
    RepoFile.matrixRows(testJob).map(_("cmd")) should contain theSameElementsAs
      Seq("testUnitWorkerScripts", "testUnitWorker", "testUnitRest")
    tasks("testUnitRest") shouldBe tasks("testUnitNoE2e") - "worker/Test/test"
    RepoFile.withoutComments(testJob) should include("run: sbt ${{ matrix.cmd }}")
  }

  they should "split the worker by one package and its exclusion, so every worker spec runs exactly once" in {
    val Selected = """worker/Test/testOnly (\S+)""".r
    val selected = RepoFile.commandAlias("testUnitWorkerScripts") match {
      case Selected(pattern) => pattern
      case other             => fail(s"testUnitWorkerScripts is `$other`, not one worker/Test/testOnly pattern")
    }
    RepoFile.commandAlias("testUnitWorker") shouldBe s"worker/Test/testOnly * -$selected"
  }

  /** The deploy specs that read the gitops manifests are worker specs outside `scripts`; checking them out on
   *  another row would leave them reading nothing. */
  they should "check the gitops manifests out on the row that runs the worker's deploy specs" in {
    val lines = testJob.linesIterator.toVector
    val at    = lines.indexWhere(_.contains("repository: pawelkrupinski/movies-gitops"))
    lines.slice(at - 3, at).mkString("\n") should include("if: matrix.shard == 'worker'")
  }

  "ci.yml's integration rows" should "be build.sbt's two IT_ROW filters, each running all of itAll" in {
    RepoFile.matrixRows(itJob).map(_("row")) should contain theSameElementsAs Seq("identity", "rest")
    itJob should include("IT_ROW: ${{ matrix.row }}")
    RepoFile.withoutComments(itJob) should include("run: sbt itAll")
  }

  they should "select a set of suites on one row and exclude exactly that set on the other" in {
    buildSbt should include("""case "identity" => IntegrationTest / testOptions += Tests.Filter(IdentityIntegrationSuites)""")
    buildSbt should include("""case "rest"     => IntegrationTest / testOptions += Tests.Filter(!IdentityIntegrationSuites(_))""")
    // Every integration module carries the filter: a module without it would run its suites on both rows.
    """\.settings\(itReportSettings, itRowSettings\)""".r.findAllIn(buildSbt).size shouldBe
      """\.settings\(itReportSettings""".r.findAllIn(buildSbt).size
  }

  they should "name integration suites that exist, so the identity row cannot run nothing" in {
    val Named = """"(integration\.\w+)"""".r
    val block = buildSbt.substring(buildSbt.indexOf("lazy val IdentityIntegrationSuites"))
    val names = Named.findAllMatchIn(block.takeWhile(_ != ')')).map(_.group(1)).toSeq
    names should not be empty
    names.foreach { name =>
      withClue(name)(RepoFile.exists(s"worker/src/it/scala/${name.stripPrefix("integration.")}.scala") shouldBe true)
    }
  }
}
