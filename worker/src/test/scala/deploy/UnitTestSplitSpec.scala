package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml spreads the unit and integration specs over several rows, because one runner running a whole layer's
 * suites on its four cores was the build's long pole. A row per selection makes a spec easy to drop in the halfway
 * state — a module added to `testUnitNoE2e` (what a local run and the order-independence suite use) but to no
 * row, a worker row named in ci.yml that build.sbt does not filter for, a module without the filter running its
 * suites on every row — and its specs would stop gating deploys, or run twice, unnoticed. Every split selects
 * through build.sbt's `rowSettings`, whose `rest` row is everything the other rows do not select.
 */
class UnitTestSplitSpec extends AnyFlatSpec with Matchers {
  private lazy val ciYml    = RepoFile.read(".github/workflows/ci.yml")
  private lazy val testJob  = RepoFile.jobs(ciYml)("test")
  private lazy val itJob    = RepoFile.jobs(ciYml)("integration-test")
  private lazy val buildSbt = RepoFile.read("build.sbt")

  private def tasks(alias: String): Set[String] =
    RepoFile.commandAlias(alias).stripPrefix("all ").split("\\s+").filter(_.nonEmpty).toSet

  /** The row names a `rowSettings(variable, …)` call in build.sbt selects explicitly, plus its catch-all `rest`. */
  private def rowsOf(variable: String): Set[String] = {
    val at = buildSbt.indexOf(s"""rowSettings("$variable"""")
    withClue(s"build.sbt has no rowSettings(\"$variable\", …): ")(at should be >= 0)
    val call = buildSbt.substring(at).split("\n\n").head   // the call runs to the next blank line
    """"(\w+)"\s*->""".r.findAllMatchIn(call).map(_.group(1)).toSet + "rest"
  }

  /** The rows' commands: each worker row runs the worker's unit tests (filtered), one of them beside other modules. */
  private val WorkerCommands = Set("testUnitWorker", "testUnitWorkerAndWeb")

  "ci.yml's unit rows" should "run every module testUnitNoE2e runs, each on one row but the filtered worker" in {
    val rows = RepoFile.matrixRows(testJob)
    rows.map(_("cmd")).toSet shouldBe WorkerCommands + "testUnitRest"
    WorkerCommands.foreach(alias => withClue(alias)(tasks(alias) should contain("worker/Test/test")))
    val others = WorkerCommands.toSeq.map(tasks(_) - "worker/Test/test") :+ tasks("testUnitRest")
    others.flatten should contain theSameElementsAs others.flatten.distinct  // no module on two rows
    others.flatten.toSet + "worker/Test/test" shouldBe tasks("testUnitNoE2e")
    RepoFile.withoutComments(testJob) should include("run: sbt ${{ matrix.cmd }}")
  }

  they should "give every worker row a filter build.sbt knows, each exactly once, the catch-all among them" in {
    val workerRows = RepoFile.matrixRows(testJob).filter(row => WorkerCommands(row("cmd"))).map(_("suites"))
    workerRows should contain theSameElementsAs rowsOf("WORKER_ROW").toSeq
    testJob should include("WORKER_ROW: ${{ matrix.suites }}")
    // Only the worker carries the filter: the rest row runs no worker suite and sets none.
    RepoFile.matrixRows(testJob).filter(_("cmd") == "testUnitRest").flatMap(_.get("suites")) shouldBe empty
    """\.settings\([^)]*workerRowSettings""".r.findAllIn(buildSbt).size shouldBe 1
    // …and on the worker project: on another, the worker rows would each run every worker suite.
    val worker = buildSbt.substring(buildSbt.indexOf("lazy val worker = "))
    worker.take(worker.indexOf("\nlazy val ", 1)) should include("workerRowSettings)")
  }

  /** The deploy specs that read the gitops manifests are worker specs in the catch-all row; checking them out on
   *  another row would leave them reading nothing. */
  they should "check the gitops manifests out on the row that runs the worker's deploy specs" in {
    val lines = testJob.linesIterator.toVector
    val at    = lines.indexWhere(_.contains("/pawelkrupinski/movies-gitops"))
    lines.slice(at - 3, at).mkString("\n") should include("if: matrix.shard == 'worker'")
    RepoFile.matrixRows(testJob).find(_("shard") == "worker").map(_("suites")) shouldBe Some("rest")
  }

  /** The heavy rows select their suites by name: a renamed or moved suite would leave its row running nothing while the
   *  suite itself ran nowhere -- every unit row excludes it. */
  they should "name heavy suites that exist, each on one heavy row" in {
    val block  = buildSbt.substring(buildSbt.indexOf("lazy val HeavyRows"), buildSbt.indexOf("lazy val HeavySuites"))
    val names  = """"([a-z]+(?:\.[a-z]+)*\.[A-Z]\w+Spec)"""".r.findAllMatchIn(block).map(_.group(1)).toSeq
    names should not be empty
    names shouldBe names.distinct
    names.foreach { name =>
      withClue(name)(RepoFile.exists(s"worker/src/test/scala/${name.replace('.', '/')}.scala") shouldBe true)
    }
    """"(heavy\d)"\s*->\s*Set\(""".r.findAllMatchIn(block).map(_.group(1)).toSet shouldBe
      RepoFile.matrixRows(testJob).flatMap(_.get("suites")).filter(_.startsWith("heavy")).toSet
  }

  "ci.yml's integration rows" should "be build.sbt's IT_ROW filters, each running all of itAll" in {
    RepoFile.matrixRows(itJob).map(_("row")).distinct should contain theSameElementsAs rowsOf("IT_ROW").toSeq
    itJob should include("IT_ROW: ${{ matrix.row }}")
    RepoFile.withoutComments(itJob) should include("run: sbt itAll")
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

  /** The hard-cluster convergence runs on several `identity` rows, each narrowed to some countries through
   *  `KINOWO_HARD_CLUSTERS_COUNTRIES`: a country on no row would stop being checked before a deploy, one on two would
   *  boot twice, and a misspelt code is dropped by the knob's parser without a word. */
  they should "split the hard-cluster convergence by country, every country on exactly one identity row" in {
    itJob should include("KINOWO_HARD_CLUSTERS_COUNTRIES: ${{ matrix.countries }}")
    val identityRows = """-\s*\{([^}]*)}""".r.findAllMatchIn(RepoFile.block(itJob, "matrix")).map(_.group(1))
      .filter(row => """\brow:\s*identity\b""".r.findFirstIn(row).isDefined).toSeq
    identityRows should not be empty
    val codes = identityRows.flatMap { row =>
      val listed = """countries:\s*'([^']*)'""".r.findFirstMatchIn(row).map(_.group(1)).getOrElse("")
      withClue(s"an identity row naming no country runs every one: $row\n")(listed should not be empty)
      listed.split(",").map(_.trim).toSeq
    }
    codes.filter(models.Country.byCode(_).isEmpty) shouldBe empty
    codes should contain theSameElementsAs models.Country.all.map(_.code)
  }
}
