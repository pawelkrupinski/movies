package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * How many Playwright workers a page-test row starts, against the 4 CPUs a job is budgeted (a hosted runner's 4
 * vCPUs). `PW_WORKERS` in `.github/actions/run-page-test` is the only thing sizing it.
 *
 * WebKit gets one worker. Its workers are each a WebProcess, a NetworkProcess and the test runner, beside the fixture
 * server's JVM. More rows, not more workers, are what buy WebKit CPU (ci.yml's shards).
 */
class PlaywrightWorkerBudgetSpec extends AnyFlatSpec with Matchers {

  private lazy val action = RepoFile.read(".github/actions/run-page-test/action.yml")
  private lazy val ciYml  = RepoFile.read(".github/workflows/ci.yml")

  private lazy val workersExpr: String = {
    val line = action.linesIterator.map(_.trim).filter(_.startsWith("PW_WORKERS:")).toVector
    line should have size 1
    line.head
  }

  private def workersFor(condition: String): Int = {
    val Pattern = (java.util.regex.Pattern.quote(condition) + """\s*&&\s*'(\d+)'""").r
    Pattern.findFirstMatchIn(workersExpr).map(_.group(1).toInt).getOrElse(fail(s"no `$condition && '<n>'` in $workersExpr"))
  }

  "run-page-test" should "give a WebKit row one worker, on every host" in {
    workersExpr should startWith("PW_WORKERS: ${{ inputs.browsers == 'webkit' &&")
    workersFor("inputs.browsers == 'webkit'") shouldBe 1
  }

  it should "leave any other row at Playwright's own CI default (no override)" in {
    workersExpr should endWith("|| '' }}")
  }

  /** The action matches `browsers` exactly, so a WebKit row that installs a second engine would fall back to 5. */
  "ci.yml" should "install WebKit alone on every row that runs a WebKit project" in {
    val safari = RepoFile.block(ciYml, "page-tests-safari")
    val Browsers = """browsers:\s*([^,}\n]+)""".r
    val engines = Browsers.findAllMatchIn(safari).map(_.group(1).trim).filterNot(_.startsWith("${{")).toVector
    engines should not be empty
    all(engines) shouldBe "webkit"
    RepoFile.block(ciYml, "page-tests-chrome") should not include "webkit-"
  }
}
