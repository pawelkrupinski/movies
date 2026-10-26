package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * How many Playwright workers a page-test row starts, against the 4 CPUs a job is budgeted (a hosted runner's 4
 * vCPUs; on the ci-runner the JVMs' ActiveProcessorCount=4). The container itself sees the whole 32-core host, so
 * `PW_WORKERS` in `.github/actions/run-page-test` is the only thing sizing it.
 *
 * WebKit gets half the budget. Its workers are each a WebProcess, a NetworkProcess and the test runner, beside the
 * fixture server's JVM: at 5 workers, once the ci-runner shared CPU by tier (2026-10-25), a WebKit test's p50 went
 * 4.4s -> 17-21s, its max past the 30s timeout, and `page tests (safari / pw behaviour)` failed 9 of 18 Main runs
 * (#1838, #1843), each time on different tests.
 */
class PlaywrightWorkerBudgetSpec extends AnyFlatSpec with Matchers {
  private val BudgetCpus = 4

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

  "run-page-test" should "give a WebKit row half the job's CPUs, on every host" in {
    workersExpr should startWith("PW_WORKERS: ${{ inputs.browsers == 'webkit' &&")
    workersFor("inputs.browsers == 'webkit'") shouldBe BudgetCpus / 2
  }

  it should "give any other row no more than Playwright's own CI default, 125% of the budget" in {
    workersFor("github.server_url != 'https://github.com'") should be <= BudgetCpus * 5 / 4
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
