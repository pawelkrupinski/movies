package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml's `needs: scope` rows are written longest first, because that is the order they start in.
 *
 * A free runner gets the waiting job with the oldest `updated`, then the lowest id, and a workflow's jobs get their ids in file order (only a matrix's rows are re-sorted, by their values). The needs-less rows wait from t=0 and take Main's slots first;
 * every `needs: scope` row turns waiting in the same moment, when scope ends, so among them the file order decides.
 * With the page tests written last, the three safari rows queued behind mobile, both e2e shards and chrome, and a
 * safari row was the last ci row to end in 174 of 204 Main runs to 2026-10-26, starting 13.5-18 min in.
 */
class CiDispatchOrderSpec extends AnyFlatSpec with Matchers {

  private lazy val ciYml = RepoFile.read(".github/workflows/ci.yml")

  /** ci.yml's job ids in file order. */
  private lazy val jobOrder: Seq[String] = {
    val jobsBlock = RepoFile.block(ciYml, "jobs")
    val Header    = """^    ([A-Za-z][\w-]*):\s*$""".r
    jobsBlock.linesIterator.collect { case Header(name) => name }.toSeq
  }

  private lazy val waitsOnScope: Seq[String] =
    jobOrder.filter(job => RepoFile.withoutComments(RepoFile.jobs(ciYml)(job)).linesIterator.exists(_.trim == "needs: scope"))

  "ci.yml's needs-scope rows" should "put the page tests, the longest of them, ahead of the shorter rows" in {
    waitsOnScope should contain allOf ("page-tests-safari", "page-tests-chrome", "mobile-local-server", "e2e")
    val pages = Seq("page-tests-safari", "page-tests-chrome").map(waitsOnScope.indexOf)
    val rest  = waitsOnScope.filterNot(_.startsWith("page-tests-")).map(waitsOnScope.indexOf)
    withClue(s"needs-scope rows in file order: $waitsOnScope: ") {
      pages.max should be < rest.min
    }
  }

  "the needs-less rows" should "stay ahead of every row that waits on scope" in {
    val immediate = jobOrder.filterNot(j => j == "scope" || waitsOnScope.contains(j))
    immediate should contain allOf ("test", "integration-test")
    immediate.map(jobOrder.indexOf).max should be < waitsOnScope.map(jobOrder.indexOf).min
  }
}
