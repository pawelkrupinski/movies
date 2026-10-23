package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The two `playwright.config.ts` settings that keep the Playwright rows (WebKit is CI's long pole) from paying for
 * work no passing test needs. Measured 2026-10-23, webkit-iphone-13, 5 workers: the full trace cost ~40% of a
 * project's wall time; running one file's tests on one worker left the `webkit-behaviour` row ~30% slower.
 */
class PlaywrightRunCostSpec extends AnyFlatSpec with Matchers {
  private lazy val code =
    RepoFile.read("page-tests-playwright/playwright.config.ts").linesIterator.filterNot(_.trim.startsWith("//")).mkString("\n")

  "playwright.config.ts" should "not record DOM snapshots or a screencast into every test's trace" in {
    code should include("snapshots: false")
    code should include("screenshots: false")
  }

  it should "spread one file's tests across workers" in {
    code should include("fullyParallel: true")
  }
}
