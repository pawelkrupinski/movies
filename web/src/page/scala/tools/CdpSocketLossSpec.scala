package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * When Chrome drops a page's DevTools connection mid-test — the tab destroyed, its
 * renderer gone, the browser killed under memory pressure — the next call says THAT,
 * promptly, instead of surfacing the JDK socket's bare "Output closed" from deep inside
 * whatever assertion happened to be polling (as HiddenFilmsSyncModelSpec's `quiesce`
 * did, which read as a sync-model failure), or waiting out the 30s reply timeout.
 */
class CdpSocketLossSpec extends AnyFlatSpec with Matchers {

  "a page whose DevTools connection Chrome closed" should "fail its next call saying Chrome closed it" in {
    Chrome.tryStart() match {
      case None => cancel("Chrome not installed")
      case Some(chrome) =>
        try chrome.openPage("about:blank") { page =>
          page.evalInt("1 + 1") shouldBe 2
          page.closeTargetFromChrome()
          val started = System.nanoTime()
          val failure = intercept[RuntimeException](
            (1 to 50).foreach { _ => page.evalInt("1"); Thread.sleep(20) })
          failure.getMessage should include ("Chrome closed this page's DevTools connection")
          ((System.nanoTime() - started) / 1000000) should be < 10000L
        }
        finally chrome.close()
    }
  }
}
