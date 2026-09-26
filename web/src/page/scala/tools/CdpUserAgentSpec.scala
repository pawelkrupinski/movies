package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Regression for the 2026-09-18 OG-card regen outage: a Cloudflare rule
 * added 2026-09-16 (site-wide on `kinowo.net`/`showtimes.cc`, see
 * `project_headless_chrome_full_crawl_2026_09_15`) challenges any request
 * whose `User-Agent` self-identifies as headless — added to stop an
 * unrelated crawler, but `OgCardGenerator`'s own `--headless` Chrome sends
 * exactly that (`…HeadlessChrome/…`), so it started tripping its own site's
 * anti-bot rule on every card, every country, from the very first request.
 *
 * `Chrome.tryStart`'s `spoofHeadlessUserAgent` flag fixes this by overriding
 * the `User-Agent` (via `Emulation.setUserAgentOverride`, never the `Network`
 * domain) to Chrome's own real UA with "Headless" stripped out. This spec
 * proves the header that actually reaches the server on the wire no longer
 * contains "Headless".
 */
class CdpUserAgentSpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  private def withServerAndChrome(spoof: Boolean)(body: (Chrome, TestHttpServer) => Unit): Unit = {
    val server = new TestHttpServer(routes = PartialFunction.empty)
    try
      Chrome.tryStart(configuration.cdpBrowserBinary, spoofHeadlessUserAgent = spoof) match {
        case None => cancel("Chrome not installed — skipping CDP user-agent spec")
        case Some(chrome) =>
          try body(chrome, server) finally chrome.close()
      }
    finally server.close()
  }

  "Chrome.tryStart's default (spoofHeadlessUserAgent = false)" should
    "send a User-Agent that still identifies as headless" in
    withServerAndChrome(spoof = false) { (chrome, server) =>
      chrome.openPage(s"${server.baseUrl}/__echo-user-agent") { page =>
        page.evalString("document.body.textContent") should include("Headless")
      }
    }

  "Chrome.tryStart's spoofHeadlessUserAgent = true" should
    "strip \"Headless\" from the User-Agent actually sent on the wire" in
    withServerAndChrome(spoof = true) { (chrome, server) =>
      chrome.openPage(s"${server.baseUrl}/__echo-user-agent") { page =>
        val userAgent = page.evalString("document.body.textContent")
        userAgent should not include "Headless"
        userAgent should include("Chrome/")
      }
    }
}
