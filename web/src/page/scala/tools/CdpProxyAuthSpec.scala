package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Regression for `Chrome`'s authenticated-proxy support, added so
 * `OgCardGenerator` can route through Decodo's residential proxy — needed
 * because GitHub Actions' own datacenter IP ranges are Cloudflare
 * Bot-Fight-Mode material on both `kinowo.net` and `showtimes.cc`
 * (2026-09-15 outage, no custom-rule skip exists for `fight_mode`).
 *
 * Two real bugs hit building this, both reproduced (and fixed) against this
 * exact spec before it existed as a spec:
 *   - Answering `Fetch.authRequired` by calling `page.send` synchronously
 *     from inside the WebSocket's own `onText` callback deadlocked: that
 *     callback is what would have to deliver the nested call's own reply,
 *     but it can't re-enter itself. Fixed by running event handlers on a
 *     fresh thread (`CdpPage.onEvent`).
 *   - `Fetch.enable` with `handleAuthRequests: true` but no `patterns` looked
 *     like "auth-only interception" but instead left Fetch inert: no
 *     `Fetch.authRequired` ever arrived and `Page.navigate` itself hung for
 *     30s. Puppeteer's own `page.authenticate()` sets
 *     `patterns: [{urlPattern: "*"}]`, which also means every request now
 *     pauses at `Fetch.requestPaused` and needs an explicit
 *     `Fetch.continueRequest` — both wired in `Chrome.openPage`.
 *
 * Drives real Chrome through a local Basic-auth proxy (`TestProxyServer`,
 * no external dependency) fetching from `TestHttpServer` — no real network
 * egress, no Decodo credentials needed.
 */
class CdpProxyAuthSpec extends AnyFlatSpec with Matchers {

  "Chrome started with a proxy" should "authenticate a plain-HTTP request through it" in {
    val server = new TestHttpServer(routes = { case "/" => "<html><body>proxied ok</body></html>" })
    val proxy  = new TestProxyServer(user = "proxyuser", pass = "proxypass")
    try
      Chrome.tryStart(proxy = Some(Chrome.ProxyConfig("127.0.0.1", proxy.port, "proxyuser", "proxypass"))) match {
        case None => cancel("Chrome not installed — skipping CDP proxy-auth spec")
        case Some(chrome) =>
          try chrome.openPage(server.baseUrl + "/") { page =>
            page.evalString("document.body.textContent") shouldBe "proxied ok"
          } finally chrome.close()
      }
    finally {
      proxy.close()
      server.close()
    }
  }

  // The real target (kinowo.net / showtimes.cc) is always HTTPS, which routes
  // through the proxy via CONNECT — a DIFFERENT Chromium code path from the
  // plain-HTTP case above. `TestProxyServer` answers CONNECT's auth challenge
  // and then closes (no real TLS origin needed) — Chrome's own subsequent TLS
  // handshake against the closed socket fails fast, so `document.readyState`
  // reaches `complete` on Chrome's own network-error page well under 15s.
  //
  // NOTE this does NOT reproduce the original CI hang by itself: both bugs
  // fixed on this class (the `onEvent` reentrancy deadlock, and `Fetch.enable`
  // needing `patterns` to fire `authRequired` at all) were verified by hand
  // against the real Decodo proxy + real showtimes.cc/kinowo.net, where an
  // unresolved auth challenge blocks inside Chrome's own process rather than
  // failing fast — a loopback proxy's near-zero latency doesn't hit whatever
  // timing that internal path depends on, confirmed by re-running this exact
  // test against the pre-fix code (both bugs reintroduced): it stayed just as
  // fast. What this test DOES cover for real: the auth mechanism (challenge →
  // `Fetch.continueWithAuth` → tunnel established) works for a CONNECT-based
  // HTTPS navigation, not only the plain-HTTP case above — and stays fast
  // doing it, which is what a regression here would most likely still break.
  it should "authenticate an HTTPS (CONNECT-tunneled) request through it, not just plain HTTP" in {
    val proxy = new TestProxyServer(user = "proxyuser", pass = "proxypass")
    try
      Chrome.tryStart(proxy = Some(Chrome.ProxyConfig("127.0.0.1", proxy.port, "proxyuser", "proxypass"))) match {
        case None => cancel("Chrome not installed — skipping CDP proxy-auth spec")
        case Some(chrome) =>
          try {
            val t0 = System.currentTimeMillis()
            noException should be thrownBy chrome.openPage("https://127.0.0.1:1/") { _ => () }
            (System.currentTimeMillis() - t0) should be < 15000L
          } finally chrome.close()
      }
    finally proxy.close()
  }
}
