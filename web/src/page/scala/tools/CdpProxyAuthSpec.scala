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
            val t0 = System.nanoTime() / 1000000
            noException should be thrownBy chrome.openPage("https://127.0.0.1:1/") { _ => () }
            (System.nanoTime() / 1000000 - t0) should be < 15000L
          } finally chrome.close()
      }
    finally proxy.close()
  }

  // A THIRD real bug, found chasing what first looked like Decodo residential-
  // proxy instability in OgCardGenerator (2026-09-15): a page with several
  // subresources pauses several `Fetch.requestPaused` events near-simultaneously,
  // each handled on `CdpPage.eventPool` and each calling `send` right back in
  // to continue it — CONCURRENTLY with whatever thread is mid-navigation. Two
  // threads calling `send` at once raced on the one underlying
  // `java.net.http.WebSocket`, which allows only one outstanding `sendText` at
  // a time: the loser threw `IllegalStateException: Send pending`, and if that
  // was a `Fetch.continueRequest` the paused resource — and the whole page —
  // never finished loading. This is what running MANY cities in one
  // `OgCardGenerator` process actually reproduced (10 cities through one
  // session, several outright page-load failures) where 1-3 never did; it
  // looked exactly like "the residential line degrades under sustained use"
  // until this exact exception turned up in a retry log. Fixed by serializing
  // the write side of `send` (`CdpPage.sendLock`) — every call still awaits its
  // OWN reply independently, so concurrent CDP round-trips stay concurrent.
  it should "not race concurrent Fetch.requestPaused continuations under proxy (many subresources at once)" in {
    val resourceCount = 30
    val routes: PartialFunction[String, String] = {
      case "/" => "<html><body>" + (1 to resourceCount).map(i => s"""<img src="/r$i">""").mkString + "</body></html>"
      case p if p.matches("/r\\d+") => "<svg xmlns='http://www.w3.org/2000/svg'></svg>"
    }
    val server = new TestHttpServer(routes = routes)
    val proxy  = new TestProxyServer(user = "proxyuser", pass = "proxypass")
    try
      Chrome.tryStart(proxy = Some(Chrome.ProxyConfig("127.0.0.1", proxy.port, "proxyuser", "proxypass"))) match {
        case None => cancel("Chrome not installed — skipping CDP proxy-auth spec")
        case Some(chrome) =>
          try noException should be thrownBy chrome.openPage(server.baseUrl + "/") { page =>
            page.waitFor(s"document.images.length >= $resourceCount", timeoutMs = 5000)
          } finally chrome.close()
      }
    finally {
      proxy.close()
      server.close()
    }
  }
}
