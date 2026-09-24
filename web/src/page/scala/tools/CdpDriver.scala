package tools

import play.api.libs.json.{JsValue, Json}

import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.WebSocket.Listener
import java.net.http.{HttpClient, HttpRequest, WebSocket}
import java.nio.file.{Files, Path, Paths}
import java.time.Duration
import java.util.Comparator
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, Executors, TimeUnit}

/**
 * Minimal headless-Chrome driver used by `PageJsBehaviourSpec` to run
 * regression tests against the JavaScript on the rendered Twirl pages.
 * Drives Chrome over the DevTools Protocol via WebSocket (java.net.http).
 *
 * Why not Playwright/Puppeteer: this repository is Scala-only with no npm.
 * Adding a JS test runner would bring node, a separate build step, and
 * cross-tool wiring just to assert on a handful of DOM behaviours.
 * CDP-over-WebSocket gives us the same surface — drive a real browser,
 * observe a real DOM — without leaving the JVM.
 *
 * Lifecycle: callers `Chrome.tryStart()` once per spec (typically in
 * `beforeAll`), then `openPage(htmlFile) { page => … }` per test for an
 * isolated tab. The tab is closed at the end of each block; the Chrome
 * process is closed when the spec finishes. `tryStart` returns `None`
 * when Chrome isn't installed locally — callers should `cancel` cleanly
 * so CI that lacks a browser doesn't fail the whole suite.
 */
object Chrome {

  /** Locations the headless test infra knows how to find a CDP-speaking
   *  browser executable. Covers macOS / Linux / common Docker images
   *  for Chrome, Chromium, and Microsoft Edge (Chromium-based, same
   *  CDP wire format). */
  private val CandidatePaths: Seq[String] = Seq(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "/usr/bin/google-chrome",
    "/usr/bin/google-chrome-stable",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
    "/usr/bin/microsoft-edge",
    "/usr/bin/microsoft-edge-stable",
    "/opt/microsoft/msedge/microsoft-edge"
  )

  /** Resolve a browser binary. The `CDP_BROWSER_BIN` env var wins so
   *  CI can point at a specific Chrome / Edge version installed at a
   *  non-standard path (the GH Actions matrix sets this per job).
   *  Otherwise scan `CandidatePaths` for the first executable hit. */
  def findExecutable(): Option[Path] = {
    val envOverride = Option(System.getenv("CDP_BROWSER_BIN"))
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Paths.get(_))
      .filter(Files.isExecutable)
    envOverride.orElse(
      CandidatePaths.iterator.map(Paths.get(_)).find(Files.isExecutable)
    )
  }

  /** An HTTP(S) forward proxy Chrome should route every request through, with
   *  Basic credentials supplied over CDP (`Fetch.authRequired` →
   *  `Fetch.continueWithAuth`) rather than embedded in `--proxy-server` —
   *  modern Chrome ignores inline `user:pass@` there. See [[Chrome.tryStart]]. */
  final case class ProxyConfig(host: String, port: Int, user: String, pass: String)

  /** Launch a headless Chrome on a free port. Returns `None` when no
   *  Chrome binary is reachable on this machine.
   *
   *  `proxy`, when given, adds `--proxy-server` so every request routes
   *  through it (auth handled per-page in `openPage`; see [[ProxyConfig]]).
   *  Used by [[OgCardGenerator]] to route through a residential IP —
   *  GitHub Actions' own datacenter IP ranges are Cloudflare Bot-Fight-Mode
   *  material (2026-09-15 CI outage, root-caused via `loadFailureDiagnostic`
   *  to `fight_mode: true` on both zones, which Cloudflare's own docs confirm
   *  has NO custom-rule skip/bypass at all).
   *
   *  `spoofHeadlessUserAgent`, when true, strips "Headless" from the
   *  User-Agent every page sends (via `Emulation.setUserAgentOverride` in
   *  `openPage`, computed from THIS Chrome's own real UA so it can never go
   *  stale as the runner's "stable" Chrome auto-updates). Used by
   *  [[OgCardGenerator]] because a SEPARATE Cloudflare rule added 2026-09-16
   *  (see `project_headless_chrome_full_crawl_2026_09_15` — a site-wide
   *  challenge on both zones for any self-identifying headless UA, added to
   *  stop an unrelated crawler) started challenging this generator's own
   *  Chrome too: `--headless` Chrome's default UA contains `HeadlessChrome/…`,
   *  which is exactly the substring that rule matches on. Confirmed
   *  2026-09-18: every leg of the weekly regen failed instantly (first
   *  request, no warm-up) on the Cloudflare challenge page, while a bare
   *  `curl` with a normal UA through the SAME proxy got a clean 200 — so the
   *  residential proxy (which already defeats Bot-Fight-Mode's IP scoring)
   *  was never the problem here, the UA string was. The Free plan has no
   *  TLS/JA3 fingerprinting or ML bot score behind this rule, so a plain
   *  string substitution is enough — no launch-flag/stealth arms race
   *  needed. */
  def tryStart(proxy: Option[ProxyConfig] = None, spoofHeadlessUserAgent: Boolean = false): Option[Chrome] = findExecutable().flatMap { exe =>
    val port    = findFreePort()
    val userDirectory = Files.createTempDirectory("chrome-cdp-test-")
    val pb = new ProcessBuilder(
      (Seq(
        exe.toString,
        "--headless",
        "--disable-gpu",
        "--no-sandbox",
        "--hide-scrollbars",
        "--mute-audio",
        "--disable-background-networking",
        "--disable-default-apps",
        "--disable-extensions",
        "--disable-sync",
        // Anchored origin allow-list — without this, Chrome 111+ rejects
        // WebSocket handshakes from clients that don't send an Origin
        // header (our java.net.http.WebSocket doesn't) with a 403.
        "--remote-allow-origins=*",
        s"--remote-debugging-port=$port",
        s"--user-data-dir=${userDirectory.toString}"
      ) ++ proxy.map(p => s"--proxy-server=http://${p.host}:${p.port}")
        ++ Seq("about:blank"))*
    ).redirectErrorStream(true)
    val process = pb.start()
    // Drain stdout/stderr so the buffer never fills up and blocks Chrome.
    new Thread(() => {
      val in = process.getInputStream
      val buf = new Array[Byte](4096)
      try while (in.read(buf) >= 0) () catch { case _: Throwable => () }
    }, "chrome-stdout-drain").start()
    val deadline = System.currentTimeMillis() + 10_000
    var ready = false
    while (!ready && System.currentTimeMillis() < deadline) {
      try {
        httpGet(s"http://localhost:$port/json/version")
        ready = true
      } catch {
        case _: Throwable => Thread.sleep(100)
      }
    }
    if (ready) {
      // Print the Chrome version + binary path to the test log so any
      // "passes locally / fails on CI" failure has a one-liner diff on
      // the version that ran. Chrome's `/json/version` endpoint returns
      // it as JSON. See [[feedback-ci-chrome-version-drift]].
      var userAgentOverride: Option[String] = None
      try {
        val info = httpGet(s"http://localhost:$port/json/version")
        val version = """"Browser":\s*"([^"]+)"""".r.findFirstMatchIn(info).map(_.group(1)).getOrElse("unknown")
        System.err.println(s"[CdpDriver] Chrome=$version path=$exe")
        if (spoofHeadlessUserAgent)
          userAgentOverride = """"User-Agent":\s*"([^"]+)"""".r.findFirstMatchIn(info)
            .map(_.group(1).replace("HeadlessChrome", "Chrome"))
      } catch { case _: Throwable => () }
      Some(new Chrome(process, port, userDirectory, proxy, userAgentOverride))
    } else {
      process.destroyForcibly()
      None
    }
  }

  private def findFreePort(): Int = {
    val serverSocket = new java.net.ServerSocket(0)
    try serverSocket.getLocalPort finally serverSocket.close()
  }

  private[tools] def httpGet(url: String): String = {
    val client = HttpClient.newHttpClient()
    val request = HttpRequest.newBuilder(URI.create(url))
      .timeout(Duration.ofSeconds(5))
      .build()
    client.send(request, BodyHandlers.ofString()).body()
  }

  private[tools] def httpPut(url: String): String = {
    val client = HttpClient.newHttpClient()
    val request = HttpRequest.newBuilder(URI.create(url))
      .timeout(Duration.ofSeconds(5))
      .PUT(HttpRequest.BodyPublishers.noBody())
      .build()
    client.send(request, BodyHandlers.ofString()).body()
  }
}

/** A running Chrome process; opens isolated tabs per test. */
class Chrome private[tools] (
                              process: Process,
                              port: Int,
                              userDataDirectory: Path,
                              proxy: Option[Chrome.ProxyConfig] = None,
                              userAgentOverride: Option[String] = None
                            ) extends AutoCloseable {

  /** Open `url` in a fresh tab, run `body`, then close the tab. The page
   *  is loaded synchronously — `body` runs after `document.readyState`
   *  is `complete`, so DOMContentLoaded handlers (buildIndex, the
   *  boot-time applyFilters() in _sharedJs) have fired.
   *
   *  When `tryStart` was given a `proxy`, this also enables `Fetch` with
   *  `handleAuthRequests`, which surfaces the proxy's 407 as a
   *  `Fetch.authRequired` event instead of Chrome showing a (headless-inert)
   *  native credentials prompt that would otherwise hang the navigation
   *  forever — `Fetch.continueWithAuth` answers it with the configured
   *  Basic credentials. Only the `source: "Proxy"` challenge is answered;
   *  a same-origin auth challenge (our own site has none) gets the default
   *  (unauthenticated) response rather than leaking the proxy password to
   *  an arbitrary origin.
   *
   *  Callers usually point at `TestHttpServer.baseUrl + "/some/path"`.
   *  Avoid file:// — `history.replaceState` (used by the date-filter ↔
   *  URL sync to rewrite `?date=`) throws SecurityError on file:// origins,
   *  which silently aborts the rest of the handler in production code. */
  def openPage[T](url: String)(body: CdpPage => T): T = {
    // Open a blank tab first, then navigate via CDP. Chrome ≥ 130 silently
    // ignores the URL passed to `/json/new?<URL>` on some platforms (CI
    // runners with the latest stable) — the tab lands on `about:blank`,
    // which has an opaque origin (localStorage denied, scripts not
    // executed against the test server) and produces null `#film-grid`
    // queries that look like phantom page failures. Driving the navigation
    // through `Page.navigate` over CDP is the supported path that doesn't
    // depend on the legacy query-string URL.
    val newTab = Json.parse(Chrome.httpPut(s"http://localhost:$port/json/new"))
    val wsUrl  = (newTab \ "webSocketDebuggerUrl").as[String]
    val tabId  = (newTab \ "id").as[String]
    val page = new CdpPage(URI.create(wsUrl))
    try {
      page.send("Page.enable")
      page.send("Runtime.enable")
      // `Emulation.setUserAgentOverride` is stateless (no `Emulation.enable`
      // needed, same as `setDeviceMetricsOverride` below) and never touches
      // the `Network` domain — a domain this driver avoids entirely (a
      // `Network.setExtraHTTPHeaders` call once broke CI outright while
      // working fine locally; see git history around 2026-09-15).
      userAgentOverride.foreach { ua =>
        page.send("Emulation.setUserAgentOverride", Json.obj("userAgent" -> ua))
      }
      proxy.foreach { p =>
        // `patterns` is REQUIRED for `Fetch.authRequired` to fire at all — an
        // empty/omitted `patterns` looked like "auth-only interception" but
        // instead left Fetch interception fully inert: no event ever arrived
        // and `Page.navigate` itself hung (confirmed locally, no `patterns` ⇒
        // 30s timeout on the navigate reply; matches how Puppeteer's own
        // `page.authenticate()` is wired — `patterns: [{urlPattern: "*"}]`).
        // Matching everything means every request now pauses at
        // `Fetch.requestPaused` too, so that has to be answered as well —
        // with a bare `continueRequest` (no modification), it is a pass-through.
        page.onEvent("Fetch.requestPaused") { params =>
          page.send("Fetch.continueRequest", Json.obj("requestId" -> (params \ "requestId").as[String]))
        }
        page.onEvent("Fetch.authRequired") { params =>
          val requestId = (params \ "requestId").as[String]
          val isProxy   = (params \ "authChallenge" \ "source").asOpt[String].contains("Proxy")
          val response  =
            if (isProxy) Json.obj("response" -> "ProvideCredentials", "username" -> p.user, "password" -> p.pass)
            else Json.obj("response" -> "Default")
          page.send("Fetch.continueWithAuth", Json.obj("requestId" -> requestId, "authChallengeResponse" -> response))
        }
        page.send("Fetch.enable", Json.obj("handleAuthRequests" -> true, "patterns" -> Json.arr(Json.obj("urlPattern" -> "*"))))
      }
      page.send("Page.navigate", Json.obj("url" -> url))
      // Wait for DOMContentLoaded so any inline `addEventListener
      // ('DOMContentLoaded', …)` registrations have fired. A short poll
      // is simpler and more reliable than wiring up CDP events.
      page.waitFor("document.readyState === 'complete'", timeoutMs = 5000)
      body(page)
    } finally {
      try page.close() catch { case _: Throwable => () }
      try Chrome.httpGet(s"http://localhost:$port/json/close/$tabId") catch { case _: Throwable => () }
    }
  }

  override def close(): Unit = {
    try process.destroy() catch { case _: Throwable => () }
    if (!process.waitFor(3, TimeUnit.SECONDS)) process.destroyForcibly()
    // Best-effort cleanup of the temp user-data directory. Don't fail the
    // spec if a lock file lingers — Chrome occasionally holds one open
    // for a tick after the process exits.
    try {
      Files.walk(userDataDirectory).sorted(Comparator.reverseOrder()).forEach(p =>
        try Files.deleteIfExists(p) catch { case _: Throwable => () }
      )
    } catch { case _: Throwable => () }
  }
}

/** One CDP page session. Synchronous request/response over the WebSocket,
 *  plus best-effort dispatch of unsolicited events (messages with a
 *  `method` but no `id`) to a handler registered via [[onEvent]] — needed
 *  for `Fetch.authRequired`, which Chrome sends on its own initiative
 *  rather than in reply to any call this driver made. */
class CdpPage private[tools] (uri: URI) extends AutoCloseable {
  private val idGen = new AtomicInteger(0)
  private val pending = new ConcurrentHashMap[Int, CompletableFuture[JsValue]]()
  private val eventHandlers = new ConcurrentHashMap[String, JsValue => Unit]()
  private val buffer  = new StringBuilder()

  // Dispatches CDP events off the WebSocket's own delivery thread (see onText
  // below for why that's required) WITHOUT spawning a fresh OS thread per
  // event. A proxied page's `Fetch.enable({patterns: [...]})` (see
  // Chrome.openPage) pauses EVERY subresource request — posters, CSS, JS, the
  // lot — so a real repertoire page fires dozens of these per load. The
  // blank-poster bug this pool was first credited with fixing (2026-09-15,
  // ~1/3 of a regen PR's cards shipping blank) was NOT thread-creation
  // overhead or GH Actions' 2-core runners having less headroom to service
  // the spawned threads — swapping per-event `new Thread(...)` for this pool
  // shipped and, per fdb7299de's commit message, made no measurable
  // difference to the failure rate. The real cause was `send`'s WebSocket
  // race (see sendLock below): whichever thread ran a Fetch.requestPaused
  // handler — spawned or pooled — called `send` back in concurrently with
  // whatever thread was mid-navigation, and the two raced on the one socket.
  // This pool is still a real, if minor, efficiency win over spawning a
  // Thread per event; it just isn't what fixed the blank posters.
  private val eventPool = Executors.newFixedThreadPool(4)

  /** Register `handler` to run whenever Chrome sends the CDP event `method`
   *  (e.g. `"Fetch.authRequired"`), passed that event's `params`. At most one
   *  handler per method — a second registration replaces the first, which is
   *  fine for this driver's one-handler-per-page usage. */
  def onEvent(method: String)(handler: JsValue => Unit): Unit = eventHandlers.put(method, handler)

  private val ws: WebSocket = HttpClient.newHttpClient()
    .newWebSocketBuilder()
    .connectTimeout(Duration.ofSeconds(5))
    .buildAsync(uri, new Listener {
      override def onOpen(ws: WebSocket): Unit = { ws.request(Long.MaxValue); super.onOpen(ws) }
      override def onText(ws: WebSocket, data: CharSequence, last: Boolean): java.util.concurrent.CompletionStage[?] = {
        buffer.append(data)
        if (last) {
          val text = buffer.toString
          buffer.setLength(0)
          val message = Json.parse(text)
          (message \ "id").asOpt[Int] match {
            case Some(id) => Option(pending.remove(id)).foreach(_.complete(message))
            case None     =>
              for {
                method  <- (message \ "method").asOpt[String]
                handler <- Option(eventHandlers.get(method))
              } {
                val params = (message \ "params").getOrElse(Json.obj())
                // MUST run off this thread: java.net.http's WebSocket listener
                // delivers messages one at a time and won't invoke onText again
                // until this call returns, so a handler that calls back into
                // `send` (as the proxy-auth handler does) would deadlock
                // waiting for a reply that this same blocked thread is the
                // only one able to deliver. Hit locally: `Fetch.continueWithAuth`
                // from inside this callback timed out at 30s every time.
                eventPool.execute(() => try handler(params) catch { case _: Throwable => () })
              }
          }
        }
        null
      }
    }).get(10, TimeUnit.SECONDS)

  // `java.net.http.WebSocket` allows only ONE outstanding send at a time per
  // socket — a second `sendText` before the first's returned CompletableFuture
  // completes throws `IllegalStateException: Send pending`. `send()` used to
  // call `ws.sendText` unsynchronized, which was fine while only ONE thread
  // (the caller) ever called it — but a proxied page's `Fetch.enable` now
  // pauses every subresource, and each `Fetch.requestPaused`/`authRequired`
  // handler runs on `eventPool` (a SEPARATE thread from whichever thread is
  // mid-navigation) and calls `send` right back in to continue it. Two
  // threads calling `send` at once — routine on any page with more than a
  // couple of concurrent resource loads — raced on the shared socket and hit
  // exactly that exception, observed live (2026-09-15) as `document.
  // readyState` itself timing out: a `Fetch.continueRequest` that lost the
  // race never went out, so the paused resource, and the whole page, never
  // finished loading. This was misread at first as residential-proxy
  // instability (network jitter has the same "some pages just hang"
  // symptom) — the giveaway was this exact exception in a retry log, a
  // signature no network condition produces. Only the WRITE needs
  // serializing; each call still waits on its OWN reply future independently,
  // so concurrent CDP round-trips (multiple in-flight sends, replies arriving
  // in any order) are unaffected.
  private val sendLock = new Object

  /** Issue a CDP method call and return the `result` field of the reply.
   *  Blocks the caller until the reply arrives or 30s elapses. */
  def send(method: String, parameters: JsValue = Json.obj()): JsValue = {
    val id  = idGen.incrementAndGet()
    val fut = new CompletableFuture[JsValue]()
    pending.put(id, fut)
    val message = Json.obj("id" -> id, "method" -> method, "params" -> parameters).toString
    sendLock.synchronized { ws.sendText(message, true).get(5, TimeUnit.SECONDS) }
    val reply = fut.get(30, TimeUnit.SECONDS)
    (reply \ "error").asOpt[JsValue].foreach { err =>
      throw new RuntimeException(s"CDP error from $method: ${Json.stringify(err)}")
    }
    (reply \ "result").asOpt[JsValue].getOrElse(Json.obj())
  }

  /** Evaluate `js` in the page and return the unwrapped value. Throws if
   *  the JS itself threw — that's the test's signal that the page is
   *  in an unexpected shape. */
  def eval(js: String): JsValue = {
    val parameters = Json.obj(
      "expression"    -> js,
      "returnByValue" -> true,
      "awaitPromise"  -> true
    )
    val result = send("Runtime.evaluate", parameters)
    (result \ "exceptionDetails").asOpt[JsValue].foreach { exception =>
      throw new RuntimeException(s"JS exception evaluating [$js]: ${Json.stringify(exception)}")
    }
    (result \ "result" \ "value").asOpt[JsValue].getOrElse(Json.obj())
  }

  def evalString(js: String): String = eval(js).as[String]
  def evalInt(js: String):    Int    = eval(js).as[Int]
  def evalBool(js: String):   Boolean = eval(js).as[Boolean]

  /** Poll until `js` evaluates truthy. Used at page-open time to wait
   *  for `document.readyState === 'complete'`; can also be used by tests
   *  that need to wait for a debounced filter pass to settle. */
  def waitFor(js: String, timeoutMs: Int = 2000, pollMs: Int = 50): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (System.currentTimeMillis() < deadline) {
      if (evalBool(s"!!($js)")) return
      Thread.sleep(pollMs)
    }
    throw new RuntimeException(s"Timed out after ${timeoutMs}ms waiting for: $js")
  }

  /** Reload and wait for the NEW document, not merely for A document.
   *
   *  Waiting on `document.readyState === 'complete'` alone is satisfied by the
   *  document being replaced — it is 'complete' too — so a reload that had not yet
   *  swapped returned immediately and handed the caller a page still running the
   *  OLD load. Whatever the caller then waited for was really waiting for the
   *  reload as well, and on a loaded runner the page load ate the budget: CI,
   *  2026-09-05, `PageJsBehaviourSpec` timed out after 2000ms on the
   *  server-state reconcile while the same test passed locally every time.
   *
   *  Stamping the outgoing document and waiting for the stamp to be GONE is
   *  positive evidence of the swap, so no sleep has to guess at it. */
  def reload(): Unit = replaceDocument(send("Page.reload"))

  /** Go to `url` in this tab and wait for the NEW document — same stamp as
   *  [[reload]], for the same reason. Keeps the origin's localStorage, which is
   *  what a spec walking one visitor across pages needs. */
  def navigate(url: String): Unit = replaceDocument(send("Page.navigate", Json.obj("url" -> url)))

  private def replaceDocument(go: => Any): Unit = {
    eval("window.__cdpReloadStamp = 1")
    go
    waitFor("typeof window.__cdpReloadStamp === 'undefined' && document.readyState === 'complete'",
            timeoutMs = 10000)
  }

  /** Capture the current viewport as PNG bytes returned Base64-encoded.
   *  Chrome encodes the screenshot that way over the CDP wire, so this
   *  passes the encoded string through verbatim — callers decode with
   *  `java.util.Base64.getDecoder.decode(...)` and write to disk with
   *  `Files.write(...)`. Used by `tools.MobileScreenshots` to produce
   *  per-viewport renders of the navbar / page chrome. */
  def screenshot(): String = {
    val res = send("Page.captureScreenshot", Json.obj("format" -> "png"))
    (res \ "data").as[String]
  }

  /** Override the viewport (CSS pixel) size and force a relayout. Used
   *  by `MobileLayoutSpec` to drive the same rendered page through a
   *  range of phone widths (320 → 575 px). `deviceScaleFactor = 0`
   *  means "use the host's default DPR" which keeps text-metrics
   *  realistic; `mobile = true` so the page sees the same
   *  `pointer: coarse` media-query truthiness as a real phone.
   *
   *  CDP's `setDeviceMetricsOverride` triggers a synchronous resize +
   *  layout in the renderer; by the time the call returns,
   *  `getBoundingClientRect()` reports the new geometry. No extra
   *  wait is needed. */
  def setViewport(width: Int, height: Int): Unit = {
    send("Emulation.setDeviceMetricsOverride", Json.obj(
      "width"  -> width,
      "height" -> height,
      "deviceScaleFactor" -> 0,
      "mobile" -> true
    ))
  }

  /** Desktop counterpart to `setViewport`: same CDP call, but with
   *  `mobile = false` so the page is laid out under desktop CSS branches
   *  (`@media (hover: hover)`, `pointer: fine`, no `--mobile-scale`
   *  shrinking). Used by the desktop layout sweep to drive the page
   *  through 1280 / 1440 / 1920 px widths and assert the navbar fits
   *  one row with zero horizontal overflow at every common desktop
   *  size. Reset with `Emulation.clearDeviceMetricsOverride` when the
   *  caller is done — same pattern as the mobile sweep. */
  def setDesktopViewport(width: Int, height: Int): Unit = {
    send("Emulation.setDeviceMetricsOverride", Json.obj(
      "width"  -> width,
      "height" -> height,
      "deviceScaleFactor" -> 0,
      "mobile" -> false
    ))
  }

  override def close(): Unit =
    try ws.sendClose(WebSocket.NORMAL_CLOSURE, "bye").get(2, TimeUnit.SECONDS)
    catch { case _: Throwable => () }
    finally {
      try ws.abort() catch { case _: Throwable => () }
      eventPool.shutdownNow()
    }
}
