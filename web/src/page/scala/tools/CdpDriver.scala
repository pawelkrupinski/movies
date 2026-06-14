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
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, TimeUnit}

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

  /** Launch a headless Chrome on a free port. Returns `None` when no
   *  Chrome binary is reachable on this machine. */
  def tryStart(): Option[Chrome] = findExecutable().flatMap { exe =>
    val port    = findFreePort()
    val userDirectory = Files.createTempDirectory("chrome-cdp-test-")
    val pb = new ProcessBuilder(
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
      s"--user-data-dir=${userDirectory.toString}",
      "about:blank"
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
      try {
        val info = httpGet(s"http://localhost:$port/json/version")
        val version = """"Browser":\s*"([^"]+)"""".r.findFirstMatchIn(info).map(_.group(1)).getOrElse("unknown")
        System.err.println(s"[CdpDriver] Chrome=$version path=$exe")
      } catch { case _: Throwable => () }
      Some(new Chrome(process, port, userDirectory))
    } else {
      process.destroyForcibly()
      None
    }
  }

  private def findFreePort(): Int = {
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()
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
                              userDataDirectory: Path
                            ) extends AutoCloseable {

  /** Open `url` in a fresh tab, run `body`, then close the tab. The page
   *  is loaded synchronously — `body` runs after `document.readyState`
   *  is `complete`, so DOMContentLoaded handlers (buildIndex, the
   *  boot-time applyFilters() in _sharedJs) have fired.
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

/** One CDP page session. Synchronous request/response over the WebSocket;
 *  events are ignored (we don't need them for the current spec). */
class CdpPage private[tools] (uri: URI) extends AutoCloseable {
  private val idGen = new AtomicInteger(0)
  private val pending = new ConcurrentHashMap[Int, CompletableFuture[JsValue]]()
  private val buffer  = new StringBuilder()

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
          (message \ "id").asOpt[Int].foreach { id =>
            Option(pending.remove(id)).foreach(_.complete(message))
          }
        }
        null
      }
    }).get(10, TimeUnit.SECONDS)

  /** Issue a CDP method call and return the `result` field of the reply.
   *  Blocks the caller until the reply arrives or 30s elapses. */
  def send(method: String, parameters: JsValue = Json.obj()): JsValue = {
    val id  = idGen.incrementAndGet()
    val fut = new CompletableFuture[JsValue]()
    pending.put(id, fut)
    val message = Json.obj("id" -> id, "method" -> method, "parameters" -> parameters).toString
    ws.sendText(message, true).get(5, TimeUnit.SECONDS)
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
    (result \ "exceptionDetails").asOpt[JsValue].foreach { ex =>
      throw new RuntimeException(s"JS exception evaluating [$js]: ${Json.stringify(ex)}")
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

  def reload(): Unit = {
    send("Page.reload")
    Thread.sleep(150) // give the reload a beat before waitFor polls
    waitFor("document.readyState === 'complete'", timeoutMs = 5000)
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
    finally try ws.abort() catch { case _: Throwable => () }
}
