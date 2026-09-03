package tools

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * Minimal embedded HTTP server used by `PageJsBehaviourSpec` to serve
 * rendered Twirl HTML over `http://localhost:<freePort>`. file:// URLs
 * can't host the page for CDP-driven tests because `history.replaceState`
 * (called by the date-filter ↔ URL sync to rewrite `?date=`) throws a
 * SecurityError on file://: the target URL isn't same-origin with the
 * served file's directory-scoped origin.
 *
 * Backed by JDK's `com.sun.net.httpserver.HttpServer` — no Play, no
 * dependency, no port collision (binds to a free port and exposes it via
 * `baseUrl`). Routes are a `PartialFunction[String, String]` so the test
 * can express the path-to-body mapping declaratively.
 */
class TestHttpServer(
  routes: PartialFunction[String, String],
  // JSON API routes (`/api/repertoire`, `/api/details`) the mobile apps
  // consume. Served as `application/json` with a `Last-Modified` header so the
  // Android `KinowoApi` / iOS `RepertoireStore` exercise the real wire
  // contract — not text/html like the page routes. Defaults to empty.
  jsonRoutes: PartialFunction[String, String] = PartialFunction.empty,
) extends AutoCloseable {
  // Stable HTTP-date stamped on every JSON response so clients can capture a
  // `Last-Modified` (and a future conditional-GET test has a value to echo
  // back). Anchored at the fixture snapshot midnight, GMT.
  private val jsonLastModified: String =
    java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(
      java.time.ZonedDateTime.of(2026, 5, 17, 0, 0, 0, 0, java.time.ZoneOffset.UTC))
  private val server: HttpServer = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)

  // THE CLASSLOADER OF WHOEVER BUILT THIS SERVER, pinned onto every handler
  // thread below. Under `sbt runMain` the application's classes and its
  // `reference.conf` / `application.conf` are reachable only through sbt's own
  // loader, which sbt installs as the CONTEXT classloader of the thread running
  // `main` — and nowhere else. `com.sun.net.httpserver` starts its own threads,
  // which inherit the JVM's system loader instead, so anything a route renders
  // that reaches for configuration by the context loader finds an empty config.
  //
  // What that looks like is worth spelling out, because it is unrecognisable:
  // the first render to touch `routes.Assets.versioned` (every page does, via
  // `_favicon`) initialises Play's global `StaticAssetsMetadata`, which reads
  // `Configuration` and throws `No configuration setting found for key 'play'`
  // with the origin "system properties" — the tell that `ConfigFactory.load()`
  // found no reference.conf at all. Every route then fails identically, whatever
  // it renders. It stayed hidden for as long as it did because `main` happened
  // to render one page eagerly before serving, warming that global on the right
  // thread; making that render lazy removed the warm-up and took the whole
  // browser and mobile suite down at once (2026-08-30).
  private val ownerLoader: ClassLoader = Thread.currentThread.getContextClassLoader

  server.createContext("/", new HttpHandler {
    override def handle(exception: HttpExchange): Unit = {
      Thread.currentThread.setContextClassLoader(ownerLoader)
      try {
        val path = exception.getRequestURI.getPath
        val rawQ = exception.getRequestURI.getRawQuery
        // Routes match on the path-plus-query, because several tests boot a
        // page with a parameter already in `location.search` (`/?date=tomorrow`)
        // and the route has to see it. Path-only routes (`/`,
        // `/movie/{slug}`) don't come through with a query attached, so they keep
        // matching on the bare path — `routeKey` is the path verbatim when
        // there's no query at all.
        val routeKey = if (rawQ == null) path else s"$path?$rawQ"
        // `/assets/*` is served from the web app's assets directory on disk
        // (`web/src/main/assets/*`, the same source Play's Assets controller
        // serves in prod) so the rendered page can load `shared.js` + the
        // inline-linked CSS in the test the same way prod does. Tests that
        // call shared.js functions (hideFilm, applyFilters, …) or assert on
        // rendered geometry need this — without shared.js every CDP eval hits
        // `ReferenceError: <fn> is not defined`. HTML routes still come from
        // `routes`; assets always fall through to disk.
        if (path.startsWith("/assets/")) {
          val rel  = path.stripPrefix("/assets/")
          val file = Paths.get("web/src/main/assets").resolve(rel).toAbsolutePath
          // Guard against `../` traversal — only serve files under the
          // assets directory (resolve + startsWith).
          val publicRoot = Paths.get("web/src/main/assets").toAbsolutePath
          if (!file.startsWith(publicRoot) || !Files.exists(file)) {
            exception.sendResponseHeaders(404, -1)
          } else {
            val bytes = Files.readAllBytes(file)
            val ct = if (path.endsWith(".css"))  "text/css; charset=UTF-8"
                     else if (path.endsWith(".js"))  "application/javascript; charset=UTF-8"
                     // Images need their real type: an <img> pointed at one of
                     // these is how the img-tracker's direct-origin probe is
                     // exercised, and Chrome refuses an SVG served as
                     // octet-stream outright.
                     else if (path.endsWith(".png")) "image/png"
                     else if (path.endsWith(".svg")) "image/svg+xml"
                     else if (path.endsWith(".webp")) "image/webp"
                     else if (path.endsWith(".jpg") || path.endsWith(".jpeg")) "image/jpeg"
                     else "application/octet-stream"
            exception.getResponseHeaders.add("Content-Type", ct)
            exception.sendResponseHeaders(200, bytes.length.toLong)
            val os = exception.getResponseBody
            try os.write(bytes) finally os.close()
          }
        } else if (jsonRoutes.isDefinedAt(routeKey)) {
          val bytes = jsonRoutes(routeKey).getBytes(StandardCharsets.UTF_8)
          exception.getResponseHeaders.add("Content-Type", "application/json; charset=UTF-8")
          exception.getResponseHeaders.add("Last-Modified", jsonLastModified)
          exception.sendResponseHeaders(200, bytes.length.toLong)
          val os = exception.getResponseBody
          try os.write(bytes) finally os.close()
        } else {
          routes.lift(routeKey) match {
          case Some(html) =>
            val bytes = html.getBytes(StandardCharsets.UTF_8)
            exception.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
            exception.sendResponseHeaders(200, bytes.length.toLong)
            val os = exception.getResponseBody
            try os.write(bytes) finally os.close()
          case None =>
            exception.sendResponseHeaders(404, -1)
          }
        }
      } catch {
        // A THROWING ROUTE USED TO LEAVE NO TRACE AT ALL, AND THAT COST HOURS.
        // Without this, the exception propagates into `com.sun.net.httpserver`,
        // which closes the connection with no headers written and logs the
        // throwable to a JUL logger that is off by default. What the other side
        // sees is `net::ERR_EMPTY_RESPONSE` (Playwright) or NSURLError -1011
        // (the iOS harness), on EVERY route at once, while the fixture server's
        // own log says "listening" and nothing else — so the whole browser and
        // mobile suite goes red pointing at a server that looks healthy. Hit on
        // 2026-08-30. The stack trace goes to stderr, which CI already uploads
        // as the `fixture-server-log-*` artifact.
        case t: Throwable =>
          System.err.println(s"[TestHttpServer] route ${exception.getRequestURI} threw:")
          t.printStackTrace()
          // Best-effort: if headers are already sent this throws in turn, and
          // the client gets the truncated body it was always going to get. The
          // trace above is the part that matters.
          try exception.sendResponseHeaders(500, -1) catch { case _: Throwable => () }
      } finally exception.close()
    }
  })
  server.start()

  val port: Int = server.getAddress.getPort
  val baseUrl: String = s"http://127.0.0.1:$port"

  override def close(): Unit = server.stop(0)
}
