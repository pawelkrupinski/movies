package tools

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * Minimal embedded HTTP server used by `PageJsBehaviourSpec` to serve
 * rendered Twirl HTML over `http://localhost:<freePort>`. file:// URLs
 * can't host the page for CDP-driven tests because `history.replaceState`
 * (called by /kina's pill toggle to rewrite the path to `/kina/<cinema>`)
 * throws a SecurityError on file://: the target URL isn't same-origin
 * with the served file's directory-scoped origin.
 *
 * Backed by JDK's `com.sun.net.httpserver.HttpServer` — no Play, no
 * dependency, no port collision (binds to a free port and exposes it via
 * `baseUrl`). Routes are a `PartialFunction[String, String]` so the test
 * can express the path-to-body mapping declaratively, including a
 * wildcard for `/kina/<cinema>` URL-path pinning.
 */
class TestHttpServer(
  routes: PartialFunction[String, String],
  // Served instead of `routes` when the request carries
  // `X-Requested-With: view-swap` — mirrors the production controller, which
  // returns just the `#view-root` fragment for an in-place Filmy↔Kina swap.
  // Defaults to empty so existing single-arg callers are unaffected.
  swapRoutes: PartialFunction[String, String] = PartialFunction.empty,
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
  server.createContext("/", new HttpHandler {
    override def handle(ex: HttpExchange): Unit = {
      try {
        val path = ex.getRequestURI.getPath
        val rawQ = ex.getRequestURI.getRawQuery
        // Routes match on the path-plus-query (`/film?title=…` carries
        // its identity in the query string, not the path). Existing
        // path-only routes (`/`, `/kina`, `/kina/<X>`) don't ever come
        // through with a query attached, so they keep matching on the
        // bare path — `routeKey` is the path verbatim when there's no
        // query at all.
        val routeKey = if (rawQ == null) path else s"$path?$rawQ"
        // `/assets/*` is served from the web app's assets dir on disk
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
          // assets dir (resolve + startsWith).
          val publicRoot = Paths.get("web/src/main/assets").toAbsolutePath
          if (!file.startsWith(publicRoot) || !Files.exists(file)) {
            ex.sendResponseHeaders(404, -1)
          } else {
            val bytes = Files.readAllBytes(file)
            val ct = if (path.endsWith(".css"))  "text/css; charset=UTF-8"
                     else if (path.endsWith(".js")) "application/javascript; charset=UTF-8"
                     else "application/octet-stream"
            ex.getResponseHeaders.add("Content-Type", ct)
            ex.sendResponseHeaders(200, bytes.length.toLong)
            val os = ex.getResponseBody
            try os.write(bytes) finally os.close()
          }
        } else if (jsonRoutes.isDefinedAt(routeKey)) {
          val bytes = jsonRoutes(routeKey).getBytes(StandardCharsets.UTF_8)
          ex.getResponseHeaders.add("Content-Type", "application/json; charset=UTF-8")
          ex.getResponseHeaders.add("Last-Modified", jsonLastModified)
          ex.sendResponseHeaders(200, bytes.length.toLong)
          val os = ex.getResponseBody
          try os.write(bytes) finally os.close()
        } else {
          val isSwap = Option(ex.getRequestHeaders.getFirst("X-Requested-With"))
                         .contains("view-swap")
          val body   = if (isSwap) swapRoutes.lift(routeKey).orElse(routes.lift(routeKey))
                       else        routes.lift(routeKey)
          body match {
          case Some(html) =>
            val bytes = html.getBytes(StandardCharsets.UTF_8)
            ex.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
            ex.sendResponseHeaders(200, bytes.length.toLong)
            val os = ex.getResponseBody
            try os.write(bytes) finally os.close()
          case None =>
            ex.sendResponseHeaders(404, -1)
          }
        }
      } finally ex.close()
    }
  })
  server.start()

  val port: Int = server.getAddress.getPort
  val baseUrl: String = s"http://127.0.0.1:$port"

  override def close(): Unit = server.stop(0)
}
