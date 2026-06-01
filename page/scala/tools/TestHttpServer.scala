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
) extends AutoCloseable {
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
        // `/assets/*` is served from `public/*` on disk so the rendered
        // page can `<link>` Bootstrap CSS in the test the same way prod
        // does. Tests that assert on rendered geometry (e.g. the mobile
        // navbar layout test in `PageJsBehaviourSpec`) need this — the
        // page's flex layout depends entirely on Bootstrap's `.d-flex`
        // rules; without them the navbar collapses to a vertical stack
        // and assertions on element rects all fail. HTML routes still
        // come from `routes`; assets always fall through to disk.
        if (path.startsWith("/assets/")) {
          val rel  = path.stripPrefix("/assets/")
          val file = Paths.get("public").resolve(rel).toAbsolutePath
          // Guard against `../` traversal — only serve files under
          // `public/` (resolve + startsWith).
          val publicRoot = Paths.get("public").toAbsolutePath
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
