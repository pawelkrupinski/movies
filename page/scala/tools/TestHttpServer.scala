package tools

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

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
class TestHttpServer(routes: PartialFunction[String, String]) extends AutoCloseable {
  private val server: HttpServer = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  server.createContext("/", new HttpHandler {
    override def handle(ex: HttpExchange): Unit = {
      try {
        val path = ex.getRequestURI.getPath
        routes.lift(path) match {
          case Some(html) =>
            val bytes = html.getBytes(StandardCharsets.UTF_8)
            ex.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
            ex.sendResponseHeaders(200, bytes.length.toLong)
            val os = ex.getResponseBody
            try os.write(bytes) finally os.close()
          case None =>
            ex.sendResponseHeaders(404, -1)
        }
      } finally ex.close()
    }
  })
  server.start()

  val port: Int = server.getAddress.getPort
  val baseUrl: String = s"http://127.0.0.1:$port"

  override def close(): Unit = server.stop(0)
}
