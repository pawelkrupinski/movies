package tools

import com.sun.net.httpserver.HttpServer

import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets

/** A local HTTP server answering every request alike, for a client spec that must read a real
 *  response — its status, body and headers — rather than a fixture's body alone. */
object AnsweringHttpServer {
  /** A server answering every request with `status`, `body` and `headers`, for the length of `use`. */
  def answering[T](status: Int, body: String, headers: Map[String, String])(use: URI => T): T = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/", exchange => {
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
      exchange.sendResponseHeaders(status, bytes.length.toLong)
      exchange.getResponseBody.write(bytes)
      exchange.close()
    })
    server.start()
    try use(URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/")) finally server.stop(0)
  }
}
