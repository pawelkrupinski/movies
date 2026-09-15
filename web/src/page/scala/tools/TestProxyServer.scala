package tools

import java.io.{BufferedReader, InputStreamReader}
import java.net.{ServerSocket, Socket, URI}
import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Minimal HTTP forward proxy requiring Basic `Proxy-Authorization`, used by
 * `CdpProxyAuthSpec` to exercise `Chrome`'s proxy-auth handling
 * (`Fetch.authRequired` → `Fetch.continueWithAuth`) without depending on the
 * real Decodo residential proxy or any network egress. Speaks plain
 * absolute-form HTTP proxying only (no CONNECT/TLS tunneling) — sufficient
 * because the auth challenge/response dance is identical for both, and the
 * page under test is `TestHttpServer`'s own plain-HTTP listener.
 */
class TestProxyServer(user: String, pass: String) extends AutoCloseable {
  private val serverSocket = new ServerSocket(0)
  private val running = new AtomicBoolean(true)
  private val expectedAuth = "Basic " + Base64.getEncoder.encodeToString(s"$user:$pass".getBytes(StandardCharsets.UTF_8))

  val port: Int = serverSocket.getLocalPort

  private val acceptThread = new Thread(() => {
    while (running.get()) {
      try {
        val client = serverSocket.accept()
        val t = new Thread(() => handle(client))
        t.setDaemon(true)
        t.start()
      } catch { case _: Throwable => () } // includes the accept() that throws on close()
    }
  }, "test-proxy-accept")
  acceptThread.setDaemon(true)
  acceptThread.start()

  private def handle(client: Socket): Unit = {
    try {
      val in = new BufferedReader(new InputStreamReader(client.getInputStream, StandardCharsets.UTF_8))
      val requestLine = in.readLine()
      if (requestLine == null) return
      val headers = Iterator.continually(in.readLine()).takeWhile(l => l != null && l.nonEmpty).toList
      val out = client.getOutputStream
      val authOk = headers.exists(h =>
        h.toLowerCase.startsWith("proxy-authorization:") && h.split(":", 2)(1).trim == expectedAuth)
      if (!authOk) {
        val body = "auth required"
        out.write(
          s"HTTP/1.1 407 Proxy Authentication Required\r\nProxy-Authenticate: Basic realm=\"test\"\r\nContent-Length: ${body.length}\r\nConnection: close\r\n\r\n$body"
            .getBytes(StandardCharsets.UTF_8))
        out.flush()
        return
      }
      val parts  = requestLine.split(" ")
      val method = parts(0)
      if (method == "CONNECT") {
        // The path taken for an HTTPS navigation (real prod traffic is always
        // TLS) — auth happens at the CONNECT itself, before any TLS byte is
        // exchanged with the tunnel's far end, so answering it needs no real
        // TLS server on the other side. Once authenticated, closing rather
        // than relaying is deliberate: it makes Chrome's subsequent TLS
        // handshake fail FAST (a network error within the page's own
        // readyState wait) instead of the test needing a real HTTPS origin —
        // proving the CDP session stayed responsive is the point, not that a
        // page actually rendered.
        out.write("HTTP/1.1 200 Connection Established\r\n\r\n".getBytes(StandardCharsets.UTF_8))
        out.flush()
      } else {
        val uri = new URI(parts(1))
        val targetPort = if (uri.getPort > 0) uri.getPort else 80
        val target = new Socket(uri.getHost, targetPort)
        try {
          val path = Option(uri.getRawQuery).fold(uri.getRawPath)(q => s"${uri.getRawPath}?$q")
          val targetOut = target.getOutputStream
          targetOut.write(s"$method $path HTTP/1.1\r\n".getBytes(StandardCharsets.UTF_8))
          headers.filterNot(_.toLowerCase.startsWith("proxy-"))
            .foreach(h => targetOut.write(s"$h\r\n".getBytes(StandardCharsets.UTF_8)))
          targetOut.write("\r\n".getBytes(StandardCharsets.UTF_8))
          targetOut.flush()
          val buf = new Array[Byte](8192)
          val targetIn = target.getInputStream
          var n = targetIn.read(buf)
          while (n != -1) { out.write(buf, 0, n); n = targetIn.read(buf) }
        } finally target.close()
      }
    } catch { case _: Throwable => () }
    finally client.close()
  }

  override def close(): Unit = {
    running.set(false)
    try serverSocket.close() catch { case _: Throwable => () }
  }
}
