package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetAddress, ServerSocket}
import java.nio.charset.StandardCharsets.ISO_8859_1
import scala.concurrent.duration._

/** The residential proxy's probe against a local stand-in proxy: it reads the CONNECT's status line, as Decodo's
 *  outages show (every port answering 503), and never goes past it. */
class ResidentialProxyProbeSpec extends AnyFlatSpec with Matchers {

  /** A one-shot proxy on a free local port that answers the first CONNECT with `statusLine`, and hands back the
   *  request it got. */
  private def proxyAnswering(statusLine: String)(probe: Int => Boolean): (Boolean, String) = {
    val server  = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val request = new java.util.concurrent.atomic.AtomicReference[String]("")
    val thread  = new Thread(() => {
      val socket = server.accept()
      try {
        val in   = new java.io.BufferedReader(new java.io.InputStreamReader(socket.getInputStream, ISO_8859_1))
        val head = Iterator.continually(in.readLine()).takeWhile(l => l != null && l.nonEmpty).mkString("\n")
        request.set(head)
        socket.getOutputStream.write(s"$statusLine\r\n\r\n".getBytes(ISO_8859_1))
        socket.getOutputStream.flush()
      } finally socket.close()
    })
    thread.setDaemon(true)
    thread.start()
    try {
      val passed = probe(server.getLocalPort)
      thread.join(SpecTimeouts.Io.toMillis)
      (passed, request.get)
    } finally server.close()
  }

  private def probeOn(port: Int) = new ResidentialProxyProbe(
    RealHttpFetch.ProxyConfig("127.0.0.1", Seq(port), settings.ProxyUser("user"), settings.ProxyPassword("pass")),
    timeout = 2.seconds)

  "the residential proxy probe" should "fail on the 503 every port answered during the outage" in {
    val (passed, request) = proxyAnswering("HTTP/1.1 503 Service Unavailable")(port => probeOn(port).passes())
    passed shouldBe false
    request should startWith (s"CONNECT ${ResidentialProxyProbe.Target} HTTP/1.1")
    request should include ("Proxy-Authorization: Basic dXNlcjpwYXNz")
  }

  it should "pass once a port tunnels again" in {
    proxyAnswering("HTTP/1.1 200 Connection established")(port => probeOn(port).passes())._1 shouldBe true
  }

  it should "fail, not throw, when no port answers at all" in {
    val closed = { val s = new ServerSocket(0, 1, InetAddress.getLoopbackAddress); val p = s.getLocalPort; s.close(); p }
    probeOn(closed).passes() shouldBe false
  }

  "a status line" should "be read for its code, and nothing else counts as one" in {
    ResidentialProxyProbe.statusOf("HTTP/1.1 200 Connection established") shouldBe Some(200)
    ResidentialProxyProbe.statusOf("HTTP/1.0 407 Proxy Authentication Required") shouldBe Some(407)
    ResidentialProxyProbe.statusOf("garbage 200") shouldBe None
    ResidentialProxyProbe.statusOf("") shouldBe None
  }
}
