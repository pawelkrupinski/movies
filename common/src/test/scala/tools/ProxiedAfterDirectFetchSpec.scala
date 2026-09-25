package tools

import com.sun.net.httpserver.{HttpServer, HttpsConfigurator, HttpsServer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{InputStream, OutputStream}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.{Files, Path, Paths}
import java.security.KeyStore
import java.util.Base64
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import javax.net.ssl.{KeyManagerFactory, SSLContext}
import scala.util.{Try, Using}

/**
 * A DIRECT fetch first, then a PROXIED one, in one fresh JVM — the order a real
 * job runs them in, against a local proxy that, like Decodo, answers an
 * unauthenticated HTTPS `CONNECT` with 407 and wants Basic credentials.
 *
 * WHY A FRESH JVM. java.net.http reads `jdk.http.auth.tunneling.disabledSchemes`
 * (JDK default: "Basic") ONCE, the first time anything in the process touches it,
 * and never again. The roster audit made ~170 direct page reads before building
 * its proxy shards; clearing the property only when a `ProxyConfig` was built came
 * too late, and every proxied chain-list request 407'd (4097c855b). Whether that
 * bites depends on what ran first in the process — so the spec's own JVM, where
 * any earlier spec may already have frozen the value either way, cannot answer
 * it. The child JVM below runs exactly one direct fetch and then one proxied
 * fetch, with nothing before them.
 *
 * No live proxy and no network: the proxy, the plain-HTTP origin and the HTTPS
 * origin (a throwaway self-signed certificate the child is told to trust) are
 * all on loopback.
 */
class ProxiedAfterDirectFetchSpec extends AnyFlatSpec with Matchers {
  import ProxiedAfterDirectFetchSpec._

  "a proxied RealHttpFetch" should "authenticate its CONNECT tunnel even when a direct fetch ran first in the JVM" in {
    val dir = Files.createTempDirectory("proxied-after-direct")
    val (keyStore, trustStore) = selfSignedStores(dir)

    val direct = HttpServer.create(new InetSocketAddress(Loopback, 0), 0)
    direct.createContext("/", exchange => respond(exchange, "direct-ok"))
    val origin = HttpsServer.create(new InetSocketAddress(Loopback, 0), 0)
    origin.setHttpsConfigurator(new HttpsConfigurator(serverContext(keyStore)))
    origin.createContext("/", exchange => respond(exchange, "through-the-tunnel"))
    val proxy = new BasicAuthConnectProxy(User, Password)
    Seq(direct, origin).foreach(_.start())

    try {
      val (exit, output) = ChildJvm.run(
        ProbeMain,
        jvmArgs = Seq(s"-Djavax.net.ssl.trustStore=$trustStore", s"-Djavax.net.ssl.trustStorePassword=$StorePassword"),
        args = Seq(s"http://127.0.0.1:${direct.getAddress.getPort}/", proxy.port.toString,
            s"https://127.0.0.1:${origin.getAddress.getPort}/", User, Password))

      withClue(s"child output:\n$output\n") {
        exit shouldBe 0
        output should include("direct: direct-ok")
        output should include("proxied: through-the-tunnel")
        // The tunnel was refused unauthenticated and then opened WITH credentials —
        // not reached some other way.
        proxy.challenged.get should be >= 1
        proxy.authenticated.get shouldBe 1
      }
    } finally {
      direct.stop(0); origin.stop(0); proxy.close()
      Using.resource(Files.list(dir))(_.forEach(Files.delete(_)))
      Files.delete(dir)
    }
  }
}

object ProxiedAfterDirectFetchSpec {
  private val Loopback      = InetAddress.getLoopbackAddress
  private val User          = "proxy-user"
  private val Password      = "proxy-pass"
  private val StorePassword = "changeit"
  private val ProbeMain     = "tools.ProxiedAfterDirectFetchProbe"

  private def respond(exchange: com.sun.net.httpserver.HttpExchange, body: String): Unit = {
    val bytes = body.getBytes(UTF_8)
    exchange.sendResponseHeaders(200, bytes.length)
    Using.resource(exchange.getResponseBody)(_.write(bytes))
  }

  /** A PKCS12 key store holding a self-signed certificate for 127.0.0.1, and a
   *  trust store holding just that certificate — made with the JDK's own keytool,
   *  since the JDK has no public API for minting a certificate. */
  private def selfSignedStores(dir: Path): (Path, Path) = {
    val keytool = Paths.get(System.getProperty("java.home"), "bin", "keytool").toString
    val keys    = dir.resolve("keys.p12")
    val cert    = dir.resolve("cert.pem")
    val trust   = dir.resolve("trust.p12")
    def run(args: String*): Unit = {
      val process = new ProcessBuilder((keytool +: args)*).redirectErrorStream(true).start()
      val output  = new String(process.getInputStream.readAllBytes(), UTF_8)
      require(process.waitFor(60, TimeUnit.SECONDS) && process.exitValue() == 0, s"keytool ${args.head} failed: $output")
    }
    run("-genkeypair", "-alias", "origin", "-keyalg", "RSA", "-keysize", "2048", "-validity", "2",
      "-dname", "CN=127.0.0.1", "-ext", "SAN=ip:127.0.0.1", "-storetype", "PKCS12",
      "-keystore", keys.toString, "-storepass", StorePassword, "-keypass", StorePassword)
    run("-exportcert", "-rfc", "-alias", "origin", "-keystore", keys.toString, "-storepass", StorePassword,
      "-file", cert.toString)
    run("-importcert", "-noprompt", "-alias", "origin", "-file", cert.toString, "-storetype", "PKCS12",
      "-keystore", trust.toString, "-storepass", StorePassword)
    (keys, trust)
  }

  private def serverContext(keyStore: Path): SSLContext = {
    val ks = KeyStore.getInstance("PKCS12")
    Using.resource(Files.newInputStream(keyStore))(ks.load(_, StorePassword.toCharArray))
    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(ks, StorePassword.toCharArray)
    val context = SSLContext.getInstance("TLS")
    context.init(kmf.getKeyManagers, null, null)
    context
  }

  /** An HTTP proxy that only tunnels: `CONNECT` without the right Basic
   *  credentials gets 407 + a Basic challenge (what Decodo answers), with them it
   *  gets a byte pipe to the named host. Counts both, so the spec can tell a tunnel
   *  opened with credentials from one opened any other way. */
  final class BasicAuthConnectProxy(user: String, password: String) extends AutoCloseable {
    private val server    = new ServerSocket(0, 50, Loopback)
    private val expected  = "Basic " + Base64.getEncoder.encodeToString(s"$user:$password".getBytes(UTF_8))
    val challenged        = new AtomicInteger
    val authenticated     = new AtomicInteger
    def port: Int         = server.getLocalPort

    private val acceptor = daemon { () =>
      while (!server.isClosed) Try(server.accept()).foreach(client => daemon(() => serve(client)))
    }

    private def serve(client: Socket): Unit = Using.resource(client) { client =>
      val in  = client.getInputStream
      val out = client.getOutputStream
      var open = true
      while (open) {
        val head = readHead(in)
        if (head.isEmpty) open = false
        else {
          val requestLine = head.head.split(" ")
          val headers     = head.tail.flatMap(line => line.split(":", 2) match {
            case Array(k, v) => Some(k.trim.toLowerCase -> v.trim)
            case _           => None
          }).toMap
          if (requestLine(0) != "CONNECT") { write(out, "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n"); open = false }
          else if (!headers.get("proxy-authorization").contains(expected)) {
            challenged.incrementAndGet()
            write(out, "HTTP/1.1 407 Proxy Authentication Required\r\n" +
              "Proxy-Authenticate: Basic realm=\"test\"\r\nContent-Length: 0\r\n\r\n")
          } else {
            authenticated.incrementAndGet()
            val Array(host, targetPort) = requestLine(1).split(":")
            Using.resource(new Socket(host, targetPort.toInt)) { upstream =>
              write(out, "HTTP/1.1 200 Connection established\r\n\r\n")
              val back = daemon(() => Try(upstream.getInputStream.transferTo(out)))
              Try(in.transferTo(upstream.getOutputStream))
              Try(upstream.shutdownOutput())
              back.join(10000)
            }
            open = false
          }
        }
      }
    }

    /** One request head, byte by byte so nothing past it is consumed; empty at EOF. */
    private def readHead(in: InputStream): Seq[String] = {
      val bytes = new java.io.ByteArrayOutputStream
      var last4 = 0
      var b     = in.read()
      while (b != -1 && last4 != 0x0d0a0d0a) {
        bytes.write(b); last4 = (last4 << 8) | b
        if (last4 != 0x0d0a0d0a) b = in.read()
      }
      if (bytes.size == 0) Nil
      else new String(bytes.toByteArray, ISO_8859_1).split("\r\n").toSeq.filter(_.nonEmpty)
    }

    private def write(out: OutputStream, s: String): Unit = { out.write(s.getBytes(ISO_8859_1)); out.flush() }

    private def daemon(body: () => Unit): Thread = {
      val t = new Thread(() => body()); t.setDaemon(true); t.start(); t
    }

    override def close(): Unit = { server.close(); acceptor.join(5000) }
  }
}

/** The child JVM's whole life: one direct fetch, then one proxied fetch, each
 *  through a fresh `RealHttpFetch` — nothing touches java.net.http before them. */
object ProxiedAfterDirectFetchProbe {
  def main(args: Array[String]): Unit = {
    val Array(directUrl, proxyPort, proxiedUrl, user, password) = args
    val outcome = Try {
      println(s"direct: ${new RealHttpFetch().get(directUrl)}")
      val proxied = new RealHttpFetch(Some(RealHttpFetch.ProxyConfig("127.0.0.1", Seq(proxyPort.toInt), user, password)))
      println(s"proxied: ${proxied.get(proxiedUrl)}")
    }
    outcome.failed.foreach(_.printStackTrace(System.out))
    System.out.flush()
    System.exit(if (outcome.isSuccess) 0 else 1)
  }
}
