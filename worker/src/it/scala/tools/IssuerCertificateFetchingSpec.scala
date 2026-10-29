package tools

import com.sun.net.httpserver.{HttpServer, HttpsConfigurator, HttpsServer}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{BufferedReader, InputStreamReader, Writer}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.cert.Certificate
import java.security.{KeyStore, PrivateKey}
import java.util.concurrent.TimeUnit
import javax.net.ssl.{KeyManagerFactory, SSLContext}
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.{Try, Using}

/**
 * A server that sends its leaf certificate WITHOUT the intermediate that issued it — the
 * artmuseum.pl shape ([[TlsTrust]], case 2a) — while the leaf names where that intermediate
 * can be fetched (its Authority-Information-Access `caIssuers` URL, served on loopback).
 * The client trusts only the root, so the handshake succeeds only if the JDK fetches the
 * intermediate.
 *
 * In a CHILD JVM, because the JDK reads [[IssuerCertificateFetching.Property]] once, when its
 * path builder first loads, and the spec's own JVM may already have loaded it either way —
 * and because setting it in the spec's JVM would set it for every suite beside it. The servers
 * run in a second child JVM, out of reach of whatever the suites beside it do to the spec's.
 */
class IssuerCertificateFetchingSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private lazy val configuration = settings.ProcessConfiguration.resolve()

  import IssuerCertificateFetchingSpec._

  "a fetch to a server that omits its intermediate" should "succeed when the process's main enabled issuer fetching" in {
    val (exit, output) = handshake(applied = Some(IssuerCertificateFetching.Enabled))
    withClue(s"child output:\n$output\n") {
      exit shouldBe 0
      output should include("fetched: leaf-only-ok")
    }
  }

  it should "succeed the same way in a JVM started with the policy as an option" in {
    val (exit, output) = handshake(applied = None, jvmOptions = IssuerCertificateFetching.Enabled.jvmOptions)
    withClue(s"child output:\n$output\n")(exit shouldBe 0)
  }

  // The control: building a TLS context no longer switches fetching on behind the process's
  // back, so a process that never applied the policy keeps the JDK default and the chain
  // cannot be completed.
  it should "fail PKIX when the process kept the JDK default" in {
    val (exit, output) = handshake(applied = None)
    withClue(s"child output:\n$output\n") {
      exit should not be 0
      output should include("PKIX")
    }
  }

  /** The servers and the chain, minted ONCE for the suite: minting is ten keytool runs (a JVM
   *  each), and nothing a child does changes them — every child JVM is still a fresh process
   *  that has fetched nothing. They serve from a JVM of their own, not the spec's: `itAll` runs
   *  suites in parallel in one JVM, and a neighbour forcing full collections (LiveHeapSpec's class
   *  histograms) stopped every thread in it, the servers' with them, past the probe's 5 s connect
   *  budget, which bounds the TLS handshake and the intermediate's fetch within it (Main #2607). */
  private final case class Serving(process: Process, originPort: Int, rootTrustStore: String)

  private var started: Option[Serving] = None
  private def serving: Serving = synchronized { started.getOrElse { val s = startServers(); started = Some(s); s } }

  private def startServers(): Serving = {
    val process = ChildJvm(configuration).start(ServersMain)
    val lines   = new BufferedReader(new InputStreamReader(process.getInputStream, UTF_8))
    val seen    = new StringBuffer
    val ready   = Future(blocking(Iterator.continually(lines.readLine()).takeWhile(_ != null)
      .map { line => seen.append(line).append('\n'); line }.find(_.startsWith(Ready))))(using ExecutionContext.global)
    Try(Await.result(ready, SpecTimeouts.Run)).toOption.flatten match {
      case Some(line) =>
        // Drained from here on, so nothing the servers print can fill the pipe and stall them.
        val drain = new Thread(() => { Try(lines.transferTo(Writer.nullWriter())); () })
        drain.setDaemon(true); drain.start()
        val Array(_, port, trustStore) = line.split(" ", 3)
        Serving(process, port.toInt, trustStore)
      case None =>
        process.destroyForcibly()
        fail(s"the servers' JVM never became ready:\n$seen")
    }
  }

  // Closing its stdin is the servers' JVM's signal to stop and delete what it minted.
  override def afterAll(): Unit = try synchronized(started.foreach { serving =>
    serving.process.getOutputStream.close()
    if (!serving.process.waitFor(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)) serving.process.destroyForcibly()
  }) finally super.afterAll()

  private def handshake(applied: Option[IssuerCertificateFetching], jvmOptions: Seq[String] = Nil): (Int, String) =
    ChildJvm(configuration).run(ProbeMain,
      jvmArgs = Seq(s"-Djavax.net.ssl.trustStore=${serving.rootTrustStore}", s"-Djavax.net.ssl.trustStorePassword=$StorePassword") ++ jvmOptions,
      args = Seq(applied.fold(NoPolicy)(_.toString), s"https://127.0.0.1:${serving.originPort}/"))
}

object IssuerCertificateFetchingSpec {
  private val Loopback      = InetAddress.getLoopbackAddress
  private val StorePassword = "changeit"
  private val ProbeMain     = "tools.IssuerCertificateFetchingProbe"
  private val ServersMain   = "tools.IssuerCertificateFetchingServers"
  /** The first word of the line the servers' JVM prints once both serve: `ready <origin port> <root trust store>`. */
  private[tools] val Ready  = "ready"
  /** The probe's first argument when its `main` applies no policy at all. */
  private[tools] val NoPolicy = "none"

  /** The AIA server (plain HTTP, serving the intermediate) and the origin (HTTPS, presenting the
   *  leaf alone). The AIA server starts first: its port goes into the leaf. */
  private[tools] final class Servers(keytool: Keytool) {
    val dir     = Files.createTempDirectory("issuer-fetching")
    val issuers = HttpServer.create(new InetSocketAddress(Loopback, 0), 0)
    issuers.start()
    val chain   = Chain.mint(keytool, dir, s"http://127.0.0.1:${issuers.getAddress.getPort}/intermediate.cer")
    issuers.createContext("/intermediate.cer", exchange => {
      val bytes = Files.readAllBytes(chain.intermediateDer)
      exchange.getResponseHeaders.set("Content-Type", "application/pkix-cert")
      exchange.sendResponseHeaders(200, bytes.length)
      Using.resource(exchange.getResponseBody)(_.write(bytes))
    })
    val origin  = HttpsServer.create(new InetSocketAddress(Loopback, 0), 0)
    origin.setHttpsConfigurator(new HttpsConfigurator(chain.leafOnlyServerContext))
    origin.createContext("/", exchange => {
      val bytes = "leaf-only-ok".getBytes(UTF_8)
      exchange.sendResponseHeaders(200, bytes.length)
      Using.resource(exchange.getResponseBody)(_.write(bytes))
    })
    origin.start()

    def close(): Unit = {
      origin.stop(0); issuers.stop(0)
      Using.resource(Files.list(dir))(_.forEach(Files.delete(_)))
      Files.delete(dir)
    }
  }

  /** root → intermediate → leaf (for 127.0.0.1, naming `aiaUrl` as its issuer's location),
   *  minted with the JDK's own keytool, since the JDK has no public API for it. */
  private[tools] final case class Chain(keys: Path, intermediateDer: Path, rootTrustStore: Path) {

    /** A server context presenting the leaf ALONE — no intermediate in the chain it sends. */
    def leafOnlyServerContext: SSLContext = {
      val full = KeyStore.getInstance("PKCS12")
      Using.resource(Files.newInputStream(keys))(full.load(_, StorePassword.toCharArray))
      val key  = full.getKey("leaf", StorePassword.toCharArray).asInstanceOf[PrivateKey]
      val leaf = full.getCertificate("leaf")
      val served = KeyStore.getInstance("PKCS12")
      served.load(null, null)
      served.setKeyEntry("leaf", key, StorePassword.toCharArray, Array[Certificate](leaf))
      val factory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      factory.init(served, StorePassword.toCharArray)
      val context = SSLContext.getInstance("TLS")
      context.init(factory.getKeyManagers, null, null)
      context
    }
  }

  private[tools] object Chain {
    def mint(keytool: Keytool, dir: Path, aiaUrl: String): Chain = {
      val keys  = dir.resolve("keys.p12")
      val trust = dir.resolve("trust.p12")
      def file(name: String) = dir.resolve(name).toString
      val store = Seq("-storetype", "PKCS12", "-keystore", keys.toString, "-storepass", StorePassword, "-keypass", StorePassword)
      def pair(alias: String, name: String, extensions: String*) =
        keytool.run(Seq("-genkeypair", "-alias", alias, "-keyalg", "RSA", "-keysize", "2048", "-validity", "2", "-dname", s"CN=$name") ++
          extensions.flatMap(Seq("-ext", _)) ++ store*)
      pair("root", "Issuer Fetching Test Root", "bc:c=ca:true")
      pair("intermediate", "Issuer Fetching Test Intermediate")
      pair("leaf", "127.0.0.1")
      keytool.run(Seq("-exportcert", "-rfc", "-alias", "root", "-file", file("root.pem")) ++ store*)
      keytool.run(Seq("-certreq", "-alias", "intermediate", "-file", file("intermediate.csr")) ++ store*)
      keytool.run(Seq("-gencert", "-alias", "root", "-ext", "bc:c=ca:true", "-validity", "2",
        "-infile", file("intermediate.csr"), "-outfile", file("intermediate.der")) ++ store*)
      keytool.run(Seq("-certreq", "-alias", "leaf", "-file", file("leaf.csr")) ++ store*)
      keytool.run(Seq("-gencert", "-alias", "intermediate", "-validity", "2", "-ext", "SAN=ip:127.0.0.1",
        "-ext", s"AIA=caIssuers:uri:$aiaUrl", "-infile", file("leaf.csr"), "-outfile", file("leaf.der")) ++ store*)
      // Install the signed certs so the leaf entry carries its real key + signed cert.
      keytool.run(Seq("-importcert", "-noprompt", "-alias", "intermediate", "-file", file("intermediate.der")) ++ store*)
      keytool.run(Seq("-importcert", "-noprompt", "-alias", "leaf", "-file", file("leaf.der")) ++ store*)
      keytool.run(Seq("-importcert", "-noprompt", "-alias", "root", "-file", file("root.pem"),
        "-storetype", "PKCS12", "-keystore", trust.toString, "-storepass", StorePassword)*)
      Seq("root.pem", "intermediate.csr", "leaf.csr", "leaf.der").foreach(name => Files.delete(dir.resolve(name)))
      Chain(keys, dir.resolve("intermediate.der"), trust)
    }
  }
}

/** The servers' JVM: mints the chain, serves until its stdin closes, then deletes what it minted. */
object IssuerCertificateFetchingServers {
  def main(args: Array[String]): Unit = {
    val servers = new IssuerCertificateFetchingSpec.Servers(new Keytool(settings.ProcessConfiguration.resolve().javaHome))
    try {
      println(s"${IssuerCertificateFetchingSpec.Ready} ${servers.origin.getAddress.getPort} ${servers.chain.rootTrustStore}")
      System.out.flush()
      System.in.readAllBytes()
    } finally servers.close()
    System.exit(0)
  }
}

/** The child JVM's whole life: apply the named [[IssuerCertificateFetching]] (or none) the way
 *  a composition root does, then one fetch through a fresh `RealHttpFetch`. */
object IssuerCertificateFetchingProbe {
  def main(args: Array[String]): Unit = {
    val Array(policy, url) = args
    if (policy != IssuerCertificateFetchingSpec.NoPolicy) IssuerCertificateFetching.valueOf(policy).applyToJvm()
    val outcome = Try(println(s"fetched: ${new RealHttpFetch().get(url)}"))
    outcome.failed.foreach(_.printStackTrace(System.out))
    System.out.flush()
    System.exit(if (outcome.isSuccess) 0 else 1)
  }
}
