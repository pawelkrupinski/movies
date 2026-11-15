package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetAddress
import java.net.spi.InetAddressResolver.LookupPolicy
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.Duration

/** [[AddressFailoverSpec]]'s case end to end: ES "Ocine Gavarres" (tickets.ocinegavarres.es, 2026-11-10..15), two
 *  addresses, one dropping every connection, and the JDK's client tried only the first. */
class AddressFailoverIntegrationSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private lazy val harness = settings.HarnessConfiguration.resolve()

  // The whole path, in a JVM of its own (the resolver SPI loads at a JVM's start), its lookups uncached as the image
  // runs them: the failover resolver over a fixed table (TableAddressResolverProvider, registered ahead of the class
  // path -- the JDK's hosts-file option would bypass every resolver provider). The host resolves to 127.0.0.2 first,
  // where nothing listens, then to 127.0.0.1, where the answer is.
  "a RealHttpFetch" should "reach a host through its second address when its first will not connect" in {
    AnsweringHttpServer.answering(200, "answered", Map.empty) { uri =>
      val services = tempDirectory("failover-services")
      Files.createDirectories(services.resolve("META-INF/services"))
      Files.writeString(services.resolve("META-INF/services/java.net.spi.InetAddressResolverProvider"),
        "tools.TableAddressResolverProvider\n", StandardCharsets.UTF_8)
      Files.writeString(services.resolve("failover-table.txt"), "failover.test=127.0.0.2,127.0.0.1\n", StandardCharsets.UTF_8)
      val (code, output) = ChildJvm(harness).run("tools.AddressFailoverProbe", Seq(s"http://failover.test:${uri.getPort}/"),
        jvmArgs = Seq("-Dsun.net.inetaddr.ttl=0", "-Djava.awt.headless=true"),
        classPathFirst = Seq(services))
      withClue(output) {
        code shouldBe 0
        output should include("body: answered")
      }
    }
  }
}

/** [[AddressFailoverIntegrationSpec]]'s probe: one GET through a [[RealHttpFetch]] with a short connect budget, in its own JVM. */
object AddressFailoverProbe {
  def main(args: Array[String]): Unit = {
    val fetch = new RealHttpFetch(connectTimeoutFor = _ => Duration.ofSeconds(2))
    val body  = scala.util.Try(fetch.get(args(0)))
    println(body.fold(failure => s"failed: $failure", text => s"body: $text"))
    sys.exit(if (body.isSuccess) 0 else 1)
  }
}

/** The probe JVM's resolver: [[FailoverAddressResolver]] over the table `failover-table.txt` (`host=ip,ip` a line, ahead on the class path) names, through
 *  the same failover state [[RealHttpFetch]] reports to. */
final class TableAddressResolverProvider extends java.net.spi.InetAddressResolverProvider {
  override def get(configuration: java.net.spi.InetAddressResolverProvider.Configuration): java.net.spi.InetAddressResolver = {
    val builtin = configuration.builtinResolver()
    val rows  = Option(getClass.getResourceAsStream("/failover-table.txt")).fold("")(in => new String(in.readAllBytes(), StandardCharsets.UTF_8))
    val table = rows.split('\n').filter(_.contains('=')).map { row =>
      val Array(host, ips) = row.split('=')
      host -> ips.split(',').toSeq.map(ip => InetAddress.getByAddress(host, ip.split('.').map(_.toInt.toByte)))
    }.toMap
    val tabled = new java.net.spi.InetAddressResolver {
      override def lookupByName(host: String, policy: LookupPolicy): java.util.stream.Stream[InetAddress] =
        table.get(host).fold(builtin.lookupByName(host, policy))(addresses => java.util.stream.Stream.of(addresses*))
      override def lookupByAddress(address: Array[Byte]): String = builtin.lookupByAddress(address)
    }
    new FailoverAddressResolver(FailoverAddressResolverProvider.failover, tabled)
  }
  override def name(): String = "address-failover-spec-table"
}
