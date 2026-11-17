package tools

import testsupport.SourceTrees
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** A process that tunnels through the residential proxy must let java.net.http answer its Basic challenge, and only the
 *  process can say so — once, first ([[ProxyTunnelAuthentication]]). The convergence fill built the proxy without it,
 *  and every request it sent through the proxy came back 407 (run 37677032787): so every source holding a `main` that
 *  builds the proxy applies the policy. */
class ProxiedMainsAllowTunnelAuthSpec extends AnyFlatSpec with Matchers {

  private def sources: Seq[Path] = SourceTrees("worker/src/main/scala", "worker/src/fixtures/scala", "worker/src/test/scala", "scrapers/src/main/scala",
    "scrapers/src/test/scala", "scrapers/src/fixtures/scala", "common/src/main/scala", "http/src/main/scala", "identity/src/main/scala").scalaFiles

  "every main that builds the residential proxy" should "allow Basic auth on its tunnels first" in {
    val proxied = sources.filter { path =>
      val text = Files.readString(path)
      text.contains("def main(") && (text.contains("ResidentialShards.") || text.contains("residentialShards(") || text.contains("ResidentialProxy.from"))
    }
    proxied should not be empty
    withClue("these build the proxy without ProxyTunnelAuthentication.BasicAllowed.applyToJvm(): ") {
      proxied.filterNot(path => Files.readString(path).contains("ProxyTunnelAuthentication.BasicAllowed.applyToJvm()")) shouldBe empty
    }
  }
}
