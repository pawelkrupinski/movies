package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.Properties

class ResidentialProxySpec extends AnyFlatSpec with Matchers {

  private def props(host: String, ports: String): Properties = {
    val p = new Properties()
    if (host != null) p.setProperty("host", host)
    if (ports != null) p.setProperty("ports", ports)
    p
  }

  "ResidentialProxy.fromConfig" should
    "build a ProxyConfig when host+ports (file) and user+pass (env) are all present" in {
    val cfg = ResidentialProxy.fromConfig(props("isp.decodo.com", "10001, 10002 ,10003"), Some("u"), Some("p"))
    cfg.map(_.host)  shouldBe Some("isp.decodo.com")
    cfg.map(_.ports) shouldBe Some(Seq(10001, 10002, 10003)) // parsed + trimmed
    cfg.map(_.user)  shouldBe Some("u")
  }

  it should "be None when the secret credentials are absent (proxy disabled → Zyte/direct)" in {
    ResidentialProxy.fromConfig(props("isp.decodo.com", "10001"), None, Some("p"))      shouldBe None
    ResidentialProxy.fromConfig(props("isp.decodo.com", "10001"), Some("u"), None)      shouldBe None
    ResidentialProxy.fromConfig(props("isp.decodo.com", "10001"), Some("  "), Some("p")) shouldBe None
  }

  it should "be None when the properties file has no host or no usable ports" in {
    ResidentialProxy.fromConfig(props(null, "10001"), Some("u"), Some("p"))              shouldBe None
    ResidentialProxy.fromConfig(props("isp.decodo.com", null), Some("u"), Some("p"))     shouldBe None
    ResidentialProxy.fromConfig(props("isp.decodo.com", " , ,"), Some("u"), Some("p"))   shouldBe None
  }

  "the committed residential-proxy.properties" should "carry the Decodo host and a non-empty port list" in {
    val loaded = ResidentialProxy.loadProperties("/residential-proxy.properties")
    loaded.getProperty("host") shouldBe "isp.decodo.com"
    ResidentialProxy.parsePorts(loaded.getProperty("ports")) should not be empty
  }
}
