package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResidentialShardsSpec extends AnyFlatSpec with Matchers {
  private val tls = TlsTrust.newContext()

  "ResidentialShards" should "build one pinned fetch per pool port" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), settings.ProxyUser("u"), settings.ProxyPassword("p"))
    ResidentialShards(Some(pool), tls).map(_.size) shouldBe Some(3)
  }

  it should "build none when the proxy isn't configured" in {
    ResidentialShards(None, tls) shouldBe None
    ResidentialShards.fromConfiguration(new settings.ProcessConfiguration(Env.of()), tls) shouldBe None
  }
}
