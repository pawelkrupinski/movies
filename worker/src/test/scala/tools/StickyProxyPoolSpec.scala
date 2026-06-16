package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StickyProxyPoolSpec extends AnyFlatSpec with Matchers {

  private def pool(ports: Int*) =
    new StickyProxyPool(RealHttpFetch.ProxyConfig("isp.decodo.com", ports, "u", "p"))

  "StickyProxyPool" should "give each successive client the next pool IP, round-robin" in {
    val p = pool(10001, 10002, 10003)
    p.nextEgress().port shouldBe 10001
    p.nextEgress().port shouldBe 10002
    p.nextEgress().port shouldBe 10003
  }

  // The regression guard: before the fix every proxied client shared ONE pinned
  // IP (ports.head), funnelling all auth through it and tripping Decodo's
  // "Limit: 3". Each client must now land on a distinct egress.
  it should "spread distinct clients across distinct IPs" in {
    val p = pool(10001, 10002, 10003, 10004, 10005, 10006, 10007)
    val sevenClients = (1 to 7).map(_ => p.nextEgress().port)
    sevenClients.distinct.size shouldBe 7
  }

  it should "wrap round-robin once the pool is exhausted" in {
    val p = pool(10001, 10002)
    Seq.fill(4)(p.nextEgress().port) shouldBe Seq(10001, 10002, 10001, 10002)
  }
}
