package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * When does the OG-card generator route through the residential proxy?
 *
 * Only when BOTH credentials carry a value. GitHub Actions renders
 * `${{ secrets.X }}` of a secret that is not set (a fork, a rotated-away
 * secret) as an EMPTY STRING, so the env var is PRESENT but blank — and a
 * presence check alone then launched Chrome with `--proxy-server` and empty
 * Basic credentials, failing every card on a 407 instead of falling back to
 * the runner's own egress the way the generator's doc promises.
 *
 * Pure env logic — no Chrome needed.
 */
class OgCardProxyConfigSpec extends AnyFlatSpec with Matchers {

  private val creds = Map("KINOWO_PROXY_USER" -> "u", "KINOWO_PROXY_PASS" -> "p")

  "The proxy config" should "route through the default residential host when both credentials are set" in {
    OgCardGenerator.proxyConfigFor(10002, creds) shouldBe
      Some(Chrome.ProxyConfig("isp.decodo.com", 10002, "u", "p"))
  }

  it should "honour a host override" in {
    OgCardGenerator.proxyConfigFor(10003, creds + ("KINOWO_OG_PROXY_HOST" -> "proxy.example")).map(_.host) shouldBe
      Some("proxy.example")
  }

  it should "stay off when either credential is absent" in {
    OgCardGenerator.proxyConfigFor(10002, creds - "KINOWO_PROXY_PASS") shouldBe None
    OgCardGenerator.proxyConfigFor(10002, creds - "KINOWO_PROXY_USER") shouldBe None
  }

  it should "stay off when a credential is present but blank, the way GitHub Actions renders an unset secret" in {
    OgCardGenerator.proxyConfigFor(10002, creds + ("KINOWO_PROXY_USER" -> "")) shouldBe None
    OgCardGenerator.proxyConfigFor(10002, creds + ("KINOWO_PROXY_PASS" -> "  ")) shouldBe None
  }
}
