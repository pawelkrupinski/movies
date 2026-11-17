package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The origin whose local storage `Chrome.openPage` clears before it navigates. A URL with no host -- `about:blank`,
 * `data:` -- has no origin, so nothing to clear: Chrome 158 refuses `Storage.clearDataForOrigin` for the
 * `about://null` the scheme-and-host join made of it ("about://null is not a valid URL", Main #2586,
 * CdpSocketLossSpec), where Chrome 157 ignored it.
 */
class ChromeOriginSpec extends AnyFlatSpec with Matchers {

  "a page's origin" should "be its scheme, host and port" in {
    Chrome.originOf("http://127.0.0.1:9123/poznan/") shouldBe Some("http://127.0.0.1:9123")
    Chrome.originOf("https://kinowo.net/poznan/films") shouldBe Some("https://kinowo.net")
  }

  it should "be none for a URL without a host, which has no storage to clear" in {
    Chrome.originOf("about:blank") shouldBe None
    Chrome.originOf("data:text/html,hello") shouldBe None
  }
}
