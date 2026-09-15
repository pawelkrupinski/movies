package tools

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.compiletime.uninitialized

/**
 * Regression for the OG-card generator rendering non-English deployments
 * (Poland, Germany, Spain) with English nav/day-tab/search chrome under an
 * otherwise-correctly-translated overlay tagline: `WebLangResolver` prefers a
 * request's own `Accept-Language` over the deployment's fixed default, and
 * the headless Chrome driving `OgCardGenerator` sends whatever the RUNNER's
 * own locale is (`en-US` on most CI/dev machines) — never the deployment's
 * language — so every non-English card screenshot rendered in English
 * regardless of `KINOWO_COUNTRY`.
 *
 * `Chrome.openPage`'s `acceptLanguage` parameter fixes this by overriding the
 * header via CDP before navigating. This spec proves the override actually
 * reaches the server on the wire (`TestHttpServer`'s `/__echo-accept-language`
 * echo route), independent of whether any particular page happens to honour
 * the header — that's `WebLangResolver`'s job, covered elsewhere.
 */
class CdpAcceptLanguageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = uninitialized

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart()
    server = new TestHttpServer(routes = PartialFunction.empty)
  }
  override def afterAll(): Unit = {
    chrome.foreach(_.close())
    server.close()
  }

  "openPage's acceptLanguage override" should "reach the server as the request's Accept-Language header" in {
    chrome match {
      case None => cancel("Chrome not installed — skipping CDP accept-language spec")
      case Some(c) =>
        c.openPage(s"${server.baseUrl}/__echo-accept-language", acceptLanguage = Some("de")) { page =>
          page.evalString("document.body.textContent") shouldBe "de"
        }
    }
  }

  it should "carry whatever language string is passed, not just \"de\"" in {
    chrome match {
      case None => cancel("Chrome not installed — skipping CDP accept-language spec")
      case Some(c) =>
        c.openPage(s"${server.baseUrl}/__echo-accept-language", acceptLanguage = Some("es")) { page =>
          page.evalString("document.body.textContent") shouldBe "es"
        }
    }
  }
}
