package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
 * `Chrome.tryStart`'s `lang` parameter fixes this by launching Chrome with
 * `--lang=<code>`, which is also where Chrome derives the `Accept-Language`
 * header it sends on every request. This spec proves the header actually
 * reaches the server on the wire (`TestHttpServer`'s
 * `/__echo-accept-language` echo route), independent of whether any
 * particular page happens to honour the header — that's `WebLangResolver`'s
 * job, covered elsewhere.
 *
 * (An earlier version of this fix set the header per-navigation via CDP's
 * `Network.enable` + `Network.setExtraHTTPHeaders` instead. That worked
 * locally but made every screenshot fail outright in CI, so the fix moved to
 * this Chrome-launch-flag approach, which never touches the `Network`
 * domain — see `Chrome.tryStart`'s doc comment.)
 */
class CdpAcceptLanguageSpec extends AnyFlatSpec with Matchers {

  private def withServerAndChrome(lang: Option[String])(body: (Chrome, TestHttpServer) => Unit): Unit = {
    val server = new TestHttpServer(routes = PartialFunction.empty)
    try
      Chrome.tryStart(lang = lang) match {
        case None => cancel("Chrome not installed — skipping CDP accept-language spec")
        case Some(chrome) =>
          try body(chrome, server) finally chrome.close()
      }
    finally server.close()
  }

  "Chrome.tryStart's lang parameter" should "make the launched Chrome send that language as Accept-Language" in
    withServerAndChrome(lang = Some("de")) { (chrome, server) =>
      chrome.openPage(s"${server.baseUrl}/__echo-accept-language") { page =>
        page.evalString("document.body.textContent") should startWith("de")
      }
    }

  it should "carry whatever language code is passed, not just \"de\"" in
    withServerAndChrome(lang = Some("es")) { (chrome, server) =>
      chrome.openPage(s"${server.baseUrl}/__echo-accept-language") { page =>
        page.evalString("document.body.textContent") should startWith("es")
      }
    }
}
