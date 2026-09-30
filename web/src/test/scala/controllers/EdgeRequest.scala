package controllers

import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest

/** A request as the TLS-terminating edge forwards it to the app: `https`, for
 *  `host`. `PageMeta.origin` reads these headers, so a spec asserting on an
 *  absolute URL the page emits (canonical, sitemap `<loc>`, a redirect off the
 *  brand domain) sends one of these rather than a bare `FakeRequest`. */
object EdgeRequest {
  def apply(path: String, host: String = "kinowo.net", method: String = "GET"): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(method, path).withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> host)
}
