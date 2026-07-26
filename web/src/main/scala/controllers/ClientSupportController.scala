package controllers

import models.ClientSupport
import play.api.mvc._

/**
 * Serves `GET /api/client-support` — the minimum client version each mobile
 * platform still gets service for, and where to send anyone below it (see
 * [[models.ClientSupport]]).
 *
 * Deliberately the same shape as [[CatalogController]]: static per deploy, so the
 * body carries a content `ETag` and honours `If-None-Match`, and a client whose
 * cached answer is current pays only headers. The apps call this on launch
 * alongside `/api/catalog`, so making it a 304 in the common case matters.
 *
 * Country-AGNOSTIC — every deployment serves identical bytes — so a client can ask
 * whichever host it is already pointed at rather than needing to know which
 * deployment owns the rule.
 */
class ClientSupportController(cc: ControllerComponents) extends AbstractController(cc) {

  def clientSupport(): Action[AnyContent] = Action { request =>
    val validators = Seq("ETag" -> ClientSupport.etag, "Cache-Control" -> "no-cache")
    if (request.headers.get("If-None-Match").contains(ClientSupport.etag))
      NotModified.withHeaders(validators*)
    else
      Ok(ClientSupport.json).as("application/json").withHeaders(validators*)
  }
}
