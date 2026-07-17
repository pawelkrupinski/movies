package controllers

import models.Catalog
import play.api.mvc._

/**
 * Serves `GET /api/catalog` — the global country + city catalog the mobile apps
 * fetch on each open (see [[models.Catalog]]). The payload is static per deploy,
 * so the response carries a content ETag and honours `If-None-Match`: a client
 * whose bundled/cached catalog is already current gets a `304 Not Modified` with
 * no body, so the common (unchanged) case transfers only headers.
 *
 * Country-AGNOSTIC — every deployment serves the same bytes — so the apps can
 * fetch it from whichever country host they're pointed at. Uses `ETag`/
 * `If-None-Match` rather than the site's usual `Last-Modified` because the
 * catalog has no meaningful timestamp; its identity IS its content hash.
 */
class CatalogController(cc: ControllerComponents) extends AbstractController(cc) {

  def catalog(): Action[AnyContent] = Action { request =>
    val validators = Seq("ETag" -> Catalog.etag, "Cache-Control" -> "no-cache")
    if (request.headers.get("If-None-Match").contains(Catalog.etag))
      NotModified.withHeaders(validators*)
    else
      Ok(Catalog.json).as("application/json").withHeaders(validators*)
  }
}
