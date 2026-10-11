package controllers

import play.api.mvc._

/**
 * Serves `GET /api/picker/:country` — one country's `/` picker fragment, the markup
 * the landing fetches for every country but its own right after it loads (see
 * `landing.scala.html`). Country-AGNOSTIC like `/api/catalog`: every deployment
 * renders the same bytes for every country, so the landing asks its own origin.
 *
 * CACHING. The landing links each fragment by a URL carrying its content version
 * (`?v=`, [[PickerFragments.url]]), so a URL names one exact body and the browser
 * may keep it for good — `immutable`, a year: a visitor's second landing costs no
 * request at all for the other countries, not even a revalidation, and a deploy
 * that changes a roster changes the URL. Only a request whose `v` IS the current
 * version gets that promise. Any other — no `v`, or a page from one deploy asking a
 * pod of the other mid-rollout — is answered `no-cache`, so a mismatched body can
 * never be pinned under the wrong version's URL. Every answer carries the content
 * ETag, so revalidation still costs only headers.
 */
class PickerController(cc: ControllerComponents, fragments: PickerFragments) extends AbstractController(cc) {

  def fragment(country: String, v: Option[String]): Action[AnyContent] = Action { request =>
    fragments.body(country).fold[Result](NotFound) { body =>
      body.serve(request, if (v.contains(body.version)) PickerController.Immutable else FixedBody.Revalidate)
    }
  }
}

object PickerController {
  val Immutable = "public, max-age=31536000, immutable"
}
