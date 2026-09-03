package controllers

import play.api.mvc._

/** The support page every app-store listing has to register a URL for. Apple
 *  checks it during review and rejects a support URL that only shows the
 *  product — it wants a page where a user can actually get help — so this is a
 *  real question-and-answer page with a contact address, not a redirect to the
 *  listings.
 *
 *  Same shape as [[LegalController]]: static content per language, chosen by
 *  the link rather than by the deployment.
 */
class SupportController(cc: ControllerComponents) extends AbstractController(cc) {

  /** `/support?lang=pl|en|de|es` — see [[PublishedLanguages]] for why the
   *  language comes from the link and how an unknown one falls back. */
  def support(lang: Option[String]): Action[AnyContent] = Action {
    Ok(page(PublishedLanguages.resolve(lang, published)))
  }

  /** Spanish is published here even though the privacy policy has no Spanish
   *  translation yet: Spain is a live App Store territory, and the support URL
   *  is the one Apple actually reads. */
  private val published = Set("pl", "en", "de", "es")

  private def page(language: String): play.twirl.api.Html = language match {
    case "pl" => views.html.supportPl()
    case "de" => views.html.supportDe()
    case "es" => views.html.supportEs()
    // English doubles as the fallback for a deployment whose language we don't
    // publish a support page in yet.
    case _    => views.html.supportEn()
  }
}
