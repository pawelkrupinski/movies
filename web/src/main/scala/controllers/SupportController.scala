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
class SupportController(cc: ControllerComponents, country: models.Country) extends AbstractController(cc) {

  /** `/support?lang=pl|en|de|es` — see [[PublishedLanguages]] for why the
   *  language comes from the link and how an unknown one falls back. */
  def support(lang: Option[String]): Action[AnyContent] = Action {
    Ok(page(PublishedLanguages.resolve(lang, published, country)))
  }

  /** Every language the site offers — a new language needs its support page
   *  translated before it ships (SupportControllerSpec fails otherwise). */
  private val published = LanguageNames.Codes.toSet

  private def page(language: String): play.twirl.api.Html = language match {
    case "pl" => views.html.supportPl(country)
    case "de" => views.html.supportDe(country)
    case "es" => views.html.supportEs(country)
    // English answers a deployment language outside `LanguageNames.Codes`.
    case _    => views.html.supportEn(country)
  }
}
