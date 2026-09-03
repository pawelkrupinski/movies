package controllers

import play.api.mvc._

// Static legal pages: the privacy policy and the Facebook-required data
// deletion instructions. No dependencies beyond the rendered Twirl views, so
// the content lives entirely in the templates and this controller just picks
// one.
class LegalController(cc: ControllerComponents) extends AbstractController(cc) {

  /** `/privacy-policy?lang=pl|en|de` — the language comes from the LINK, not
   *  from the deployment; see [[PublishedLanguages]] for why, and for how an
   *  unknown or absent `lang` falls back instead of 404ing.
   */
  def privacy(lang: Option[String]): Action[AnyContent] = Action {
    Ok(policy(PublishedLanguages.resolve(lang, published)))
  }

  /** The policy used to live at `/polityka-prywatnosci`, a URL that is
   *  registered in Meta's app dashboard and already sits in links in the wild.
   *  Redirect it rather than keeping two paths for one document. */
  def privacyLegacyPath: Action[AnyContent] = Action {
    MovedPermanently(routes.LegalController.privacy(None).url)
  }

  private val published = Set("pl", "en", "de")

  private def policy(language: String): play.twirl.api.Html = language match {
    case "pl" => views.html.privacyPl()
    case "de" => views.html.privacyDe()
    // English doubles as the fallback for a deployment whose language we don't
    // publish a policy in yet.
    case _    => views.html.privacyEn()
  }
}
