package controllers

import play.api.mvc._

// Static legal pages: the privacy policy and the Facebook-required data
// deletion instructions. No dependencies beyond the rendered Twirl views, so
// the content lives entirely in the templates and this controller just picks
// one.
class LegalController(cc: ControllerComponents, country: models.Country) extends AbstractController(cc) {

  /** `/privacy-policy?lang=pl|en|de|es` — the language comes from the LINK, not
   *  from the deployment; see [[PublishedLanguages]] for why, and for how an
   *  unknown or absent `lang` falls back instead of 404ing.
   */
  def privacy(lang: Option[String]): Action[AnyContent] = Action {
    Ok(policy(PublishedLanguages.resolve(lang, published.toSet, country)))
  }

  /** Every language the site offers, in the navbar picker's order — a new
   *  language needs its policy translation before it ships (LegalControllerSpec
   *  fails otherwise). */
  private val published = LanguageNames.Codes

  private def policy(language: String): play.twirl.api.Html = language match {
    case "pl" => views.html.privacyPl(country, published)
    case "de" => views.html.privacyDe(country, published)
    case "es" => views.html.privacyEs(country, published)
    // English answers a deployment language outside `LanguageNames.Codes`.
    case _    => views.html.privacyEn(country, published)
  }
}
