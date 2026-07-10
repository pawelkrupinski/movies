package controllers

import play.api.mvc._

// Static legal pages (privacy policy, and later the Facebook-required data
// deletion instructions). No dependencies beyond the rendered Twirl views, so
// the content lives entirely in app/views and this controller just serves it.
class LegalController(cc: ControllerComponents) extends AbstractController(cc) {

  // Poland renders the full Polish policy; other countries get the English stub
  // (the full legal prose is only translated for Polish — see `privacyEn`).
  def privacy: Action[AnyContent] = Action {
    if (models.Country.fromEnv.language.getLanguage == "pl") Ok(views.html.privacy())
    else Ok(views.html.privacyEn())
  }
}
