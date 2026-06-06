package controllers

import play.api.mvc._

// Static legal pages (privacy policy, and later the Facebook-required data
// deletion instructions). No dependencies beyond the rendered Twirl views, so
// the content lives entirely in app/views and this controller just serves it.
class LegalController(cc: ControllerComponents) extends AbstractController(cc) {

  def privacy: Action[AnyContent] = Action {
    Ok(views.html.privacy())
  }
}
