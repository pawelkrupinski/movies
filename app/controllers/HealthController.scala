package controllers

import play.api.libs.json.Json
import play.api.mvc._

class HealthController(cc: ControllerComponents) extends AbstractController(cc) {

  def check: Action[AnyContent] = Action {
    Ok(Json.obj("status" -> "ok", "service" -> "scala-play-app"))
  }
}
