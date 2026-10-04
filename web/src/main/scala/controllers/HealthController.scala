package controllers

import play.api.libs.json.Json
import play.api.mvc._

/** The kubelet's two questions, answered separately because they want opposite things.
 *
 *  `check` (`/health`, startup + liveness): is the process up? Never waits on data — a pod whose
 *  Mongo is slow is not a pod to restart.
 *
 *  `ready` (`/ready`, readiness): may it take traffic? Only once `readiness` holds — the read
 *  model has completed a whole read. Booting into a Mongo blip, the hydrate read fails and the
 *  model serves every city empty until its cold retry lands; reporting ready meanwhile let a
 *  rolling deploy retire the warm pod for the empty one. */
class HealthController(cc: ControllerComponents, readiness: () => Boolean) extends AbstractController(cc) {

  def check: Action[AnyContent] = Action {
    Ok(Json.obj("status" -> "ok", "service" -> "scala-play-app"))
  }

  def ready: Action[AnyContent] = Action {
    if (readiness()) Ok(Json.obj("status" -> "ready"))
    else ServiceUnavailable(Json.obj("status" -> "hydrating"))
  }
}
