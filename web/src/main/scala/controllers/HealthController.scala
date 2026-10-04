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
 *  rolling deploy retire the warm pod for the empty one.
 *
 *  Both also answer for the process's `databases` ([[services.DatabaseBinding]]): booted into an
 *  unreachable Mongo, every repository holds no database, so the pod is not ready however its read
 *  model looks (it read "nothing" as an empty corpus) — and once that Mongo is back, nothing wired
 *  at boot can use it, so the pod is not alive either: the liveness probe restarts it onto it. */
class HealthController(cc: ControllerComponents, readiness: () => Boolean,
                       databases: Seq[services.DatabaseBinding] = Nil) extends AbstractController(cc) {

  def check: Action[AnyContent] = Action {
    if (services.DatabaseBinding.anyRestartRequired(databases)) ServiceUnavailable(Json.obj("status" -> "restart-required"))
    else Ok(Json.obj("status" -> "ok", "service" -> "scala-play-app"))
  }

  def ready: Action[AnyContent] = Action {
    if (!services.DatabaseBinding.allBound(databases)) ServiceUnavailable(Json.obj("status" -> "database-unreachable-at-boot"))
    else if (readiness()) Ok(Json.obj("status" -> "ready"))
    else ServiceUnavailable(Json.obj("status" -> "hydrating"))
  }
}
