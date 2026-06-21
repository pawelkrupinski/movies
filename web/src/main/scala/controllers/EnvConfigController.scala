package controllers

import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services.config.{EnvConfigRow, EnvConfigService, TimeKnobFormat}

/**
 * Admin page at `/admin/config` to view and flip the non-secret config knobs
 * both apps expose through [[tools.Env]]. The page renders a static shell and
 * polls `/admin/config/data`; Set/Reset are JSON POSTs that write the Mongo
 * `env_overrides` collection, which both processes consult live — so a flip
 * takes effect mid-flight (within the config refresh tick) without a restart.
 *
 * Gated by [[AdminAction]] (login session + ADMIN_ALLOWLIST), exactly like the
 * title-rules editor. Secrets never reach here — the service excludes them at
 * publish, merge, and write — so there's nothing sensitive to render.
 */
class EnvConfigController(cc: ControllerComponents, adminAction: AdminAction, config: EnvConfigService)
    extends AbstractController(cc) {

  def index: Action[AnyContent] = adminAction { Ok(views.html.admin.envConfig()) }

  def data: Action[AnyContent] = adminAction {
    Ok(Json.obj("rows" -> config.rows().map(rowJson)))
  }

  /** Apply an override: `{ "key": …, "value": … }`. 400 when the key is unknown
   *  or secret, or the value doesn't parse for the knob's numeric type. */
  def set: Action[JsObject] = adminAction(parse.json[JsObject]) { request =>
    val key   = (request.body \ "key").asOpt[String].getOrElse("")
    val value = (request.body \ "value").asOpt[String].getOrElse("")
    if (config.set(key, value)) Ok(Json.obj("ok" -> true))
    else BadRequest(Json.obj("error" -> "rejected: unknown/secret key or invalid value for its type"))
  }

  /** Remove an override ("reset to default / env"): `{ "key": … }`. */
  def reset: Action[JsObject] = adminAction(parse.json[JsObject]) { request =>
    val key = (request.body \ "key").asOpt[String].getOrElse("")
    if (config.reset(key)) Ok(Json.obj("ok" -> true))
    else BadRequest(Json.obj("error" -> "rejected: unknown/secret key"))
  }

  private def rowJson(r: EnvConfigRow): JsObject = Json.obj(
    "key"           -> r.key,
    "kind"          -> r.kind.toString,
    "default"       -> r.default,
    "defaultHuman"  -> r.default.flatMap(TimeKnobFormat.humanize(r.key, _)),
    "override"      -> r.overrideValue,
    "overrideHuman" -> r.overrideValue.flatMap(TimeKnobFormat.humanize(r.key, _)),
    "apps"          -> r.apps.map(a => Json.obj(
      "app"          -> a.app,
      "current"      -> a.current,
      "currentHuman" -> a.current.flatMap(TimeKnobFormat.humanize(r.key, _))))
  )
}
