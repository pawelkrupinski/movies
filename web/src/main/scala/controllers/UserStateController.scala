package controllers

import models.UserState
import play.api.libs.json.{JsNull, JsValue, Json}
import play.api.mvc._
import services.users.{AccountDeletion, UserStateRepository}

import java.time.Instant

/**
 * REST endpoint for the authenticated user's personalization state —
 * hidden films, disabled cinemas, and language. The browser-side JS uses
 * this to sync localStorage with the server on login; the iOS/Android apps
 * sync their own local store the same way.
 *
 * Shape (both directions):
 *   { "hiddenFilms":     [titles…],
 *     "disabledCinemas": [cinema display names…],
 *     "language":        "pl" | "en" | "de" | "es" | null }
 *
 * `language` is a single explicit pick, not a set like the other two
 * fields, so it carries no union semantics: a client either overwrites it
 * with a pick of its own, or leaves it out of the body to keep whatever is
 * stored (same partial-update rule as the sets — see `fromJson`).
 */
class UserStateController(
  cc:              ControllerComponents,
  userStateRepository:   UserStateRepository,
  accountDeletion: AccountDeletion
) extends AbstractController(cc) {
  import UserStateController._

  // Every action here answers about ONE person, so every answer says so — see
  // `PerUserResponse`. Since the HTML pages stopped carrying a signed-in visitor
  // at all, these endpoints and `/api/me` are the ENTIRE per-user surface, and a
  // cached copy of one is the whole privacy failure the split was meant to end.
  def get(): Action[AnyContent] = Action { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        val state = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
        Ok(toJson(state))
    })
  }

  def put(): Action[JsValue] = Action(parse.json) { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        // PUT is a partial update over the stored row (see fromJson): fields
        // the body omits keep their stored value, so a client that only
        // models some of the sets can't wipe the others.
        val base = userStateRepository.find(userId).getOrElse(UserState.empty(userId))
        fromJson(base, request.body) match {
          case Left(reason) => BadRequest(Json.obj("error" -> reason))
          case Right(state) =>
            userStateRepository.upsert(state)
            Ok(toJson(state))
        }
    })
  }

  /** Hard-delete the user's row + state row, drop their session.
   *  GDPR-aligned: after this call we hold no row keyed by this user's
   *  id. The browser's localStorage is left alone — it's per-device, the
   *  user can clear it themselves; we don't have a server-side handle to
   *  do it.
   *
   *  Anonymous → 401. Authenticated → delete both rows (via the shared
   *  `AccountDeletion`, same path Facebook's callback uses) + return 204
   *  with the session cleared. The response carries no body so a fetch
   *  call doesn't need a parser. */
  def deleteAccount(): Action[AnyContent] = Action { request =>
    PerUserResponse(request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        accountDeletion.delete(userId)
        NoContent.withNewSession
    })
  }
}

object UserStateController {

  /** Render a `UserState` to its wire JSON. Sorted lists at the wire
   *  edge so the response is deterministic (helps caching and makes
   *  spec assertions stable); the in-memory model stays a Set.
   */
  def toJson(state: UserState): JsValue = Json.obj(
    "hiddenFilms"     -> state.hiddenFilms.toSeq.sorted,
    "disabledCinemas" -> state.disabledCinemas.toSeq.sorted,
    "language"        -> state.language
  )

  /** Parse a wire JSON into `UserState` as a PARTIAL update over `base`: a
   *  field present in the body overwrites that set, a field the body omits
   *  keeps `base`'s value (and a present-but-empty array clears it). This
   *  lets a client send only the fields it owns without re-shipping, and
   *  without wiping the ones it doesn't model — the rule that mattered when
   *  the web carried two fields the mobile apps did not, and that stays
   *  because the next such field should not have to rediscover it. Wrong
   *  shape (non-array value, non-string element) returns Left with a hint.
   */
  def fromJson(base: UserState, body: JsValue): Either[String, UserState] = {
    def stringSet(field: String, fallback: Set[String]): Either[String, Set[String]] =
      (body \ field).toOption match {
        case None                      => Right(fallback)
        case Some(jsArray) =>
          jsArray.asOpt[Seq[String]] match {
            case Some(seq) => Right(seq.toSet)
            case None      => Left(s"$field must be an array of strings")
          }
      }
    // Present and a known code → overwrite; present and `null` → clear
    // (a client that wants to give up its pick sends this, though none do
    // today); absent → keep `base`'s value, same rule as the sets above.
    def language(fallback: Option[String]): Either[String, Option[String]] =
      (body \ "language").toOption match {
        case None            => Right(fallback)
        case Some(JsNull)    => Right(None)
        case Some(jsValue) =>
          jsValue.asOpt[String] match {
            case Some(code) if LanguageNames.Codes.contains(code) => Right(Some(code))
            case Some(code) => Left(s"language must be one of ${LanguageNames.Codes.mkString(", ")}, got $code")
            case None       => Left("language must be a string")
          }
      }
    for {
      hf   <- stringSet("hiddenFilms",     base.hiddenFilms)
      dc   <- stringSet("disabledCinemas", base.disabledCinemas)
      lang <- language(base.language)
    } yield UserState(base.userId, hf, dc, Instant.now(), lang)
  }
}
