package controllers

import models.UserState
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.users.UserStateRepo

import java.time.Instant

/**
 * REST endpoint for the authenticated user's personalization state —
 * favourites + hidden films + disabled cinemas. The browser-side JS
 * (Phase D) uses this to:
 *
 *   - On page load when logged in: GET /api/me/state, merge into
 *     localStorage, PUT the merged state back so the next device
 *     sees the union (the "migrate on entry" behaviour the spec asked
 *     for).
 *   - On every toggle (favourite, hide, disable cinema): PUT the full
 *     new state. Replace, not patch — keeps the wire shape trivial
 *     and the conflict story bounded (last-write-wins per user, which
 *     is fine for this scale of UX).
 *
 * Anonymous requests get 401 — JS falls back to localStorage on its
 * own.
 *
 * Shape (both directions):
 *   { "favouriteMovies":     [titles…],
 *     "favouriteScreenings": [screening ids…],
 *     "hiddenFilms":         [titles…],
 *     "disabledCinemas":     [cinema display names…] }
 */
class UserStateController(
  cc:            ControllerComponents,
  userStateRepo: UserStateRepo,
  userRepo:      services.users.UserRepo
) extends AbstractController(cc) with Logging {
  import UserStateController._

  def get(): Action[AnyContent] = Action { request =>
    request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        val state = userStateRepo.find(userId).getOrElse(UserState.empty(userId))
        Ok(toJson(state))
    }
  }

  def put(): Action[JsValue] = Action(parse.json) { request =>
    request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        fromJson(userId, request.body) match {
          case Left(reason) => BadRequest(Json.obj("error" -> reason))
          case Right(state) =>
            userStateRepo.upsert(state)
            Ok(toJson(state))
        }
    }
  }

  /** Hard-delete the user's row + state row, drop their session.
   *  GDPR-aligned: after this call we hold no row keyed by this user's
   *  id. The browser's localStorage is left alone — it's per-device, the
   *  user can clear it themselves; we don't have a server-side handle to
   *  do it.
   *
   *  Anonymous → 401. Authenticated → delete both rows + return 204
   *  with the session cleared. The response carries no body so a fetch
   *  call doesn't need a parser. */
  def deleteAccount(): Action[AnyContent] = Action { request =>
    request.session.get("userId") match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        logger.info(s"Account deletion requested for $userId")
        userStateRepo.delete(userId)
        userRepo.delete(userId)
        NoContent.withNewSession
    }
  }
}

object UserStateController {

  /** Render a `UserState` to its wire JSON. Sorted lists at the wire
   *  edge so the response is deterministic (helps caching and makes
   *  spec assertions stable); the in-memory model stays a Set.
   */
  def toJson(state: UserState): JsValue = Json.obj(
    "favouriteMovies"     -> state.favouriteMovies.toSeq.sorted,
    "favouriteScreenings" -> state.favouriteScreenings.toSeq.sorted,
    "hiddenFilms"         -> state.hiddenFilms.toSeq.sorted,
    "disabledCinemas"     -> state.disabledCinemas.toSeq.sorted
  )

  /** Parse a wire JSON into `UserState` bound to `userId`. Missing
   *  arrays default to empty (lets the client send just the field
   *  that changed without re-shipping everything). Wrong shape
   *  (non-array value, non-string element) returns Left with a hint.
   */
  def fromJson(userId: String, body: JsValue): Either[String, UserState] = {
    def stringSet(field: String): Either[String, Set[String]] =
      (body \ field).toOption match {
        case None                      => Right(Set.empty)
        case Some(jsArr) =>
          jsArr.asOpt[Seq[String]] match {
            case Some(seq) => Right(seq.toSet)
            case None      => Left(s"$field must be an array of strings")
          }
      }
    for {
      fm <- stringSet("favouriteMovies")
      fs <- stringSet("favouriteScreenings")
      hf <- stringSet("hiddenFilms")
      dc <- stringSet("disabledCinemas")
    } yield UserState(userId, fm, fs, hf, dc, Instant.now())
  }
}
