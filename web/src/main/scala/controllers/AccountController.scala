package controllers

import scala.util.{Failure, Success, Try}

import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.*
import services.users.{AccountDeletion, UserRepository}

/** `DELETE /api/me`: the signed-in visitor's account, gone. */
class AccountController(cc: ControllerComponents, accountDeletion: AccountDeletion, userRepository: UserRepository)
  extends AbstractController(cc) with Logging {

  /** Hard-delete the user's row + state row, drop their session.
   *  GDPR-aligned: after this call we hold no row keyed by this user's
   *  id. The browser's localStorage is left alone — it's per-device, the
   *  user can clear it themselves; we don't have a server-side handle to
   *  do it.
   *
   *  Anonymous → 401. Authenticated → delete both rows (via the shared
   *  `AccountDeletion`, same path Facebook's callback uses) + return 204
   *  with the session cleared. The response carries no body so a fetch
   *  call doesn't need a parser. A store that could not delete is a 503
   *  with the session kept: a 204 is the visitor's word that nothing is left. */
  def deleteAccount(): Action[AnyContent] = Action { request =>
    SignedInUser.answering(SignedInUser.idOf(request, userRepository) match {
      case None         => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(userId) =>
        Try(accountDeletion.delete(userId)) match {
          case Success(_) => NoContent.withNewSession
          case Failure(e) =>
            logger.warn(s"Account deletion of $userId failed: ${e.getMessage}")
            ServiceUnavailable(Json.obj("error" -> "account could not be deleted — retry"))
        }
    })
  }
}
