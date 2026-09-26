package controllers

import play.api.mvc._
import services.users.UserRepository

import scala.concurrent.{ExecutionContext, Future}

/** Session + email-allowlist gate for the app's operational/admin endpoints —
 *  the title-rules editor, `/uptime`, `/tasks`, and the `/…/debug/rehydrate`
 *  trigger. The Play session carries our internal user UUID (set by
 *  `AuthController.callback`); this resolves it and requires the user's email be
 *  on the `ADMIN_ALLOWLIST`. 401 when not logged in, 403 when logged in but not
 *  an admin —
 *  now shared so every admin controller behaves identically.
 *
 *  Use as an `ActionBuilder`: `adminAction { Ok(...) }` for a no-body action, or
 *  `adminAction(parse.json) { request => ... }` when the action reads a body. */
class AdminAction(
  override val parser: BodyParser[AnyContent],
  userRepository:            UserRepository,
  adminAllowlist:      settings.AdminAllowlist
)(implicit val executionContext: ExecutionContext)
    extends ActionBuilder[Request, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] =
    if (request.session.get(SignedInUser.UserIdKey).isEmpty)
      Future.successful(Results.Unauthorized("Not logged in."))
    else
      scala.util.Try(SignedInUser(request, userRepository)) match {
        case scala.util.Success(user) if user.exists(_.email.exists(adminAllowlist.value.contains)) => block(request)
        case scala.util.Success(_) => Future.successful(Results.Forbidden("Not an admin."))
        // Unreadable, not "not an admin": answered as every session lookup is.
        case scala.util.Failure(e: SignedInUser.LookupFailed) => Future.successful(SignedInUser.answering(throw e))
        case scala.util.Failure(e) => Future.failed(e)
      }
}
