package controllers

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

import play.api.mvc.*
import services.users.UserLookup

/** Session + email-allowlist gate for the app's operational/admin endpoints —
 *  the title-rules editor, `/uptime`, `/tasks`, and the `/…/debug/rehydrate`
 *  trigger, so every admin controller behaves identically. The session's user
 *  ([[SignedInUser]]) must have an email on the `ADMIN_ALLOWLIST`: 401 when
 *  nobody is signed in — no session, or one that no longer resolves (its row
 *  deleted, or revoked by a sign-out everywhere) — and 403 when somebody is but
 *  is not an admin.
 *
 *  Use as an `ActionBuilder`: `adminAction { Ok(...) }` for a no-body action, or
 *  `adminAction(parse.json) { request => ... }` when the action reads a body. */
class AdminAction(
  override val parser: BodyParser[AnyContent],
  userLookup:          UserLookup,
  adminAllowlist:      settings.AdminAllowlist
)(implicit val executionContext: ExecutionContext)
    extends ActionBuilder[Request, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] =
    Try(SignedInUser(request, userLookup)) match {
      case Success(None)                                                         => Future.successful(Results.Unauthorized("Not logged in."))
      case Success(Some(user)) if user.email.exists(adminAllowlist.value.contains) => block(request)
      case Success(Some(_))                                                      => Future.successful(Results.Forbidden("Not an admin."))
      // Unreadable, not "not an admin": answered as every session lookup is.
      case Failure(e: SignedInUser.LookupFailed)                                 => Future.successful(SignedInUser.answering(throw e))
      case Failure(e)                                                            => Future.failed(e)
    }
}
