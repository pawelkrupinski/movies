package controllers

import play.api.mvc._
import services.users.UserRepo

import scala.concurrent.{ExecutionContext, Future}

/** Session + email-allowlist gate for the app's operational/admin endpoints —
 *  the title-rules editor, `/uptime`, `/tasks`, and the `/…/debug/rehydrate`
 *  trigger. The Play session carries our internal user UUID (set by
 *  `AuthController.callback`); this resolves it and requires the user's email be
 *  on the `ADMIN_ALLOWLIST`. 401 when not logged in, 403 when logged in but not
 *  an admin — the contract `AdminTitleRulesController` used to enforce inline,
 *  now shared so every gated controller behaves identically.
 *
 *  Use as an `ActionBuilder`: `adminAction { Ok(...) }` for a no-body action, or
 *  `adminAction(parse.json) { request => ... }` when the action reads a body. */
class AdminAction(
  override val parser: BodyParser[AnyContent],
  userRepo:            UserRepo,
  adminAllowlist:      Set[String]
)(implicit val executionContext: ExecutionContext)
    extends ActionBuilder[Request, AnyContent] {

  override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] =
    request.session.get("userId") match {
      case None      => Future.successful(Results.Unauthorized("Not logged in."))
      case Some(uid) =>
        userRepo.findById(uid).filter(_.email.exists(adminAllowlist.contains)) match {
          case Some(_) => block(request)
          case None    => Future.successful(Results.Forbidden("Not an admin."))
        }
    }
}
