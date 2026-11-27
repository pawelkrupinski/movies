package controllers

import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.*
import services.users.UserRepository

/**
 * Who is signed in, and signing out: `GET /api/me` and `POST /auth/logout`. How an account signs in is
 * [[OauthSignInController]]'s and [[NativeSignInController]]'s; the cross-domain hop sign-out makes through
 * the sibling deployment is [[SignInFlow]]'s, and the far half is [[SsoController.ssoLogout]].
 */
class SessionController(
  cc:             ControllerComponents,
  userRepository: UserRepository
) extends AbstractController(cc) with Logging {

  /** Who is looking — the ONLY place this application names a visitor now.
   *
   *  The HTML pages stopped rendering the avatar so that a shared cache could
   *  hold them (`CachePolicy.RevalidatedAnywhere`), which moved the whole
   *  per-user surface here: `js/shared/` calls this on every page load and builds
   *  the account menu from the answer. So this response, and only this response,
   *  is the one nothing may keep a copy of — a stored 200 is an avatar rebuilt
   *  after a sign-out, or on a device the answer was never for. */
  def me(): Action[AnyContent] = Action { request =>
    SignedInUser.answering(SignedInUser(request, userRepository) match {
      case None => Unauthorized(Json.obj("error" -> "not logged in"))
      case Some(user) => Ok(SignInFlow.userJson(user))
    })
  }

  /** Sign out — EVERYWHERE: every browser, every app, every deployment.
   *
   *  The account's `sessionVersion` is bumped first, which fails the
   *  `SignedInUser` check for every cookie issued before it — the web's on
   *  every device and the native apps' (theirs is the same session cookie,
   *  handed out by `/auth/token`) — on every deployment, since they all read
   *  the one shared `users` row. Only a session that still resolves bumps it:
   *  an anonymous caller, or a cookie an earlier logout already killed, must
   *  not sign the account out of the sessions it has opened since. A store
   *  that cannot be read or written is logged and does not stop THIS cookie
   *  being dropped — logging out never fails here.
   *
   *  The version bump already reaches the other domain's session, but its
   *  cookie still names the account until it is next checked, so the visitor
   *  is sent through that domain's [[SsoController.ssoLogout]] too: the pairing at sign-in
   *  establishes a session on both, and "log out" has to mean the far cookie
   *  is gone, not merely dead. Same one hop, in the other direction. */
  def logout(): Action[AnyContent] = Action { request =>
    revokeEverySession(request)
    val target = SignInFlow.viaSibling(SsoController.SsoLogoutPath, routes.LandingController.index().url, request)
    // `withNewSession` DISCARDS the cookie rather than re-signing a smaller one.
    // Removing the keys would leave a valid session cookie behind holding
    // nothing, which is the same thing to this application and not the same
    // thing at all to a browser, a proxy, or anyone reading the response.
    PerUserResponse.signedOut(Redirect(target).withNewSession)
  }

  private def revokeEverySession(request: RequestHeader): Unit =
    try SignedInUser(request, userRepository).foreach { user =>
      if (userRepository.revokeSessions(user.id).isEmpty)
        logger.warn(s"logout: sessions of ${user.id} could not be revoked; only this cookie is dropped")
    } catch {
      case SignedInUser.LookupFailed(exception) =>
        logger.warn(s"logout: user unreadable, only this cookie is dropped: ${exception.getMessage}")
    }

}
