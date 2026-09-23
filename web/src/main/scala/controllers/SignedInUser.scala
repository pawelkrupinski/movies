package controllers

import models.User
import play.api.mvc.{RequestHeader, Session}
import services.users.UserRepository

/**
 * Who is signed in on this request — the one place that turns the session's
 * `userId` back into a `User`, and the one place that writes a session for a
 * user who has just signed in.
 *
 * It exists because those two halves have to agree about a SECOND key,
 * `sessionVersion`, and four copies of
 * `session.get("userId").flatMap(repository.findById)` cannot.
 *
 * `sessionVersion` is the revocation check: the cookie carries a copy of
 * `User.sessionVersion` as of sign-in, and a request is only honoured when
 * that copy still matches the row's CURRENT value. "Sign out everywhere"
 * bumps the row's version, which instantly fails this check for every cookie
 * issued before the bump — on every device, since it's the same shared row
 * every deployment reads. A cookie with no stamp (issued before this key
 * existed) reads as `0`, matching a fresh row's own default, so an existing
 * session keeps working unrevoked until its owner signs out everywhere.
 *
 * The check is only as current as the row it reads, which is why the
 * `UserRepository` a pod hands this is the shared store itself, uncached (see
 * `UsersWiring.podUserRepository`): a per-process copy of the row kept a
 * revoked cookie working on every OTHER pod, and signed the revoking device
 * out of them, for as long as the copy lived.
 */
object SignedInUser {

  /** Set on a successful sign-in, dropped on logout. The user's row id. */
  val UserIdKey = "userId"

  /** The row's `sessionVersion` as of sign-in — see the class doc above. */
  val SessionVersionKey = "sessionVersion"

  /** The signed-in user, or `None` for an anonymous browser, a session whose
   *  user row has since been deleted, or a session revoked since it was
   *  issued (`sessionVersion` mismatch) — a stale cookie is logged out. */
  def apply(request: RequestHeader, users: UserRepository): Option[User] =
    request.session.get(UserIdKey)
      .flatMap(users.findById)
      .filter(_.sessionVersion == sessionVersionOf(request.session))

  /** `session`, carrying `user`. Callers that mean to discard everything else
   *  pass an empty `Session()`; callers continuing an existing one pass it. */
  def establish(session: Session, user: User): Session =
    session + (UserIdKey -> user.id) + (SessionVersionKey -> user.sessionVersion.toString)

  private def sessionVersionOf(session: Session): Int =
    session.get(SessionVersionKey).flatMap(_.toIntOption).getOrElse(0)
}
