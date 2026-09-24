package controllers

import models.User
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.{RequestHeader, Result, Results, Session}
import services.users.UserRepository

import scala.util.control.NonFatal

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
object SignedInUser extends Logging {

  /** Set on a successful sign-in, dropped on logout. The user's row id. */
  val UserIdKey = "userId"

  /** The row's `sessionVersion` as of sign-in — see the class doc above. */
  val SessionVersionKey = "sessionVersion"

  /** The signed-in user, or `None` for an anonymous browser, a session whose
   *  user row has since been deleted, or a session revoked since it was
   *  issued (`sessionVersion` mismatch) — a stale cookie is logged out.
   *
   *  A row that could not be READ is none of those: it throws [[LookupFailed]],
   *  which [[answering]] turns into a 503 to retry. */
  def apply(request: RequestHeader, users: UserRepository): Option[User] =
    request.session.get(UserIdKey)
      .flatMap(id =>
        try users.findById(id)
        catch { case NonFatal(e) => throw LookupFailed(e) })
      .filter(_.sessionVersion == sessionVersionOf(request.session))

  /** The session's user row could not be read — see [[apply]]. */
  final case class LookupFailed(cause: Throwable) extends RuntimeException(cause)

  /** `result`, marked per-user, or a per-user 503 when computing it met a user
   *  row that could not be read: not "signed out" (a 401 the apps act on), and
   *  not the framework's bare 500. Every action that looks the session up
   *  answers through this. */
  def answering(result: => Result): Result =
    PerUserResponse(
      try result
      catch {
        case LookupFailed(e) =>
          logger.warn(s"SignedInUser: user unreadable: ${e.getClass.getSimpleName}: ${e.getMessage}")
          Results.ServiceUnavailable(Json.obj("error" -> "user unavailable"))
      })

  /** `session`, carrying `user`. Callers that mean to discard everything else
   *  pass an empty `Session()`; callers continuing an existing one pass it. */
  def establish(session: Session, user: User): Session =
    session + (UserIdKey -> user.id) + (SessionVersionKey -> user.sessionVersion.toString)

  private def sessionVersionOf(session: Session): Int =
    session.get(SessionVersionKey).flatMap(_.toIntOption).getOrElse(0)
}
