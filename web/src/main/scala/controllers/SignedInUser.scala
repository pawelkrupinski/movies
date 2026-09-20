package controllers

import models.User
import play.api.mvc.{RequestHeader, Session}
import services.users.UserRepository

import java.time.Instant

/**
 * Who is signed in on this request — the one place that turns the session's
 * `userId` back into a `User`, and the one place that writes a session for a
 * user who has just signed in.
 *
 * It exists because those two halves have to agree about a SECOND key, and four
 * copies of `session.get("userId").flatMap(repository.findById)` cannot.
 *
 * WHY THE SECOND KEY. The showtimes.cc countries share an origin and therefore a
 * session cookie, but they are separate deployments: `/us` is its own pod, and
 * `/auth/…` is answered by the process mounted at the apex. So a sign-in is
 * completed by one pod and every page that follows it is rendered by another —
 * and `CachingUserRepository` holds the user's row for an hour, per process.
 * Signing in with Facebook to an account last seen through Google updated the
 * row, updated the cache on the pod that did the exchange, and left the pod that
 * renders the page serving the Google name and avatar for the rest of the hour.
 *
 * `signedInAt` is the `lastSeenAt` of the row the session was ISSUED against —
 * the only fact a sibling pod can use to tell that its own copy predates a
 * sign-in it never saw. It is not a credential and is not trusted as one: the
 * worst a forged value can do is make the pod re-read a row from Mongo that it
 * would otherwise have taken from memory, and the id it re-reads is still the
 * one the signed cookie names.
 *
 * A session issued before this key existed has no stamp, reads as
 * `Instant.EPOCH`, and keeps the old behaviour until its owner next signs in.
 *
 * `sessionVersion` is the revocation check: the cookie carries a copy of
 * `User.sessionVersion` as of sign-in, and a request is only honoured when
 * that copy still matches the row's CURRENT value. "Sign out everywhere"
 * bumps the row's version, which instantly fails this check for every cookie
 * issued before the bump — on every device, since it's the same shared row
 * every deployment reads. A cookie with no stamp (issued before this key
 * existed) reads as `0`, matching a fresh row's own default, so an existing
 * session keeps working unrevoked until its owner signs out everywhere.
 */
object SignedInUser {

  /** Set on a successful sign-in, dropped on logout. The user's row id. */
  val UserIdKey = "userId"

  /** When the row this session was issued against was last written. */
  val SignedInAtKey = "signedInAt"

  /** The row's `sessionVersion` as of sign-in — see the class doc above. */
  val SessionVersionKey = "sessionVersion"

  /** The signed-in user, or `None` for an anonymous browser, a session whose
   *  user row has since been deleted, or a session revoked since it was
   *  issued (`sessionVersion` mismatch) — a stale cookie is logged out. */
  def apply(request: RequestHeader, users: UserRepository): Option[User] =
    request.session.get(UserIdKey)
      .flatMap(users.findById(_, issuedAgainst(request.session)))
      .filter(_.sessionVersion == sessionVersionOf(request.session))

  /** `session`, carrying `user`. Callers that mean to discard everything else
   *  pass an empty `Session()`; callers continuing an existing one pass it. */
  def establish(session: Session, user: User): Session =
    session + (UserIdKey -> user.id) + (SignedInAtKey -> user.lastSeenAt.toEpochMilli.toString) +
      (SessionVersionKey -> user.sessionVersion.toString)

  private def issuedAgainst(session: Session): Instant =
    session.get(SignedInAtKey).flatMap(_.toLongOption).map(Instant.ofEpochMilli).getOrElse(Instant.EPOCH)

  private def sessionVersionOf(session: Session): Int =
    session.get(SessionVersionKey).flatMap(_.toIntOption).getOrElse(0)
}
