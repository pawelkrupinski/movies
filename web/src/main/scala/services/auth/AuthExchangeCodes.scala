package services.auth

import java.time.{Clock, Duration, Instant}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

/**
 * A one-shot code that stands in for a signed-in `userId` just long enough to
 * carry it somewhere a session cookie cannot go.
 *
 * Two places need that. The native apps finish their OAuth flow in a system
 * browser and come back through a `kinowo://` deep link, which shares no cookie
 * jar with the page that signed in. And Poland is on `kinowo.net` while the
 * other three countries are on `showtimes.cc`: two registrable domains, so no
 * cookie setting in existence spans them — a visitor switching country there has
 * to be handed over explicitly.
 *
 * `issuedAt` rather than an expiry instant so the TTL is one number, owned by
 * [[AuthExchangeCodes]], instead of a deadline each writer computes for itself.
 */
final case class PendingExchangeCode(code: String, userId: String, issuedAt: Instant)

/**
 * The durable boundary under [[AuthExchangeCodes]] — persistence only, no policy.
 *
 * Single-use is the STORE's job because only the store can make "read it and
 * remove it" one atomic step; two browsers redeeming the same code must not both
 * win. Everything else about what a code MEANS — how long it lives, what a fresh
 * one looks like — belongs to `AuthExchangeCodes` and is deliberately not
 * restated in any implementation.
 */
trait AuthExchangeCodeStore {
  /** Record a freshly minted code. Best-effort: a store that cannot write leaves
   *  the visitor to sign in again on the far side, which is the same place a
   *  wrong password lands them. */
  def put(pending: PendingExchangeCode): Unit

  /** Remove `code` and return what it stood for, or `None` when it was never
   *  there — invented, or already redeemed. */
  def remove(code: String): Option[PendingExchangeCode]
}

/**
 * Minting and redeeming the codes. THE RULES LIVE HERE, above the store seam, so
 * the Mongo and in-memory stores cannot drift on them: a fake that quietly kept
 * codes a minute longer than production would let a spec pass over a bug.
 *
 * Two minutes because a code is spent by the redirect that carries it — the
 * budget is one HTTP hop plus a slow phone, not a user's attention span. It is
 * never at rest anywhere a person could come back to.
 */
class AuthExchangeCodes(
  store: AuthExchangeCodeStore,
  clock: Clock = Clock.systemUTC(),
  ttl:   Duration = AuthExchangeCodes.Ttl
) {

  /** A fresh single-use code standing in for `userId`. */
  def mint(userId: String): String = {
    val pending = PendingExchangeCode(UUID.randomUUID().toString, userId, clock.instant())
    store.put(pending)
    pending.code
  }

  /** The `userId` behind `code`, spending it in the process. `None` when the code
   *  is unknown, already spent, or older than the TTL.
   *
   *  An EXPIRED code is still removed rather than left to the store's own
   *  housekeeping: it is spent either way, and leaving it would let a caller keep
   *  retrying a code that can never work again. */
  def redeem(code: String): Option[String] =
    store.remove(code)
      .filter(pending => !pending.issuedAt.plus(ttl).isBefore(clock.instant()))
      .map(_.userId)
}

object AuthExchangeCodes {
  /** How long a minted code stays redeemable. See the class comment for why it
   *  is this short. */
  val Ttl: Duration = Duration.ofMinutes(2)
}

/**
 * Process-local store — what a deployment with no Mongo runs on, and what specs
 * use. Correct for the native-app handoff, which starts and finishes on the same
 * pod; NOT correct for the cross-domain one, where the pod that mints the code is
 * by definition not the pod that redeems it. Wiring picks this only when there is
 * no database to share.
 */
class InMemoryAuthExchangeCodeStore extends AuthExchangeCodeStore {
  private val codes = new ConcurrentHashMap[String, PendingExchangeCode]()

  override def put(pending: PendingExchangeCode): Unit = codes.put(pending.code, pending)

  override def remove(code: String): Option[PendingExchangeCode] = Option(codes.remove(code))

  /** Test/diagnostic view. Not part of the trait: nothing in production may ask a
   *  store how many codes it is holding. */
  def size: Int = codes.size
}
