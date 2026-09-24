package services.auth

import java.nio.charset.StandardCharsets.US_ASCII
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
 * other four countries are on `showtimes.cc`: two registrable domains, so no
 * cookie setting in existence spans them — a visitor switching country there has
 * to be handed over explicitly.
 *
 * `issuedAt` rather than an expiry instant so the TTL is one number, owned by
 * [[AuthExchangeCodes]], instead of a deadline each writer computes for itself.
 *
 * `binding` ties a cross-domain handoff code to the ONE browser meant to spend
 * it: a random value that browser's own session on the receiving domain holds
 * (see `AuthController.ssoFinish`). Without it a code is a bearer token for
 * whoever follows the link, and a signed-in attacker can mint one for their own
 * account and send it to a victim. The deep-link codes the native apps redeem
 * carry none — the app itself received the deep link.
 *
 * `challenge` does the same for a deep-link code: base64url(SHA-256) of a
 * verifier the app that STARTED the flow generated and kept, so the code is
 * spendable only by the app presenting that verifier (see
 * [[AuthExchangeCodes.redeem]]). Without it anyone can read their own code off a
 * desktop run of the native flow and send the `kinowo://auth-done` link to a
 * victim's app. `None` for app versions released before it existed.
 */
final case class PendingExchangeCode(
  code:      String,
  userId:    String,
  issuedAt:  Instant,
  binding:   Option[String] = None,
  challenge: Option[String] = None
)

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

  /** A fresh single-use code standing in for `userId`, spendable only by a
   *  redeemer presenting the same `binding`, and — when minted with a
   *  `challenge` — the verifier behind it. */
  def mint(userId: String, binding: Option[String] = None, challenge: Option[String] = None): String = {
    val pending = PendingExchangeCode(UUID.randomUUID().toString, userId, clock.instant(), binding, challenge)
    store.put(pending)
    pending.code
  }

  /** The `userId` behind `code`, spending it in the process. `None` when the code
   *  is unknown, already spent, older than the TTL, or was minted for a different
   *  `binding` — a bound code is worthless to anyone but its browser, and an
   *  unbound one is not spendable as a bound one either — or whose `challenge`
   *  the `verifier` does not answer (see [[AuthExchangeCodes.answers]]).
   *
   *  An EXPIRED code is still removed rather than left to the store's own
   *  housekeeping: it is spent either way, and leaving it would let a caller keep
   *  retrying a code that can never work again. */
  def redeem(code: String, binding: Option[String] = None, verifier: Option[String] = None): Option[String] =
    store.remove(code)
      .filter(pending => !pending.issuedAt.plus(ttl).isBefore(clock.instant()))
      .filter(_.binding == binding)
      .filter(pending => AuthExchangeCodes.answers(pending.challenge, verifier))
      .map(_.userId)
}

object AuthExchangeCodes {
  /** How long a minted code stays redeemable. See the class comment for why it
   *  is this short. */
  val Ttl: Duration = Duration.ofMinutes(2)

  /** What a `challenge` must look like: base64url, unpadded, of a 32-byte
   *  SHA-256 — RFC 7636's S256, the only method accepted. */
  val ChallengePattern: scala.util.matching.Regex = "[A-Za-z0-9_-]{43}".r

  /** Whether `verifier` answers `challenge`. Both absent: a code minted for a
   *  released app, redeemed by one. Both present: S256(verifier) must equal the
   *  challenge (compared in constant time). One without the other never: a
   *  challenged code with no verifier is somebody else's code in this app, and
   *  a verifier with no challenge means this app started a flow that is not
   *  the one this code came from. */
  def answers(challenge: Option[String], verifier: Option[String]): Boolean = (challenge, verifier) match {
    case (None, None)          => true
    case (Some(c), Some(v))    => java.security.MessageDigest.isEqual(s256(v).getBytes(US_ASCII), c.getBytes(US_ASCII))
    case _                     => false
  }

  /** base64url(SHA-256(`verifier`)), unpadded — the challenge a verifier stands for. */
  def s256(verifier: String): String =
    java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
      java.security.MessageDigest.getInstance("SHA-256").digest(verifier.getBytes(US_ASCII)))
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
