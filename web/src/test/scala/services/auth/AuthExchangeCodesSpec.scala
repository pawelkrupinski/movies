package services.auth

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Duration, Instant, ZoneOffset}

/**
 * The rules a one-shot sign-in code obeys, asserted once, above the store seam.
 *
 * They live in `AuthExchangeCodes` rather than in each store precisely so this
 * spec can be the only place they are stated: a Mongo store that quietly kept
 * codes a minute longer than the in-memory one would let every spec here pass
 * while production handed out credentials that outlived their window.
 */
class AuthExchangeCodesSpec extends AnyFlatSpec with Matchers {

  private val Now = Instant.parse("2026-08-30T12:00:00Z")

  /** A clock the test moves by hand, so the TTL is asserted rather than waited out. */
  private class MovableClock(var now: Instant) extends Clock {
    def getZone: java.time.ZoneId                        = ZoneOffset.UTC
    override def withZone(zone: java.time.ZoneId): Clock = this
    def instant(): Instant                               = now
  }

  private def fixture(clock: Clock = Clock.fixed(Now, ZoneOffset.UTC)) =
    new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, clock)

  "A minted code" should "redeem to the user it was minted for" in {
    val codes = fixture()
    codes.redeem(codes.mint("alice@example.com")).value shouldBe "alice@example.com"
  }

  it should "be spent by the first redemption" in {
    val codes = fixture()
    val code  = codes.mint("alice@example.com")

    codes.redeem(code).value shouldBe "alice@example.com"
    codes.redeem(code)       shouldBe empty
  }

  // Two mints must never collide — one visitor's handoff cannot be redeemable
  // by another's code.
  it should "be unique per mint, even for the same user" in {
    val codes = fixture()
    val first  = codes.mint("alice@example.com")
    val second = codes.mint("alice@example.com")

    first should not be second
    codes.redeem(first).value  shouldBe "alice@example.com"
    codes.redeem(second).value shouldBe "alice@example.com"
  }

  "A code nobody minted" should "redeem to nothing" in {
    fixture().redeem("not-a-real-code") shouldBe empty
  }

  // ── The window ───────────────────────────────────────────────────────────
  // A code is spent by the redirect that carries it, so the budget is one HTTP
  // hop and a slow phone — not a browser tab someone comes back to tomorrow.

  "A code inside the TTL" should "still redeem at the last moment" in {
    val clock = new MovableClock(Now)
    val codes = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, clock)
    val code  = codes.mint("alice@example.com")

    clock.now = Now.plus(AuthExchangeCodes.Ttl)
    codes.redeem(code).value shouldBe "alice@example.com"
  }

  "A code past the TTL" should "redeem to nothing" in {
    val clock = new MovableClock(Now)
    val codes = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, clock)
    val code  = codes.mint("alice@example.com")

    clock.now = Now.plus(AuthExchangeCodes.Ttl).plusSeconds(1)
    codes.redeem(code) shouldBe empty
  }

  // Removed, not merely refused: an expired code is spent either way, and
  // leaving it behind would let a caller keep retrying something that can never
  // work again — and would let Mongo's TTL sweep be the only thing that ever
  // cleared it.
  it should "be consumed by the failed redemption rather than left in the store" in {
    val clock = new MovableClock(Now)
    val store = new InMemoryAuthExchangeCodeStore
    val codes = new AuthExchangeCodes(store, clock)
    val code  = codes.mint("alice@example.com")
    store.size shouldBe 1

    clock.now = Now.plus(AuthExchangeCodes.Ttl).plusSeconds(1)
    codes.redeem(code) shouldBe empty
    store.size shouldBe 0

    // And winding the clock back cannot resurrect it.
    clock.now = Now
    codes.redeem(code) shouldBe empty
  }

  "The default TTL" should "be short enough to be a handoff rather than a session" in {
    AuthExchangeCodes.Ttl should be <= Duration.ofMinutes(5)
  }
}
