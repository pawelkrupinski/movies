package controllers

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore}
import models.User
import services.users.InMemoryUserRepository

import java.time.Instant

/** The two places a one-shot code turns into a session, checked over EVERY
 *  combination of what the code was minted with and what the redeemer holds —
 *  not the handful of cases someone thought to write down.
 *
 *  Each dimension is enumerated by its equivalence classes (absent / this one /
 *  a different one), so the product below IS the whole input space as far as the
 *  rules can tell values apart. A rule that only holds for the cases a spec
 *  happened to name — `redeem(code, sessionBinding)` passing a browser's absent
 *  binding straight through, so "no binding" matched "minted without one" (the
 *  login CSRF fixed in 3aba2f665) — fails one row here.
 *
 *  The spend rule is checked on every row too: a code is gone after its FIRST
 *  attempt, whatever that attempt's outcome, so a refused guess cannot be
 *  followed by the right answer. */
class AuthCodeBindingPropertySpec extends AnyFlatSpec with Matchers {

  private val UserId = "alice@example.com"

  private def fixture(): (AuthController, AuthExchangeCodes) = {
    val users = new InMemoryUserRepository
    users.upsert(User(id = UserId, provider = "google", providerSub = "G-1", email = Some(UserId),
      displayName = None, avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    val codes = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore)
    (new AuthController(Helpers.stubControllerComponents(), Map.empty, users, codes, models.Country.Poland), codes)
  }

  private val Bindings: Seq[Option[String]] = Seq(None, Some("browser-A"), Some("browser-B"))

  private val VerifierA = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
  private val VerifierB = "M25iVXpKU3puUjFaYWg3T1NDTDQtcW1ROUY5YXlwalNoc0hhakxifmZHag"
  // Absent; the S256 of each verifier; and a well-formed challenge nobody holds
  // the verifier of.
  private val Challenges: Seq[Option[String]] =
    Seq(None, Some(AuthExchangeCodes.s256(VerifierA)), Some(AuthExchangeCodes.s256(VerifierB)), Some("x" * 43))
  // Absent; each real verifier; and an empty one (present, answers nothing).
  private val Verifiers: Seq[Option[String]] = Seq(None, Some(VerifierA), Some(VerifierB), Some(""))

  private def signedInAs(result: scala.concurrent.Future[play.api.mvc.Result]): Option[String] =
    session(result).get("userId")

  // ── /auth/sso/finish — the cross-domain handoff ─────────────────────────────

  private def finish(ctl: AuthController, code: String, sessionBinding: Option[String]) =
    ctl.ssoFinish()(FakeRequest("GET", s"/auth/sso/finish?code=$code")
      .withSession(sessionBinding.map(AuthController.SsoBindingKey -> _).toSeq*))

  private val finishCases = Table(("code binding", "session binding", "code challenge"),
    (for { c <- Bindings; s <- Bindings; ch <- Challenges.take(2) } yield (c, s, ch))*)

  "ssoFinish" should "sign in exactly when the code and the session hold the same, non-empty binding" in {
    forAll(finishCases) { (codeBinding, sessionBinding, challenge) =>
      val (ctl, codes) = fixture()
      val code         = codes.mint(UserId, codeBinding, challenge)
      // A handoff code is minted by `ssoStart` for a binding and never with a
      // challenge — a challenged code is a native deep-link code, and a browser
      // has no verifier to offer for it.
      val expected = (codeBinding, sessionBinding) match {
        case (Some(c), Some(s)) => c == s && challenge.isEmpty
        case _                  => false
      }

      withClue(s"code=$codeBinding session=$sessionBinding challenge=$challenge: ") {
        signedInAs(finish(ctl, code, sessionBinding)) shouldBe (if (expected) Some(UserId) else None)
        // Spent by the first attempt: the matching browser cannot come back for it.
        signedInAs(finish(ctl, code, codeBinding)) shouldBe None
      }
    }
  }

  it should "never leave the browser holding its binding, whatever the outcome" in {
    forAll(finishCases) { (codeBinding, sessionBinding, challenge) =>
      val (ctl, codes) = fixture()
      val result       = finish(ctl, codes.mint(UserId, codeBinding, challenge), sessionBinding)
      withClue(s"code=$codeBinding session=$sessionBinding challenge=$challenge: ") {
        session(result).get(AuthController.SsoBindingKey) shouldBe None
      }
    }
  }

  // ── /auth/exchange — the native apps' deep-link code (PKCE, b3431607a) ──────

  /** The rule, stated independently of `AuthExchangeCodes.answers`: both absent
   *  (a released app's code, redeemed by a released app), or the verifier's
   *  S256 IS the challenge. */
  private def verifierAnswers(challenge: Option[String], verifier: Option[String]): Boolean =
    (challenge, verifier) match {
      case (None, None)       => true
      case (Some(c), Some(v)) => AuthExchangeCodes.s256(v) == c
      case _                  => false
    }

  private def exchange(ctl: AuthController, code: String, verifier: Option[String]) =
    status(ctl.exchange()(FakeRequest("POST", "/auth/exchange")
      .withBody(Json.obj("code" -> code) ++ verifier.fold(Json.obj())(v => Json.obj("verifier" -> v)))))

  private val exchangeCases = Table(("code binding", "code challenge", "verifier"),
    (for { b <- Bindings.take(2); c <- Challenges; v <- Verifiers } yield (b, c, v))*)

  "exchange" should "spend a code exactly when it is unbound and the verifier answers its challenge" in {
    forAll(exchangeCases) { (binding, challenge, verifier) =>
      val (ctl, codes) = fixture()
      val code         = codes.mint(UserId, binding, challenge)
      // A bound code is a browser handoff code and is never an app's to spend.
      val expected = binding.isEmpty && verifierAnswers(challenge, verifier)

      withClue(s"binding=$binding challenge=$challenge verifier=$verifier: ") {
        exchange(ctl, code, verifier) shouldBe (if (expected) OK else UNAUTHORIZED)
        exchange(ctl, code, verifier) shouldBe UNAUTHORIZED
      }
    }
  }

  // The whole native flow, not just a hand-minted code: whatever challenge the
  // app starts with is the one its code carries.
  it should "hold across the real start → callback path for every challenge/verifier pair" in {
    val google = new services.auth.OauthProvider {
      val name = "google"
      def authUrl(state: String, redirectUri: String): String = s"https://google.test/?state=$state"
      def exchangeCode(code: String, redirectUri: String) =
        services.auth.OauthProfile(sub = "G-1", email = Some(UserId), displayName = None, avatarUrl = None)
    }
    forAll(Table(("challenge", "verifier"), (for { c <- Challenges; v <- Verifiers } yield (c, v))*)) { (challenge, verifier) =>
      val ctl = new AuthController(Helpers.stubControllerComponents(), Map("google" -> google), new InMemoryUserRepository,
        new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore), models.Country.Poland)
      val start = ctl.start("google")(FakeRequest("GET",
        s"/auth/google/start?platform=ios${challenge.fold("")(c => s"&challenge=$c")}"))
      val sess  = session(start)
      val back  = ctl.callback("google")(FakeRequest("GET", s"/auth/google/callback?code=C&state=${sess.get("oauthState").value}")
        .withSession(sess.data.toSeq*))
      val code  = redirectLocation(back).value.stripPrefix("kinowo://auth-done?code=")
      val expected = verifierAnswers(challenge, verifier)

      withClue(s"challenge=$challenge verifier=$verifier: ") {
        exchange(ctl, code, verifier) shouldBe (if (expected) OK else UNAUTHORIZED)
      }
    }
  }
}
