package controllers

import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore, OauthProfile, OauthProvider}
import services.users.InMemoryUserRepository

import java.nio.charset.StandardCharsets.US_ASCII
import java.security.MessageDigest
import java.util.Base64

/** The native apps' sign-in, end to end, with its PKCE-style verifier: the app
 *  sends `challenge` = base64url(SHA-256(verifier)) to `/auth/:provider/start`,
 *  the deep-link code it gets back carries that challenge, and `/auth/exchange`
 *  spends it only for the matching `verifier`.
 *
 *  THE HOLE IT CLOSES. Anyone could run the native flow in a desktop browser,
 *  read their own code off `kinowo://auth-done?code=…`, and send that link to a
 *  victim: the victim's app redeemed it and was signed into the attacker's
 *  account. With a challenge the code is worthless to any app but the one that
 *  holds the verifier. Codes minted WITHOUT a challenge (released app versions)
 *  still redeem without a verifier, until those versions age out. */
class NativeSignInPkceSpec extends AnyFlatSpec with Matchers {

  private object Google extends OauthProvider {
    val name = "google"
    def authUrl(state: String, redirectUri: String): String = s"https://google.test/?state=$state"
    def exchangeCode(code: String, redirectUri: String): OauthProfile =
      OauthProfile(sub = "G-1", email = Some("alice@example.com"), displayName = Some("Alice"), avatarUrl = None)
  }

  private def challengeOf(verifier: String): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(MessageDigest.getInstance("SHA-256").digest(verifier.getBytes(US_ASCII)))

  private val Verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

  private def controller() =
    new AuthController(Helpers.stubControllerComponents(), Map("google" -> Google), new InMemoryUserRepository,
      new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore), models.Country.Poland)

  /** Start (with `startQuery`) and finish a native flow; the deep-link code. */
  private def deepLinkCode(ctl: AuthController, startQuery: String): String = {
    val start = ctl.start("google")(FakeRequest("GET", s"/auth/google/start?platform=ios$startQuery"))
    status(start) shouldBe SEE_OTHER
    val sess  = session(start)
    val state = sess.get("oauthState").value
    val back  = ctl.callback("google")(FakeRequest("GET", s"/auth/google/callback?code=C&state=$state")
      .withSession(sess.data.toSeq*))
    redirectLocation(back).value.stripPrefix("kinowo://auth-done?code=")
  }

  private def exchange(ctl: AuthController, body: play.api.libs.json.JsObject) =
    status(ctl.exchange()(FakeRequest("POST", "/auth/exchange").withBody(body)))

  "A native sign-in started with a challenge" should "exchange its code for the matching verifier" in {
    val ctl  = controller()
    val code = deepLinkCode(ctl, s"&challenge=${challengeOf(Verifier)}")
    exchange(ctl, Json.obj("code" -> code, "verifier" -> Verifier)) shouldBe OK
  }

  // The login-CSRF case: the victim's app holds no verifier for this code.
  it should "refuse its code with no verifier" in {
    val ctl  = controller()
    val code = deepLinkCode(ctl, s"&challenge=${challengeOf(Verifier)}")
    exchange(ctl, Json.obj("code" -> code)) shouldBe UNAUTHORIZED
  }

  it should "refuse its code with another app's verifier" in {
    val ctl  = controller()
    val code = deepLinkCode(ctl, s"&challenge=${challengeOf(Verifier)}")
    exchange(ctl, Json.obj("code" -> code, "verifier" -> ("x" * 43))) shouldBe UNAUTHORIZED
  }

  // Released apps send no challenge and no verifier: they keep working.
  "A native sign-in started without a challenge" should "exchange its code with no verifier, as released apps do" in {
    val ctl  = controller()
    exchange(ctl, Json.obj("code" -> deepLinkCode(ctl, ""))) shouldBe OK
  }

  // A current app always started its own flow with a challenge, so a
  // challenge-less code reaching it came from somebody else's flow.
  it should "refuse its code when a verifier is presented" in {
    val ctl  = controller()
    exchange(ctl, Json.obj("code" -> deepLinkCode(ctl, ""), "verifier" -> Verifier)) shouldBe UNAUTHORIZED
  }

  // Android's Custom Tab shares Chrome's cookie jar: a native flow abandoned at
  // the provider leaves its keys in the session, and the next start in that
  // browser must not inherit them.
  "/auth/:provider/start" should "not let an abandoned native flow's keys ride into a later web sign-in" in {
    val ctl       = controller()
    val abandoned = session(ctl.start("google")(FakeRequest("GET",
      s"/auth/google/start?platform=android&challenge=${challengeOf(Verifier)}")))
    val web   = ctl.start("google")(FakeRequest("GET", "/auth/google/start").withSession(abandoned.data.toSeq*))
    val sess  = session(web)
    sess.get("mobileClient") shouldBe None
    sess.get(AuthController.MobileChallengeKey) shouldBe None
    val back = ctl.callback("google")(FakeRequest("GET", s"/auth/google/callback?code=C&state=${sess.get("oauthState").value}")
      .withSession(sess.data.toSeq*))
    redirectLocation(back).value should not startWith "kinowo://"
  }

  it should "not stamp an abandoned flow's challenge on a later challenge-less native code" in {
    val ctl       = controller()
    val abandoned = session(ctl.start("google")(FakeRequest("GET",
      s"/auth/google/start?platform=android&challenge=${challengeOf(Verifier)}")))
    session(ctl.start("google")(FakeRequest("GET", "/auth/google/start?platform=android")
      .withSession(abandoned.data.toSeq*))).get(AuthController.MobileChallengeKey) shouldBe None
  }

  it should "refuse a challenge that is not a base64url SHA-256" in {
    val ctl = controller()
    for (bad <- Seq("short", "=" * 43, "a" * 44))
      status(ctl.start("google")(FakeRequest("GET", s"/auth/google/start?platform=android&challenge=$bad"))) shouldBe BAD_REQUEST
  }
}
