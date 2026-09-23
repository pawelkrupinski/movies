package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore, OauthProfile, OauthProvider}
import services.users.InMemoryUserRepository

import java.time.Instant

/** Every auth response that names a person or hands out / withholds a session
 *  is `PerUserResponse` (`private, no-store`) — including the ones that were
 *  missed: `/auth/token` and `/auth/exchange` (JSON naming the user plus a
 *  `Set-Cookie`), and `ssoFinish`'s signed-out landing for a dead code. */
class AuthNoStoreSpec extends AnyFlatSpec with Matchers {

  private object Google extends OauthProvider {
    val name = "google"
    def authUrl(state: String, redirectUri: String): String = "https://google.test/"
    def exchangeCode(code: String, redirectUri: String): OauthProfile =
      OauthProfile(sub = "G-1", email = Some("alice@example.com"), displayName = Some("Alice"), avatarUrl = None)
  }

  private val users = new InMemoryUserRepository
  private val codes = new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore)
  private val ctl   = new AuthController(Helpers.stubControllerComponents(), Map("google" -> Google), users, codes,
    models.Country.Poland)

  "POST /auth/token" should "forbid keeping a copy of a successful sign-in" in {
    val result = ctl.token()(FakeRequest("POST", "/auth/token")
      .withBody(Json.obj("provider" -> "google", "token" -> "code", "redirectUri" -> "https://x/cb")))
    status(result) shouldBe OK
    header("Cache-Control", result) shouldBe Some(PerUserResponse.CacheControl)
  }

  it should "forbid keeping a copy of a refusal too" in {
    val result = ctl.token()(FakeRequest("POST", "/auth/token").withBody(Json.obj("provider" -> "apple", "token" -> "t")))
    header("Cache-Control", result) shouldBe Some(PerUserResponse.CacheControl)
  }

  "POST /auth/exchange" should "forbid keeping a copy of the session it hands out" in {
    users.upsert(models.User(id = "bob@example.com", provider = "google", providerSub = "G-2", email = Some("bob@example.com"),
      displayName = Some("Bob"), avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    val result = ctl.exchange()(FakeRequest("POST", "/auth/exchange").withBody(Json.obj("code" -> codes.mint("bob@example.com"))))
    status(result) shouldBe OK
    header("Cache-Control", result) shouldBe Some(PerUserResponse.CacheControl)
  }

  it should "forbid keeping a copy of a refused code" in {
    val result = ctl.exchange()(FakeRequest("POST", "/auth/exchange").withBody(Json.obj("code" -> "nope")))
    status(result) shouldBe UNAUTHORIZED
    header("Cache-Control", result) shouldBe Some(PerUserResponse.CacheControl)
  }

  "GET /auth/sso/finish" should "forbid keeping a copy of the signed-out landing for a dead code" in {
    val result = ctl.ssoFinish()(FakeRequest("GET", "/auth/sso/finish?code=dead"))
    status(result) shouldBe SEE_OTHER
    header("Cache-Control", result) shouldBe Some(PerUserResponse.CacheControl)
  }
}
