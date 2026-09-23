package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore}
import services.users.InMemoryUserRepository

/** `POST /auth/token`'s refusal names no internals: which providers are wired,
 *  what a validator's HTTP call returned, or a stack's message. The detail is
 *  for the server log; the client only needs "that did not sign you in". */
class AuthTokenErrorSpec extends AnyFlatSpec with Matchers {

  private val ctl = new AuthController(Helpers.stubControllerComponents(), Map.empty, new InMemoryUserRepository,
    new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore), models.Country.Poland)

  private def token(provider: String) =
    ctl.token()(FakeRequest("POST", "/auth/token").withBody(Json.obj("provider" -> provider, "token" -> "t")))

  "a refused token" should "401 with a generic message, not the exception's" in {
    val result = token("google") // no validator wired → "Google not configured" internally
    status(result) shouldBe UNAUTHORIZED
    (contentAsJson(result) \ "error").as[String] shouldBe AuthController.TokenRejected
    contentAsString(result) should not include "configured"
  }

  it should "say the same for an unknown provider" in {
    (contentAsJson(token("myspace")) \ "error").as[String] shouldBe AuthController.TokenRejected
  }
}
