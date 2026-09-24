package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Results
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore}
import services.users.FailingReadUserRepository

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * A session whose user row could not be READ is neither signed in nor signed out: every
 * endpoint that looks the session up answers a per-user 503 to retry — never the 401 the
 * apps act on as "signed out", nor the framework's bare 500 without the per-user
 * Cache-Control (the state endpoints' version of this is in UserStateControllerSpec).
 */
class UnreadableSignedInUserSpec extends AnyFlatSpec with Matchers {

  private val clock = Clock.fixed(Instant.parse("2026-09-24T10:00:00Z"), ZoneOffset.UTC)
  private val users = new FailingReadUserRepository
  private val auth  = new AuthController(Helpers.stubControllerComponents(), Map.empty, users,
    new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore, clock), Country.Poland, clock = clock)
  private def signedIn(method: String, path: String) =
    FakeRequest(method, path).withSession(SignedInUser.UserIdKey -> "alice@example.com")

  "every session lookup" should "answer a per-user 503 when the user cannot be read" in {
    Seq(
      "me"             -> auth.me()(signedIn("GET", "/api/me")),
      "revokeSessions" -> auth.revokeSessions()(signedIn("POST", "/api/me/sessions/revoke")),
      "ssoStart"       -> auth.ssoStart()(signedIn("GET", s"/auth/sso/start?to=${Country.UnitedKingdom.webUrl.get}")),
      "admin"          -> new AdminAction(Helpers.stubBodyParser(), users, Set("alice@example.com"))
                            .async(_ => Future.successful(Results.Ok("admin")))(signedIn("GET", "/admin"))
    ).foreach { case (name, result) =>
      withClue(s"$name: ") {
        status(result) shouldBe SERVICE_UNAVAILABLE
        header("Cache-Control", result) shouldBe Some(PerUserResponse.CacheControl)
      }
    }
  }
}
