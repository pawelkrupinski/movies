package controllers

import models.{User, UserState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.FacebookSignedRequestFixture
import services.users.{AccountDeletion, InMemoryUserRepository, InMemoryUserStateRepository}

import java.time.Instant

class FacebookDataDeletionControllerSpec extends AnyFlatSpec with Matchers {

  private val Secret = "test-app-secret"

  private def fixture(appSecret: Option[String] = Some(Secret))
    : (FacebookDataDeletionController, InMemoryUserRepository, InMemoryUserStateRepository) = {
    val userRepository  = new InMemoryUserRepository
    val stateRepository = new InMemoryUserStateRepository
    val ctl = new FacebookDataDeletionController(
      Helpers.stubControllerComponents(),
      appSecret,
      userRepository,
      new AccountDeletion(userRepository, stateRepository)
    )
    (ctl, userRepository, stateRepository)
  }

  private def seedFacebookUser(userRepository: InMemoryUserRepository, stateRepository: InMemoryUserStateRepository, fbId: String): Unit = {
    userRepository.upsert(User(
      id = "alice@example.com", provider = "facebook", providerSub = fbId,
      email = Some("alice@example.com"), displayName = Some("Alice"), avatarUrl = None,
      createdAt = Instant.now(), lastSeenAt = Instant.now()
    ))
    stateRepository.upsert(UserState("alice@example.com", Set("Conclave"), Set.empty, Instant.now()))
  }

  private def callbackRequest(signedRequest: String) =
    FakeRequest("POST", "/facebook/data-deletion").withFormUrlEncodedBody("signed_request" -> signedRequest)

  "POST /facebook/data-deletion" should "delete the matching local account and return the JSON receipt" in {
    val (ctl, userRepository, stateRepository) = fixture()
    seedFacebookUser(userRepository, stateRepository, "fb-777")

    val result = ctl.callback()(callbackRequest(FacebookSignedRequestFixture.forUser(Secret, "fb-777")))

    status(result) shouldBe OK
    val js = contentAsJson(result)
    (js \ "confirmation_code").as[String] shouldBe "fb-777"
    (js \ "url").as[String]               should include ("/facebook/data-deletion/status?code=fb-777")

    userRepository.findById("alice@example.com")  shouldBe empty
    stateRepository.find("alice@example.com")     shouldBe empty
  }

  it should "still 200 (no-op) when no local account matches the Facebook id" in {
    val (ctl, _, _) = fixture()
    val result = ctl.callback()(callbackRequest(FacebookSignedRequestFixture.forUser(Secret, "fb-unknown")))
    status(result) shouldBe OK
    (contentAsJson(result) \ "confirmation_code").as[String] shouldBe "fb-unknown"
  }

  it should "leave other users untouched" in {
    val (ctl, userRepository, stateRepository) = fixture()
    seedFacebookUser(userRepository, stateRepository, "fb-777")
    userRepository.upsert(User(
      id = "bob@example.com", provider = "google", providerSub = "G-2",
      email = Some("bob@example.com"), displayName = Some("Bob"), avatarUrl = None,
      createdAt = Instant.now(), lastSeenAt = Instant.now()
    ))

    ctl.callback()(callbackRequest(FacebookSignedRequestFixture.forUser(Secret, "fb-777")))

    userRepository.findById("bob@example.com") should not be empty
  }

  it should "400 a request with no signed_request" in {
    val (ctl, _, _) = fixture()
    val result = ctl.callback()(FakeRequest("POST", "/facebook/data-deletion").withFormUrlEncodedBody())
    status(result) shouldBe BAD_REQUEST
  }

  it should "400 a request whose signature was made with the wrong secret" in {
    val (ctl, userRepository, stateRepository) = fixture()
    seedFacebookUser(userRepository, stateRepository, "fb-777")

    val result = ctl.callback()(callbackRequest(FacebookSignedRequestFixture.forUser("wrong-secret", "fb-777")))

    status(result) shouldBe BAD_REQUEST
    // The forged request must NOT have deleted anything.
    userRepository.findById("alice@example.com") should not be empty
  }

  it should "503 when FACEBOOK_APP_SECRET is not configured" in {
    val (ctl, _, _) = fixture(appSecret = None)
    val result = ctl.callback()(callbackRequest("anything.atall"))
    status(result) shouldBe SERVICE_UNAVAILABLE
  }

  "GET /facebook/data-deletion/status" should "render a Polish confirmation page echoing the code" in {
    val (ctl, _, _) = fixture()
    val result = ctl.status("fb-777")(FakeRequest("GET", "/facebook/data-deletion/status?code=fb-777"))
    status(result)      shouldBe OK
    contentType(result) shouldBe Some("text/html")
    val body = contentAsString(result)
    body should include ("""<html lang="pl">""")
    body should include ("fb-777")
    body should include ("zostały trwale usunięte")
  }
}
