package controllers

import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.users.{AccountDeletion, InMemoryUserRepository, InMemoryUserStateRepository}

import java.time.Instant

class UserStateControllerSpec extends AnyFlatSpec with Matchers {

  private def fixture(prefilled: Option[UserState] = None): (UserStateController, InMemoryUserStateRepository, InMemoryUserRepository) = {
    val stateRepository = new InMemoryUserStateRepository
    val userRepository  = new InMemoryUserRepository
    prefilled.foreach(stateRepository.upsert)
    val accountDeletion = new AccountDeletion(userRepository, stateRepository)
    (new UserStateController(Helpers.stubControllerComponents(), stateRepository, accountDeletion), stateRepository, userRepository)
  }

  // ── GET /api/me/state ─────────────────────────────────────────────────────

  "GET /api/me/state" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    val result   = ctl.get()(FakeRequest("GET", "/api/me/state"))
    status(result) shouldBe UNAUTHORIZED
  }

  it should "return an empty state for a user with no stored row" in {
    val (ctl, _, _) = fixture()
    val request  = FakeRequest("GET", "/api/me/state").withSession("userId" -> "newbie")
    val result   = ctl.get()(request)
    status(result)              shouldBe OK
    contentAsJson(result)       shouldBe Json.obj(
      "hiddenFilms"         -> Json.arr(),
      "disabledCinemas"     -> Json.arr()
    )
  }

  it should "return the stored state sorted (deterministic wire format)" in {
    val stored = UserState(
      userId          = "u1",
      hiddenFilms     = Set("Madagaskar", "ABC"),
      disabledCinemas = Set("Kino Apollo"),
      updatedAt       = Instant.parse("2026-05-19T12:00:00Z")
    )
    val (ctl, _, _) = fixture(Some(stored))
    val request  = FakeRequest("GET", "/api/me/state").withSession("userId" -> "u1")
    val result   = ctl.get()(request)

    status(result) shouldBe OK
    val js = contentAsJson(result)
    (js \ "hiddenFilms").as[Seq[String]]     shouldBe Seq("ABC", "Madagaskar")
  }

  // ── PUT /api/me/state ─────────────────────────────────────────────────────

  "PUT /api/me/state" should "401 anonymous requests without writing anything" in {
    val (ctl, repository, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("X")))
      .withHeaders("Content-Type" -> "application/json")
    val result = ctl.put()(request)
    status(result)              shouldBe UNAUTHORIZED
    repository.find("anyone")         shouldBe empty
  }

  it should "replace the user's state with the request body" in {
    val initial = UserState("u1", Set("OLD"), Set.empty, Instant.now())
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj(
        "hiddenFilms"     -> Json.arr("Hidden A")
      ))

    val result = ctl.put()(request)
    status(result) shouldBe OK

    val stored = repository.find("u1").value
    stored.hiddenFilms     shouldBe Set("Hidden A")
    stored.disabledCinemas shouldBe empty
  }

  // THE RULE OUTLIVES THE FIELDS IT WAS WRITTEN FOR. This covered a client that sent only the
  // sets it modelled while the web carried two more (`selectedMovies` / `favouriteRooms`, retired
  // with the plan page). Both remaining fields are now modelled by every client, so the omission
  // is constructed rather than incidental — but the rule is the contract, and the next field added
  // on one platform before the other depends on it.
  it should "preserve a field the body omits, rather than clearing it" in {
    val initial = UserState(
      userId          = "u1",
      hiddenFilms     = Set("OLD HIDE"),
      disabledCinemas = Set("OLD CINEMA"),
      updatedAt       = Instant.now()
    )
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("New Hide")))

    status(ctl.put()(request)) shouldBe OK
    val stored = repository.find("u1").value
    stored.hiddenFilms     shouldBe Set("New Hide")    // present → replaced
    stored.disabledCinemas shouldBe Set("OLD CINEMA")  // absent  → preserved
  }

  it should "still clear a field when the body sends it as an explicit empty array" in {
    val initial = UserState("u1", Set("H"), Set("C"), Instant.now())
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("disabledCinemas" -> Json.arr()))
    status(ctl.put()(request)) shouldBe OK
    repository.find("u1").value.disabledCinemas shouldBe empty    // present-but-empty → cleared
    repository.find("u1").value.hiddenFilms     shouldBe Set("H") // absent → preserved
  }

  it should "echo the saved state in the response so the client confirms what landed" in {
    val (ctl, _, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("A")))
    val result = ctl.put()(request)

    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("A")
  }

  it should "400 a malformed payload (wrong type) and not touch storage" in {
    val (ctl, repository, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> "not-an-array"))
    val result = ctl.put()(request)
    status(result)               shouldBe BAD_REQUEST
    (contentAsJson(result) \ "error").as[String] should include ("hiddenFilms")
    repository.find("u1")              shouldBe empty
  }

  // ── DELETE /api/me ──────────────────────────────────────────────────────

  "DELETE /api/me" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    status(ctl.deleteAccount()(FakeRequest("DELETE", "/api/me"))) shouldBe UNAUTHORIZED
  }

  it should "remove the user + state rows AND clear the session" in {
    val initialState = UserState("u1", Set("Conclave"), Set.empty, Instant.now())
    val (ctl, stateRepository, userRepository) = fixture(Some(initialState))
    userRepository.upsert(models.User(
      id = "u1", provider = "google", providerSub = "G-1",
      email = Some("a@x"), displayName = Some("Alice"), avatarUrl = None,
      createdAt = Instant.now(), lastSeenAt = Instant.now()
    ))

    val request = FakeRequest("DELETE", "/api/me").withSession("userId" -> "u1", "extra" -> "leftover")
    val result  = ctl.deleteAccount()(request)

    status(result)               shouldBe NO_CONTENT
    stateRepository.find("u1")         shouldBe empty
    userRepository.findById("u1")      shouldBe empty
    val sess = session(result)
    sess.get("userId")           shouldBe empty
    sess.get("extra")            shouldBe empty
  }

  // ── Pure helpers (also covered indirectly by the action specs above) ────

  "UserStateController.fromJson" should "keep base fields the body omits and overwrite the ones it sends" in {
    val base = UserState("u1", Set("H"), Set("D"), Instant.now())
    UserStateController.fromJson(base, Json.obj("hiddenFilms" -> Json.arr("H2"))) match {
      case Right(s) =>
        s.hiddenFilms     shouldBe Set("H2")  // present → overwritten
        s.disabledCinemas shouldBe Set("D")   // absent  → preserved
      case Left(reason) => fail(s"expected Right, got Left($reason)")
    }
  }
}
