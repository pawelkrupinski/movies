package controllers

import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.users.{InMemoryUserRepo, InMemoryUserStateRepo}

import java.time.Instant

class UserStateControllerSpec extends AnyFlatSpec with Matchers {

  private def fixture(prefilled: Option[UserState] = None): (UserStateController, InMemoryUserStateRepo, InMemoryUserRepo) = {
    val stateRepo = new InMemoryUserStateRepo
    val userRepo  = new InMemoryUserRepo
    prefilled.foreach(stateRepo.upsert)
    (new UserStateController(Helpers.stubControllerComponents(), stateRepo, userRepo), stateRepo, userRepo)
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
      "favouriteMovies"     -> Json.arr(),
      "favouriteScreenings" -> Json.arr(),
      "hiddenFilms"         -> Json.arr(),
      "disabledCinemas"     -> Json.arr()
    )
  }

  it should "return the stored state sorted (deterministic wire format)" in {
    val stored = UserState(
      userId              = "u1",
      favouriteMovies     = Set("Dune", "Conclave"),
      favouriteScreenings = Set("Conclave|Multikino|2026-05-20T18:00"),
      hiddenFilms         = Set("Madagaskar"),
      disabledCinemas     = Set("Kino Apollo"),
      updatedAt           = Instant.parse("2026-05-19T12:00:00Z")
    )
    val (ctl, _, _) = fixture(Some(stored))
    val request  = FakeRequest("GET", "/api/me/state").withSession("userId" -> "u1")
    val result   = ctl.get()(request)

    status(result) shouldBe OK
    val js = contentAsJson(result)
    (js \ "favouriteMovies").as[Seq[String]] shouldBe Seq("Conclave", "Dune")   // alpha-sorted
    (js \ "hiddenFilms").as[Seq[String]]     shouldBe Seq("Madagaskar")
  }

  // ── PUT /api/me/state ─────────────────────────────────────────────────────

  "PUT /api/me/state" should "401 anonymous requests without writing anything" in {
    val (ctl, repo, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withBody(Json.obj("favouriteMovies" -> Json.arr("X")))
      .withHeaders("Content-Type" -> "application/json")
    val result = ctl.put()(request)
    status(result)              shouldBe UNAUTHORIZED
    repo.find("anyone")         shouldBe empty
  }

  it should "replace the user's state with the request body" in {
    val initial = UserState("u1", Set("OLD"), Set.empty, Set.empty, Set.empty, Instant.now())
    val (ctl, repo, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj(
        "favouriteMovies" -> Json.arr("Conclave", "Dune"),
        "hiddenFilms"     -> Json.arr("Hidden A")
      ))

    val result = ctl.put()(request)
    status(result) shouldBe OK

    val stored = repo.find("u1").value
    stored.favouriteMovies shouldBe Set("Conclave", "Dune")    // OLD is gone — replace, not merge
    stored.hiddenFilms     shouldBe Set("Hidden A")
    // Fields omitted from the body default to empty (lets the client
    // ship a focused payload when only one field changed).
    stored.favouriteScreenings shouldBe empty
    stored.disabledCinemas     shouldBe empty
  }

  it should "echo the saved state in the response so the client confirms what landed" in {
    val (ctl, _, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("favouriteMovies" -> Json.arr("A")))
    val result = ctl.put()(request)

    (contentAsJson(result) \ "favouriteMovies").as[Seq[String]] shouldBe Seq("A")
  }

  it should "400 a malformed payload (wrong type) and not touch storage" in {
    val (ctl, repo, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("favouriteMovies" -> "not-an-array"))
    val result = ctl.put()(request)
    status(result)               shouldBe BAD_REQUEST
    (contentAsJson(result) \ "error").as[String] should include ("favouriteMovies")
    repo.find("u1")              shouldBe empty
  }

  // ── DELETE /api/me ──────────────────────────────────────────────────────

  "DELETE /api/me" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    status(ctl.deleteAccount()(FakeRequest("DELETE", "/api/me"))) shouldBe UNAUTHORIZED
  }

  it should "remove the user + state rows AND clear the session" in {
    val initialState = UserState("u1", Set("Conclave"), Set.empty, Set.empty, Set.empty, Instant.now())
    val (ctl, stateRepo, userRepo) = fixture(Some(initialState))
    userRepo.upsert(models.User(
      id = "u1", provider = "google", providerSub = "G-1",
      email = Some("a@x"), displayName = Some("Alice"), avatarUrl = None,
      createdAt = Instant.now(), lastSeenAt = Instant.now()
    ))

    val request = FakeRequest("DELETE", "/api/me").withSession("userId" -> "u1", "extra" -> "leftover")
    val result  = ctl.deleteAccount()(request)

    status(result)               shouldBe NO_CONTENT
    stateRepo.find("u1")         shouldBe empty   // state row gone
    userRepo.findById("u1")      shouldBe empty   // user row gone
    val sess = session(result)
    sess.get("userId")           shouldBe empty   // session cleared
    sess.get("extra")            shouldBe empty   // entire session dropped (.withNewSession), not just userId
  }

  // ── Pure helpers (also covered indirectly by the action specs above) ────

  "UserStateController.fromJson" should "treat missing fields as empty sets" in {
    UserStateController.fromJson("u1", Json.obj()) match {
      case Right(s) =>
        s.favouriteMovies     shouldBe empty
        s.favouriteScreenings shouldBe empty
      case Left(reason) => fail(s"expected Right, got Left($reason)")
    }
  }
}
