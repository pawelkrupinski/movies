package controllers

import io.prometheus.metrics.model.registry.PrometheusRegistry
import modules.webwiring.UsersWiring
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Session
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.auth.{AuthExchangeCodes, InMemoryAuthExchangeCodeStore}
import services.metrics.LegacyUserStateMetrics
import services.users.{AccountDeletion, InMemoryUserRepository, InMemoryUserStateRepository, NoUserChangeTimeCache}

import java.time.Instant

/**
 * One person, several web processes, one shared users database.
 *
 * Every country is its own pod (`/uk`, `/de`, `/us`, `/es` share showtimes.cc
 * and therefore the session cookie; the apex pod answers `/auth/…` for all of
 * them), and a rolling deploy runs two of the same country side by side. They
 * share nothing but Mongo and the cookie — so whatever each pod puts between
 * its controllers and that database (`UsersWiring.podUserRepository` /
 * `podUserStateRepository`) must not let one pod act on a copy another pod has
 * already changed. Each test below is a real cross-pod sequence that a
 * per-process cache got wrong.
 */
class UserAcrossPodsSpec extends AnyFlatSpec with Matchers {

  private val Now = Instant.parse("2026-09-20T12:00:00Z")

  private def alice(sessionVersion: Int = 0): models.User = models.User(
    id = "alice@example.com", provider = "google", providerSub = "G-1",
    email = Some("alice@example.com"), displayName = Some("Alice"), avatarUrl = None,
    createdAt = Now.minusSeconds(86400), lastSeenAt = Now.minusSeconds(3600),
    sessionVersion = sessionVersion)

  /** The shared database, and a way to stand up another pod over it. */
  private class Fleet {
    val users  = new InMemoryUserRepository
    val states = new InMemoryUserStateRepository
    users.upsert(alice())
    // She already has a state row — a pod can only hold a copy of one that exists.
    states.upsert(models.UserState("alice@example.com", Set.empty, Set.empty, Now.minusSeconds(60)))

    def statePod(): UserStateController = {
      val podUsers  = UsersWiring.podUserRepository(users)
      val podStates = UsersWiring.podUserStateRepository(states)
      new UserStateController(Helpers.stubControllerComponents(), podStates,
        new AccountDeletion(podUsers, podStates), NoUserChangeTimeCache,
        new LegacyUserStateMetrics(new PrometheusRegistry(), "pl", java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)), podUsers)
    }

    def authPod(): AuthController =
      new AuthController(Helpers.stubControllerComponents(), Map.empty, UsersWiring.podUserRepository(users),
        new AuthExchangeCodes(new InMemoryAuthExchangeCodeStore), models.Country.Poland)
  }

  private val aliceSession: Session = SignedInUser.establish(Session(), alice())

  private def as[A](session: Session)(request: FakeRequest[A]): FakeRequest[A] =
    request.withSession(session.data.toSeq*)

  "a hidden-films write on one pod" should "survive a later write to another country on a different pod" in {
    val fleet = new Fleet
    val uk    = fleet.statePod()
    val de    = fleet.statePod()

    // Alice has a /uk tab open — that pod has read her row.
    status(uk.hiddenFilms("uk")(as(aliceSession)(FakeRequest("GET", "/api/me/uk/hidden-films")))) shouldBe OK
    // …then hides a film on /de, answered by the /de pod.
    status(de.hideFilm("de", "Der Film")(as(aliceSession)(FakeRequest("PUT", "/api/me/de/hidden-films/Der%20Film")))) shouldBe OK
    // …then hides one back on /uk. The /uk pod must not write back the row as
    // it last saw it, which would silently un-hide "Der Film".
    status(uk.hideFilm("uk", "The Film")(as(aliceSession)(FakeRequest("PUT", "/api/me/uk/hidden-films/The%20Film")))) shouldBe OK

    val stored = fleet.states.find("alice@example.com").value.hiddenFilmsByCountry
    stored.get("de") shouldBe Some(Set("Der Film"))
    stored.get("uk") shouldBe Some(Set("The Film"))
  }

  "a hidden-films read on one pod" should "see a write another pod just made" in {
    val fleet = new Fleet
    val a     = fleet.statePod()
    val b     = fleet.statePod()

    a.hiddenFilms("pl")(as(aliceSession)(FakeRequest("GET", "/api/me/pl/hidden-films")))
    b.hideFilm("pl", "Film")(as(aliceSession)(FakeRequest("PUT", "/api/me/pl/hidden-films/Film")))

    val fresh = a.hiddenFilms("pl")(as(aliceSession)(FakeRequest("GET", "/api/me/pl/hidden-films")))
    (contentAsJson(fresh) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("Film")
  }

  "sign out everywhere" should "reject the revoked cookie on a pod that had already seen it" in {
    val fleet   = new Fleet
    val apex    = fleet.authPod()
    val sibling = fleet.authPod()
    val laptop  = aliceSession
    val phone   = aliceSession

    // The laptop's cookie is in use on the sibling pod…
    status(sibling.me()(as(laptop)(FakeRequest("GET", "/api/me")))) shouldBe OK
    // …when Alice, on her phone, signs out every other session.
    status(apex.revokeSessions()(as(phone)(FakeRequest("POST", "/auth/sessions/revoke")))) shouldBe OK

    status(sibling.me()(as(laptop)(FakeRequest("GET", "/api/me")))) shouldBe UNAUTHORIZED
  }

  it should "keep the device that asked signed in on every pod" in {
    val fleet   = new Fleet
    val apex    = fleet.authPod()
    val sibling = fleet.authPod()

    status(sibling.me()(as(aliceSession)(FakeRequest("GET", "/api/me")))) shouldBe OK
    val revoke = apex.revokeSessions()(as(aliceSession)(FakeRequest("POST", "/auth/sessions/revoke")))
    val reissued = session(revoke)

    status(sibling.me()(as(reissued)(FakeRequest("GET", "/api/me")))) shouldBe OK
  }
}
