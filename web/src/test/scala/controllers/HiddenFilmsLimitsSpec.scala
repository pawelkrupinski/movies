package controllers

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.metrics.LegacyUserStateMetrics
import services.users.{AccountDeletion, InMemoryUserRepository, InMemoryUserStateRepository, NoUserChangeTimeCache}

import java.time.Instant

/** The per-country hidden-films writes are bounded: one signed-in account must
 *  not be able to grow its `userStates` row without limit (Mongo refuses a
 *  document past 16 MB, and every read of it gets slower on the way there). */
class HiddenFilmsLimitsSpec extends AnyFlatSpec with Matchers {

  private val clock = java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)

  private def fixture(bucket: Set[String] = Set.empty): (UserStateController, InMemoryUserStateRepository) = {
    val users  = new InMemoryUserRepository
    users.upsert(models.User(id = "u1", provider = "google", providerSub = "G-u1", email = Some("u1@example.com"),
      displayName = None, avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    val states = new InMemoryUserStateRepository
    states.upsert(UserState("u1", Set.empty, Set.empty, Instant.EPOCH, Map("pl" -> bucket)))
    (new UserStateController(Helpers.stubControllerComponents(), states, new AccountDeletion(users, states),
      NoUserChangeTimeCache, new LegacyUserStateMetrics(new PrometheusRegistry(), "pl", clock), users, clock), states)
  }

  private def hide(ctl: UserStateController, title: String) =
    ctl.hideFilm("pl", title)(FakeRequest("PUT", "/api/me/pl/hidden-films/x").withSession("userId" -> "u1"))

  "hiding a film" should "400 a title longer than any real one, and store nothing" in {
    val (ctl, states) = fixture()
    status(hide(ctl, "x" * (UserStateController.MaxTitleLength + 1))) shouldBe BAD_REQUEST
    states.find("u1").value.hiddenFilmsByCountry("pl") shouldBe empty
  }

  it should "accept a title right at the limit" in {
    val (ctl, _) = fixture()
    status(hide(ctl, "x" * UserStateController.MaxTitleLength)) shouldBe OK
  }

  it should "413 a new title once the country's bucket is full, and store nothing" in {
    val full = (1 to UserStateController.MaxHiddenPerCountry).map(i => s"Film $i").toSet
    val (ctl, states) = fixture(full)
    status(hide(ctl, "One Too Many")) shouldBe REQUEST_ENTITY_TOO_LARGE
    states.find("u1").value.hiddenFilmsByCountry("pl") shouldBe full
  }

  it should "still accept re-hiding a title already in a full bucket (idempotent, no growth)" in {
    val full = (1 to UserStateController.MaxHiddenPerCountry).map(i => s"Film $i").toSet
    val (ctl, _) = fixture(full)
    status(hide(ctl, "Film 1")) shouldBe OK
  }

  "unhiding a film" should "always be allowed, even from a full bucket" in {
    val full = (1 to UserStateController.MaxHiddenPerCountry).map(i => s"Film $i").toSet
    val (ctl, states) = fixture(full)
    status(ctl.unhideFilm("pl", "Film 1")(FakeRequest("DELETE", "/api/me/pl/hidden-films/x").withSession("userId" -> "u1"))) shouldBe OK
    states.find("u1").value.hiddenFilmsByCountry("pl") should not contain "Film 1"
  }
}
