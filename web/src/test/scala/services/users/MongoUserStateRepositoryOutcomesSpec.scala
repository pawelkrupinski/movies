package services.users

import controllers.UserStateController
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.User
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.metrics.{LegacyUserStateMetrics, PrometheusExposition, UserStateWriteMetrics}
import services.users.UserStateWriteOutcomes.{Endpoint, Outcome}

import java.time.Instant
import scala.collection.mutable

/** The write outcomes `MongoUserStateRepository` reports for the paths that need
 *  no database: a pod whose users store never came up answers every write with
 *  a 503, and that has to be countable as `unavailable` rather than lost among
 *  Mongo throwing (`store_failure`) — and a write the driver really does throw
 *  on has to be counted as `store_failure`, through the controller's 503, all the
 *  way to the exported counter. The `ok` / `conflict` paths are driven against
 *  real Mongo in `HiddenFilmsConcurrentWritesIntegrationSpec`. */
class MongoUserStateRepositoryOutcomesSpec extends AnyFlatSpec with Matchers {

  private def recording() = {
    val seen = mutable.ListBuffer.empty[(String, String)]
    val outcomes: UserStateWriteOutcomes = (endpoint: String, outcome: String) => seen += (endpoint -> outcome)
    (seen, outcomes)
  }

  "a users store that never came up" should "report each refused write as unavailable, on its own endpoint" in {
    val (seen, outcomes) = recording()
    val store = new MongoUserStateRepository(database = None, writeOutcomes = outcomes)
    val now   = Instant.parse("2026-09-23T12:00:00Z")

    store.changeHiddenFilms("u", "pl", HiddenFilmsChange.Hide("Film", 10), now) shouldBe None
    store.changeHiddenFilms("u", "pl", HiddenFilmsChange.Unhide("Film"), now) shouldBe None
    store.changeHiddenFilms("u", "pl", HiddenFilmsChange.Clear, now) shouldBe None
    store.patchLegacyState("u", LegacyStatePatch(None, None, Some(Some("en"))), now) shouldBe None

    seen.toList shouldBe List(
      Endpoint.Hide      -> Outcome.Unavailable,
      Endpoint.Unhide    -> Outcome.Unavailable,
      Endpoint.Clear     -> Outcome.Unavailable,
      Endpoint.LegacyPut -> Outcome.Unavailable)
  }

  "a write the Mongo driver throws on" should "answer 503 and count as store_failure on the exported counter" in {
    // A real driver and collection whose client is CLOSED: `findOneAndUpdate`
    // throws inside the driver ("state should be: open"), with no server needed.
    val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=500")
    val db     = client.getDatabase("closed")
    client.close()

    val registry = new PrometheusRegistry()
    val clock    = java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)
    val store    = new MongoUserStateRepository(database = Some(db),
      writeOutcomes = new UserStateWriteMetrics(registry, "pl"))
    val users    = new InMemoryUserRepository
    users.upsert(User(id = "u", provider = "google", providerSub = "G-u", email = None, displayName = None,
      avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    val controller = new UserStateController(Helpers.stubControllerComponents(), store,
      new AccountDeletion(users, store), NoUserChangeTimeCache, new LegacyUserStateMetrics(registry, "pl", clock), users, clock)

    val result = controller.hideFilm("pl", "Film")(FakeRequest("PUT", "/api/me/pl/hidden-films/Film").withSession("userId" -> "u"))

    status(result) shouldBe SERVICE_UNAVAILABLE
    val text = PrometheusExposition.render(registry)
    text should include ("""kinowo_web_user_state_writes_total{country="pl",endpoint="hide",outcome="store_failure"} 1""")
    text should include ("""kinowo_web_user_state_writes_total{country="pl",endpoint="hide",outcome="ok"} 0""")
  }

  // Every web pod builds the unique `userId` index on boot, and a build that
  // failed (duplicate rows, a Mongo refusing it) used to vanish into a bare
  // `Try` — leaving `userStates` with no index at all, silently, every boot.
  "a unique-index build that fails" should "be reported as a missing index, not swallowed" in {
    val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=500")
    val db     = client.getDatabase("closed")
    client.close()
    val reported = mutable.ListBuffer.empty[Boolean]
    val store = new MongoUserStateRepository(database = Some(db),
      indexHealth = (present: Boolean) => reported += present)

    store.enabled shouldBe true   // still serves: a missing index is not a reason to refuse every read
    reported.toList shouldBe List(false)
  }
}
