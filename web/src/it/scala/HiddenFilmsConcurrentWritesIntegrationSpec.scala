package integration

import controllers.UserStateController
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.User
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.metrics.LegacyUserStateMetrics
import services.users.{AccountDeletion, InMemoryUserRepository, MongoUserStateRepository, NoUserChangeTimeCache}
import tools.Env

import java.time.Instant
import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/** The per-country hidden-films writes against REAL Mongo, many requests for one
 *  user at once — the case a second tab, the app and a quick double-tap produce.
 *  What matters is what the server does with overlapping writes, so the store
 *  here is `MongoUserStateRepository`, not the in-memory one. */
class HiddenFilmsConcurrentWritesIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val Prefix = "__integration-test-hide-"
  private val states = new MongoUserStateRepository()
  private val users  = new InMemoryUserRepository
  private val pool   = Executors.newFixedThreadPool(32)
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  override protected def afterAll(): Unit = try {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    Await.ready(client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo")).getCollection("userStates")
      .deleteMany(Filters.regex("userId", s"^$Prefix")).toFuture(), 10.seconds)
    client.close()
    states.close()
    pool.shutdown()
  } finally super.afterAll()

  private val controller = new UserStateController(Helpers.stubControllerComponents(), states,
    new AccountDeletion(users, states), NoUserChangeTimeCache, new LegacyUserStateMetrics(new PrometheusRegistry(), "pl"), users)

  private def signedIn(suffix: String): String = {
    val id = s"$Prefix$suffix"
    users.upsert(User(id = id, provider = "google", providerSub = s"G-$id", email = None, displayName = None,
      avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    id
  }

  private def hide(userId: String, country: String, title: String) =
    controller.hideFilm(country, title)(FakeRequest("PUT", s"/api/me/$country/hidden-films/x").withSession("userId" -> userId))

  "parallel hides of distinct titles for one user" should "all succeed and all persist — starting from no row at all" in {
    val userId = signedIn("parallel")
    val titles = (1 to 40).map(i => s"Film $i")

    val statuses = Await.result(Future.traverse(titles)(t => Future(status(hide(userId, "pl", t)))), 60.seconds)

    statuses.distinct shouldBe Seq(OK)
    states.find(userId).value.hiddenFilmsByCountry("pl") shouldBe titles.toSet
  }

  they should "not disturb another country's bucket being written at the same time" in {
    val userId = signedIn("two-countries")
    val writes = (1 to 20).flatMap(i => Seq("pl" -> s"PL $i", "de" -> s"DE $i"))

    Await.result(Future.traverse(writes) { case (c, t) => Future(status(hide(userId, c, t))) }, 60.seconds).distinct shouldBe Seq(OK)
    val stored = states.find(userId).value.hiddenFilmsByCountry
    stored("pl") shouldBe (1 to 20).map(i => s"PL $i").toSet
    stored("de") shouldBe (1 to 20).map(i => s"DE $i").toSet
  }
}
