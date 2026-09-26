package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.FakeRequest
import services.users.{InMemoryUserRepository, MongoUserStateRepository, NoUserChangeTimeCache, UserStateWriteOutcomes}
import tools.{Env, IsolatedMongoDatabase}

import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.jdk.CollectionConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/** The per-country hidden-films writes against REAL Mongo, many requests for one
 *  user at once — the case a second tab, the app and a quick double-tap produce.
 *  What matters is what the server does with overlapping writes, so the store
 *  here is `MongoUserStateRepository`, not the in-memory one.
 *
 *  The spec owns its database. In the shared one, a co-running spec's
 *  `afterAll` purge (`UserRepositoryIntegrationSpec` deletes every
 *  `^__integration-test-` userId) could land between these writes and the
 *  read-back and erase the row — every write 200, then `find` returned None.
 *  Isolating by database keeps "neither erases the other" about THIS server's
 *  writes, not about who else is cleaning up. */
class HiddenFilmsConcurrentWritesIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway(Env.fromProcess())

  private val Prefix = "__integration-test-hide-"
  // Every write's reported outcome, as (userId-free) (endpoint, outcome) pairs.
  private val outcomes = new ConcurrentLinkedQueue[(String, String)]()
  private val isolated = IsolatedMongoDatabase.open(tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get, "hidden-films-concurrent")
  private val database = isolated.database
  private val states = new MongoUserStateRepository(Some(database),
    writeOutcomes = (endpoint: String, outcome: String) => { outcomes.add(endpoint -> outcome); () })
  private val users  = new InMemoryUserRepository
  private val pool   = Executors.newFixedThreadPool(32)
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  override protected def afterAll(): Unit = try {
    states.close()
    isolated.drop()
    pool.shutdown()
  } finally super.afterAll()

  private val controller = UserStatePod.controller(states, users, NoUserChangeTimeCache,
    java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC))

  private def signedIn(suffix: String): String = UserStatePod.signIn(users, s"$Prefix$suffix")

  private def hide(userId: String, country: String, title: String) =
    controller.hideFilm(country, title)(FakeRequest("PUT", s"/api/me/$country/hidden-films/x").withSession("userId" -> userId))

  "parallel hides of distinct titles for one user" should "all succeed and all persist — starting from no row at all" in {
    val userId = signedIn("parallel")
    val titles = (1 to 40).map(i => s"Film $i")

    val statuses = Await.result(Future.traverse(titles)(t => Future(status(hide(userId, "pl", t)))), 60.seconds)

    statuses.distinct shouldBe Seq(OK)
    states.find(userId).value.hiddenFilmsByCountry("pl") shouldBe titles.toSet
    // One outcome per write, each on the `hide` endpoint, and every one of them a
    // success — a first-write race is a `conflict` that succeeded on the retry,
    // never a store failure. This is what the write-outcome counter reports.
    val reported = outcomes.asScala.toList
    outcomes.clear()
    reported should have size titles.size.toLong
    reported.map(_._1).distinct shouldBe List(UserStateWriteOutcomes.Endpoint.Hide)
    reported.map(_._2).toSet should contain noneOf (UserStateWriteOutcomes.Outcome.StoreFailure, UserStateWriteOutcomes.Outcome.Unavailable)
  }

  they should "not disturb another country's bucket being written at the same time" in {
    val userId = signedIn("two-countries")
    val writes = (1 to 20).flatMap(i => Seq("pl" -> s"PL $i", "de" -> s"DE $i"))

    Await.result(Future.traverse(writes) { case (c, t) => Future(status(hide(userId, c, t))) }, 60.seconds).distinct shouldBe Seq(OK)
    val stored = states.find(userId).value.hiddenFilmsByCountry
    stored("pl") shouldBe (1 to 20).map(i => s"PL $i").toSet
    stored("de") shouldBe (1 to 20).map(i => s"DE $i").toSet
  }

  "legacy whole-state PUTs racing per-title hides for one user" should "all succeed, and neither erases the other" in {
    val userId = signedIn("legacy-put")
    val titles = (1 to 30).map(i => s"Film $i")
    val puts   = (1 to 30).map(i => if (i % 2 == 0) "en" else "pl")
    def put(language: String) = controller.put()(FakeRequest("PUT", "/api/me/state").withSession("userId" -> userId)
      .withBody(play.api.libs.json.Json.obj("language" -> language, "disabledCinemas" -> Seq("Kino"))))

    val writes = titles.zip(puts).flatMap { case (t, l) => Seq(Future(status(hide(userId, "pl", t))), Future(status(put(l)))) }
    Await.result(Future.sequence(writes), 60.seconds).distinct shouldBe Seq(OK)

    val stored = states.find(userId).value
    stored.hiddenFilmsByCountry("pl") shouldBe titles.toSet
    stored.disabledCinemas shouldBe Set("Kino")
    stored.language should (be(Some("en")) or be(Some("pl")))
  }
}
