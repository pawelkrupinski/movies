package integration

import models.{Country, User, UserState}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.users.{MongoUserRepository, MongoUserStateRepository}
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * ONE account across four country deployments.
 *
 * Each web pod serves one country out of its own database — `kinowo`,
 * `kinowo_uk`, `kinowo_de`, `kinowo_us` — which is right for films and wrong for
 * people. The three Showtimes countries share one origin, so a session cookie
 * minted under `/uk` is sent to `/de`; if `users` is per country that `userId`
 * resolves to nobody and the visitor is silently signed out with their hidden
 * films and /plan picks apparently gone. `Country.usersDbName` is what sends
 * both user collections to one database instead, and this spec is that claim
 * against a real Mongo: the same rows, reached from two different pods'
 * bindings.
 *
 * Both shapes are asserted deliberately. The isolated one is not a leftover —
 * it is the behaviour being fixed, and it has to keep working, because it is
 * still what an unset `MONGODB_USERS_DB` means for a country deployed alone.
 */
class SharedUsersDatabaseIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: this spec creates and DROPS whole databases.
  tools.IntegrationMongo.requireThrowaway()

  // Own prefix so the drop in `afterAll` can never reach a database another spec
  // (or a local dev's corpus) is using.
  private val Prefix   = "kinowo_it_sharedusers"
  private val SharedDb = s"${Prefix}_users"

  private lazy val client: MongoClient = MongoClient(Env.get("MONGODB_URI").get)

  /** The database a pod serving `country` keeps its own corpus in, under this
   *  spec's prefix — standing in for `kinowo_uk` / `kinowo_de`. */
  private def corpusDb(country: Country): String = s"${Prefix}_${country.code}"

  /** Where that pod's `users` + `userStates` actually land, resolved through the
   *  PRODUCTION rule rather than restated here: `MONGODB_USERS_DB` set → the
   *  shared database, unset → the pod's own. */
  private def usersDbFor(country: Country, sharedUsersDb: Option[String]): MongoDatabase =
    client.getDatabase(Country.usersDbNameFrom(sharedUsersDb, corpusDb(country)))

  private def usersOn(country: Country, sharedUsersDb: Option[String]) =
    new MongoUserRepository(Some(usersDbFor(country, sharedUsersDb)), fallbackToOwnInit = false)

  private def statesOn(country: Country, sharedUsersDb: Option[String]) =
    new MongoUserStateRepository(Some(usersDbFor(country, sharedUsersDb)), fallbackToOwnInit = false)

  override protected def afterAll(): Unit = try {
    (Country.all.map(corpusDb) :+ SharedDb).distinct.foreach { db =>
      Await.ready(client.getDatabase(db).drop().toFuture(), 10.seconds)
    }
    client.close()
  } finally super.afterAll()

  private val Now = Instant.parse("2026-08-30T12:00:00Z")

  private def user(email: String) = User(
    id          = email,
    provider    = "google",
    providerSub = s"sub-$email",
    email       = Some(email),
    displayName = Some("Alice"),
    avatarUrl   = Some("https://lh3/avatar"),
    createdAt   = Now,
    lastSeenAt  = Now
  )

  // ── With a shared users database: the account follows the visitor ─────────

  "A user signed in on one country" should "be the same account on the next when MONGODB_USERS_DB is set" in {
    val shared = Some(SharedDb)
    val alice  = user("alice-shared@example.com")

    usersOn(Country.UnitedKingdom, shared).upsert(alice)

    // A different pod, a different corpus database, the same person.
    usersOn(Country.Germany, shared).findById(alice.id).value shouldBe alice
    usersOn(Country.UnitedStates, shared).findById(alice.id).value shouldBe alice
    usersOn(Country.Poland, shared).findById(alice.id).value shouldBe alice
  }

  it should "keep the identity lookups the OAuth callback uses working across countries" in {
    val shared = Some(SharedDb)
    val bob    = user("bob-shared@example.com")
    usersOn(Country.UnitedKingdom, shared).upsert(bob)

    val germany = usersOn(Country.Germany, shared)
    germany.findByEmail(bob.email.value).value.id            shouldBe bob.id
    germany.findByProviderSub("google", bob.providerSub).value.id shouldBe bob.id
  }

  // The account existing is half of it; the state hanging off it is what the
  // visitor actually notices going missing.
  "Per-user state" should "survive the hop to another country when the users database is shared" in {
    val shared = Some(SharedDb)
    val state  = UserState(
      userId          = "carol-shared@example.com",
      hiddenFilms     = Set("Madagaskar"),
      disabledCinemas = Set("Cinema City"),
      updatedAt       = Now
    )
    statesOn(Country.UnitedKingdom, shared).upsert(state)
    statesOn(Country.Germany, shared).find(state.userId).value shouldBe state
  }

  it should "let one country's write be read back by another — last write wins, not a per-country copy" in {
    val shared = Some(SharedDb)
    val userId = "dave-shared@example.com"
    statesOn(Country.UnitedKingdom, shared).upsert(UserState(userId, Set("Hidden-UK"), Set.empty, Now))
    statesOn(Country.Germany, shared).upsert(UserState(userId, Set("Hidden-DE"), Set.empty, Now.plusSeconds(60)))

    statesOn(Country.UnitedKingdom, shared).find(userId).value.hiddenFilms shouldBe Set("Hidden-DE")
  }

  // ── Without one: the old, isolated shape, which must keep working ─────────

  "A country deployed on its own" should "keep its users in its own database when MONGODB_USERS_DB is unset" in {
    val alice = user("alice-isolated@example.com")
    usersOn(Country.UnitedKingdom, None).upsert(alice)

    // Not a bug — this IS what an unset variable has to mean, and it is exactly
    // the silent sign-out the shared database exists to prevent.
    usersOn(Country.Germany, None).findById(alice.id) shouldBe empty
    usersOn(Country.UnitedKingdom, None).findById(alice.id).value shouldBe alice
  }

  it should "not read the shared database by accident" in {
    val eve = user("eve-shared-only@example.com")
    usersOn(Country.UnitedKingdom, Some(SharedDb)).upsert(eve)
    usersOn(Country.UnitedKingdom, None).findById(eve.id) shouldBe empty
  }
}
