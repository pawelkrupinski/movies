package integration

import models.{User, UserState}
import org.scalatest.OptionValues._
import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ChangeStreamReopen
import services.users.{UserSessionVersionContract, CaffeineUserChangeTimeCache, UserStateRows, UserStateWritesContract, MongoUserRepository, MongoUserStateRepository, UserCodecs}
import tools.Env
import tools.Eventually.eventually

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

class UserRepositoryIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with UserStateWritesContract with UserSessionVersionContract {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val users  = new MongoUserRepository()
  private val states = new MongoUserStateRepository()
  // The database `states` resolved for itself, for seeding whole rows: the production store
  // has no whole-row write (see `UserStateRows`).
  private lazy val seedClient = MongoClient(Env.get("MONGODB_URI").get)
  protected def seed(state: UserState): Unit =
    UserStateRows.replace(seedClient.getDatabase(models.Country.resolvedDbName), state)

  override protected def afterAll(): Unit = try {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    Await.ready(db.getCollection("users")     .deleteMany(Filters.regex("id",     "^__integration-test-")).toFuture(), 10.seconds)
    Await.ready(db.getCollection("userStates").deleteMany(Filters.regex("userId", "^__integration-test-")).toFuture(), 10.seconds)
    client.close()
    seedClient.close()
    users.close()
    states.close()
  } finally super.afterAll()

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  // The update pipelines behind every hidden-films and legacy-PUT write, held to
  // the same cases the in-memory store runs (`UserStateRepositorySpec`).
  protected def writesStore  = states
  protected val userIdPrefix = "__integration-test-writes-"
  atomicWritesBehaviour("MongoUserStateRepository")

  // `upsert`'s `$replaceWith` and `revokeSessions`' `$inc`, held to the cases the
  // in-memory store runs (`UserRepositorySpec`).
  protected def sessionStore        = users
  protected val sessionUserIdPrefix = "__integration-test-session-"
  sessionVersionBehaviour("MongoUserRepository")

  private def sentinelUser(suffix: String, email: Option[String]) = User(
    id          = s"__integration-test-$suffix",
    provider    = "google",
    providerSub = s"__integration-test-sub-$suffix",
    email       = email,
    displayName = Some(s"Test User $suffix"),
    avatarUrl   = Some("https://lh3/avatar"),
    createdAt   = Now,
    lastSeenAt  = Now
  )
  private def sentinelUser(suffix: String): User =
    sentinelUser(suffix, Some(s"__integration-test-$suffix@example.com"))

  // ── User round-trip ──────────────────────────────────────────────────────

  "MongoUserRepository" should "be enabled when MONGODB_URI is set" in {
    users.enabled shouldBe true
  }

  it should "round-trip a User: upsert → findById → match (all fields including Optionals)" in {
    val u = sentinelUser("roundtrip")
    users.upsert(u)
    users.findById(u.id).value shouldBe u
  }

  it should "round-trip a User with all-None Optionals (user declined email + avatar)" in {
    val u = sentinelUser("no-opts", email = None).copy(displayName = None, avatarUrl = None)
    users.upsert(u)
    val got = users.findById(u.id).value
    got.email       shouldBe None
    got.displayName shouldBe None
    got.avatarUrl   shouldBe None
  }

  it should "find by (provider, providerSub) — the OAuth callback's primary lookup" in {
    val u = sentinelUser("by-sub")
    users.upsert(u)
    users.findByProviderSub("google", u.providerSub).value shouldBe u
    users.findByProviderSub("facebook", u.providerSub) shouldBe empty
  }

  it should "find by email case-insensitively (account-linking key)" in {
    val u = sentinelUser("case-email").copy(email = Some("__integration-test-CaSe@Example.com"))
    users.upsert(u)
    users.findByEmail("__integration-test-case@example.com").value.id shouldBe u.id
    users.findByEmail("__INTEGRATION-TEST-CASE@EXAMPLE.COM").value.id shouldBe u.id
  }

  it should "treat upsert(same id, changed fields) as an update — newest write wins" in {
    val u   = sentinelUser("update")
    users.upsert(u)
    val u2  = u.copy(displayName = Some("Renamed"), lastSeenAt = Now.plusSeconds(3600))
    users.upsert(u2)
    val got = users.findById(u.id).value
    got.displayName shouldBe Some("Renamed")
    got.lastSeenAt  shouldBe Now.plusSeconds(3600)
  }

  it should "delete the user row by id" in {
    val u = sentinelUser("delete")
    users.upsert(u)
    users.findById(u.id) should be (defined)
    users.delete(u.id)
    users.findById(u.id)                          shouldBe empty
    users.findByProviderSub("google", u.providerSub) shouldBe empty
  }

  // ── UserState round-trip ─────────────────────────────────────────────────

  "MongoUserStateRepository" should "be enabled when MONGODB_URI is set" in {
    states.enabled shouldBe true
  }

  it should "round-trip a UserState (Set[String] fields survive the BSON Array codec)" in {
    val s = UserState(
      userId          = "__integration-test-state-rt",
      hiddenFilms     = Set("Madagaskar"),
      disabledCinemas = Set("Kino Apollo", "Cinema City"),
      updatedAt       = Now
    )
    seed(s)
    states.find(s.userId).value shouldBe s
  }

  it should "return None for a userId with no row" in {
    states.find("__integration-test-no-such-user") shouldBe empty
  }

  it should "delete the state row by userId" in {
    val s = UserState("__integration-test-state-delete", Set("X"), Set.empty, Now)
    seed(s)
    states.find(s.userId) should be (defined)
    states.delete(s.userId)
    states.find(s.userId) shouldBe empty
  }

  // Regression: a non-unique index on `userId` let concurrent first-time
  // upserts for the same brand-new user race — each of several parallel
  // `replaceOne(Filters.eq("userId", …), upsert=true)` calls can see no
  // existing row and insert its OWN, leaving multiple documents for one
  // userId (found in prod 2026-09-20: 4 duplicates from exactly this race,
  // `find`/`upsert` thereafter silently disagreeing on which one was "the"
  // row). Reproducing the original race via concurrent app-level calls is
  // inherently timing-dependent — asserting on the INDEX's own effect
  // directly (two raw inserts of the same userId) is what actually changed
  // here and is deterministic: the second insert is rejected outright,
  // rather than merely "unlikely to lose the race" as an upsert-based test
  // would only prove probabilistically.
  it should "reject a second row for a userId that already has one" in {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo")).withCodecRegistry(UserCodecs.registry)
    val coll   = db.getCollection[UserState]("userStates")
    val userId = "__integration-test-state-unique"
    try {
      Await.result(coll.insertOne(UserState(userId, Set("A"), Set.empty, Now)).toFuture(), 10.seconds)
      a[com.mongodb.MongoWriteException] should be thrownBy
        Await.result(coll.insertOne(UserState(userId, Set("B"), Set.empty, Now.plusSeconds(1))).toFuture(), 10.seconds)
      Await.result(db.getCollection("userStates").countDocuments(Filters.eq("userId", userId)).toFuture(), 10.seconds) shouldBe 1
    } finally client.close()
  }


  // The real cursor, not the in-memory ring: a `userStates` row's `_id` is a driver-generated
  // ObjectId, so a DELETE event's documentKey carries no `userId` at all. The change-time
  // cache must still stop vouching for that user — a stale entry would answer a later
  // conditional GET with 304 for state that no longer exists.
  it should "stream a delete through to the change-time cache, even though the event key names no user" in {
    val cache  = new CaffeineUserChangeTimeCache(states)
    val userId = "__integration-test-state-stream-delete"
    cache.start()
    try {
      Thread.sleep(500) // the cursor opens at "now": give it a moment to be open before writing
      seed(UserState(userId, Set("X"), Set.empty, Now))
      eventually(cache.lastChangeAt(userId) shouldBe Some(Now), timeoutMs = 10000)

      states.delete(userId)
      eventually(cache.lastChangeAt(userId) shouldBe None, timeoutMs = 10000)
    } finally cache.stop()
  }

  // Every web pod runs this on boot. Dropping and re-creating the index each time rebuilt it
  // for nothing and, between the drop and the create, left `userStates` with no index at all —
  // no uniqueness for a concurrent first write to hit, and a collection scan for every `find`.
  it should "leave an already-unique userId index alone on the next boot" in {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo")).getCollection("userStates")
    def userIdIndexCreatedAt(): Any =
      Await.result(coll.aggregate(Seq(org.mongodb.scala.bson.collection.immutable.Document(
        "$indexStats" -> org.mongodb.scala.bson.collection.immutable.Document()))).toFuture(), 10.seconds)
        .find(_.get("name").map(_.asString.getValue).contains("userId_1")).value
        .get("accesses").value.asDocument().get("since")
    try {
      val booted = new MongoUserStateRepository()
      try booted.enabled shouldBe true finally booted.close() // boots once: the index exists, unique
      val before = userIdIndexCreatedAt()
      Thread.sleep(50)
      val rebooted = new MongoUserStateRepository()
      try {
        rebooted.enabled shouldBe true
        userIdIndexCreatedAt() shouldBe before
      } finally rebooted.close()
    } finally client.close()
  }

  it should "migrate a legacy plain userId index to a unique one on boot" in {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo")).getCollection("userStates")
    try {
      Await.result(coll.dropIndex("userId_1").toFuture(), 10.seconds)
      Await.result(coll.createIndex(org.mongodb.scala.model.Indexes.ascending("userId")).toFuture(), 10.seconds)

      val rebooted = new MongoUserStateRepository()
      try rebooted.enabled shouldBe true finally rebooted.close()

      Await.result(coll.listIndexes().toFuture(), 10.seconds)
        .find(_.get("name").map(_.asString.getValue).contains("userId_1")).value
        .get("unique").map(_.asBoolean.getValue) shouldBe Some(true)
    } finally client.close()
  }

  // THE REOPEN GAP. A dead `userStates` cursor reopens at "now", so a write made between the
  // death and the reopen is never delivered. The death's `onDisconnect` only covered what the
  // listener knew THEN; the reopen must report losing track again, so nothing a listener picked
  // up in the gap is trusted past it. Runs in its own database: killing the cursor means
  // dropping the collection, and the shared spec DB's `userStates` carries the unique index
  // other cases depend on.
  it should "report lost track again when a dead cursor reopens" in {
    val client  = MongoClient(Env.get("MONGODB_URI").get)
    val dbName  = Env.get("MONGODB_DB").getOrElse("kinowo") + "_userstate_reopen"
    val db      = client.getDatabase(dbName)
    val raw     = db.withCodecRegistry(UserCodecs.registry).getCollection[UserState]("userStates")
    val pendingReopen = new java.util.concurrent.atomic.AtomicReference[() => Unit]()
    val manualReopen: (String, () => Unit) => ChangeStreamReopen =
      (name, reopen) => new ChangeStreamReopen(name, reopen, (_, run) => pendingReopen.set(run))
    val mongo    = new MongoUserStateRepository(Some(db), fallbackToOwnInit = false, reopenDriver = manualReopen)
    val lostTrack = new java.util.concurrent.atomic.AtomicInteger(0)
    val handle   = mongo.watchChanges(_ => (), _ => (), () => { lostTrack.incrementAndGet(); () })
    try {
      Await.result(raw.insertOne(UserState("__integration-test-warm", Set.empty, Set.empty, Now)).toFuture(), 10.seconds)
      Thread.sleep(500)
      Await.result(raw.drop().toFuture(), 10.seconds) // the cursor's terminal end
      eventually(Option(pendingReopen.get()) should not be empty, timeoutMs = 10000)
      val atDeath = lostTrack.get()

      pendingReopen.get()() // the reopen fires
      eventually(lostTrack.get() should be > atDeath, timeoutMs = 10000)
    } finally {
      handle.foreach(_.close())
      Await.ready(db.drop().toFuture(), 10.seconds)
      client.close()
    }
  }

}
