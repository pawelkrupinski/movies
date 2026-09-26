package integration

import models.{User, UserState}
import org.scalatest.OptionValues._
import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
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

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway(_root_.settings.ProcessConfiguration.resolve())

  // The database is handed in, as `UsersWiring` hands in the shared connection's. It is
  // this spec's own: its old `^__integration-test-` purge of a shared one also erased
  // HiddenFilmsConcurrentWritesIntegrationSpec's rows mid-test.
  private lazy val isolated = tools.IsolatedMongoDatabase.open(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "user-repository")
  private lazy val database = isolated.database
  private lazy val users    = new MongoUserRepository(Some(database))
  private lazy val states   = new MongoUserStateRepository(Some(database))
  // For seeding whole rows: the production store has no whole-row write (see `UserStateRows`).
  protected def seed(state: UserState): Unit = UserStateRows.replace(database, state)

  override protected def afterAll(): Unit = try {
    users.close()
    states.close()
    isolated.drop()
  } finally super.afterAll()

  // The store's only door to Mongo is the database it is handed: with none it is disabled,
  // even though MONGODB_URI (set for this very spec) names a reachable server. It used to
  // read that variable and open a MongoClient of its own, behind the composition root.
  "The users stores" should "stay disabled when handed no database, never opening their own connection" in {
    val ownUsers  = new MongoUserRepository(None)
    val ownStates = new MongoUserStateRepository(None)
    try {
      ownUsers.enabled  shouldBe false
      ownStates.enabled shouldBe false
      ownUsers.findById("__integration-test-nobody") shouldBe None
      ownStates.find("__integration-test-nobody") shouldBe None
    } finally { ownUsers.close(); ownStates.close() }
  }

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

  "MongoUserRepository" should "be enabled when handed a database" in {
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

  "MongoUserStateRepository" should "be enabled when handed a database" in {
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
    val coll   = database.withCodecRegistry(UserCodecs.registry).getCollection[UserState]("userStates")
    val userId = "__integration-test-state-unique"
    Await.result(coll.insertOne(UserState(userId, Set("A"), Set.empty, Now)).toFuture(), 10.seconds)
    a[com.mongodb.MongoWriteException] should be thrownBy
      Await.result(coll.insertOne(UserState(userId, Set("B"), Set.empty, Now.plusSeconds(1))).toFuture(), 10.seconds)
    Await.result(database.getCollection("userStates").countDocuments(Filters.eq("userId", userId)).toFuture(), 10.seconds) shouldBe 1
  }


  // The real cursor, not the in-memory ring: a `userStates` row's `_id` is a driver-generated
  // ObjectId, so a DELETE event's documentKey carries no `userId` at all. The change-time
  // cache must still stop vouching for that user — a stale entry would answer a later
  // conditional GET with 304 for state that no longer exists.
  // In its own database: the cursor watches the whole collection, and in the shared one every
  // sibling spec's `userStates` write reaches it (the shape that flaked a `screenings` watcher).
  it should "stream a delete through to the change-time cache, even though the event key names no user" in
    tools.IsolatedMongoDatabase.withDatabase(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "userstate-stream-delete") { db =>
      val isolated = new MongoUserStateRepository(Some(db))
      val cache    = new CaffeineUserChangeTimeCache(isolated)
      val userId   = "__integration-test-state-stream-delete"
      cache.start()
      try {
        Thread.sleep(500) // the cursor opens at "now": give it a moment to be open before writing
        UserStateRows.replace(db, UserState(userId, Set("X"), Set.empty, Now))
        eventually(cache.lastChangeAt(userId) shouldBe Some(Now), timeoutMs = 10000)

        isolated.delete(userId)
        eventually(cache.lastChangeAt(userId) shouldBe None, timeoutMs = 10000)
      } finally cache.stop()
    }

  // One row the codec refuses used to END the cursor (the driver decoded every post-image), and
  // the reopen opened at "now", past every write made meanwhile. It is one skipped event now:
  // counted, and reported as lost track (whose row changed, to what, is unknowable).
  it should "keep its change stream open past a row it cannot decode" in
    tools.IsolatedMongoDatabase.withDatabase(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "userstate-malformed") { db =>
      val counted  = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      val isolated = new MongoUserStateRepository(Some(db),
        decodeFailures = collection => { counted.add(collection); () })
      tools.MalformedChangeEventProbe.failure(
        seen => isolated.watchChanges(state => seen(state.userId), _ => (), () => ()).get,
        n => { UserStateRows.replace(db, UserState(s"__integration-test-user$n", Set.empty, Set.empty, Now)); s"__integration-test-user$n" },
        () => Await.result(db.getCollection("userStates").insertOne(org.mongodb.scala.Document(
          "userId" -> "__integration-test-malformed", "hiddenFilms" -> "not an array", "disabledCinemas" -> Seq.empty[String],
          "updatedAt" -> new java.util.Date(0L))).toFuture(), 10.seconds)
      ) shouldBe None
      counted.toArray.toSeq shouldBe Seq("userStates")
    }

  // Every web pod runs this on boot. Dropping and re-creating the index each time rebuilt it
  // for nothing and, between the drop and the create, left `userStates` with no index at all —
  // no uniqueness for a concurrent first write to hit, and a collection scan for every `find`.
  it should "leave an already-unique userId index alone on the next boot" in {
    val coll = database.getCollection("userStates")
    def userIdIndexCreatedAt(): Any =
      Await.result(coll.aggregate(Seq(org.mongodb.scala.bson.collection.immutable.Document(
        "$indexStats" -> org.mongodb.scala.bson.collection.immutable.Document()))).toFuture(), 10.seconds)
        .find(_.get("name").map(_.asString.getValue).contains("userId_1")).value
        .get("accesses").value.asDocument().get("since")
    val booted = new MongoUserStateRepository(Some(database))
    try booted.enabled shouldBe true finally booted.close() // boots once: the index exists, unique
    val before = userIdIndexCreatedAt()
    Thread.sleep(50)
    val rebooted = new MongoUserStateRepository(Some(database))
    try {
      rebooted.enabled shouldBe true
      userIdIndexCreatedAt() shouldBe before
    } finally rebooted.close()
  }

  // THE REOPEN GAP. A dead `userStates` cursor reopens at "now", so a write made between the
  // death and the reopen is never delivered. The death's `onDisconnect` only covered what the
  // listener knew THEN; the reopen must report losing track again, so nothing a listener picked
  // up in the gap is trusted past it. Runs in its own database: killing the cursor means
  // dropping the collection, and the shared spec DB's `userStates` carries the unique index
  // other cases depend on.
  it should "report lost track again when a dead cursor reopens" in
    tools.IsolatedMongoDatabase.withDatabase(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "userstate-reopen") { db =>
    val raw     = db.withCodecRegistry(UserCodecs.registry).getCollection[UserState]("userStates")
    val pendingReopen = new java.util.concurrent.atomic.AtomicReference[() => Unit]()
    val manualReopen: (String, () => Unit) => ChangeStreamReopen =
      (name, reopen) => new ChangeStreamReopen(name, reopen, (_, run) => pendingReopen.set(run))
    val mongo    = new MongoUserStateRepository(Some(db), reopenDriver = manualReopen)
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
    } finally handle.foreach(_.close())
    }

}
