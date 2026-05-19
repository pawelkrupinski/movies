package integration

import models.{User, UserState}
import org.scalatest.OptionValues._
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.users.{MongoUserRepo, MongoUserStateRepo}
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live integration coverage for `MongoUserRepo` + `MongoUserStateRepo`
 * against MongoDB Atlas. Mirrors `MovieRepoIntegrationSpec`: writes
 * sentinel rows with the `__integration-test-…` id convention, asserts
 * round-trip semantics, cleans up.
 *
 * Catches the codec-shape failures the in-memory specs can't:
 *   - the `Macros.createCodecProviderIgnoreNone` macro derives the
 *     right BSON shape for `User` (mix of `String`, `Option[String]`,
 *     `Instant`)
 *   - `Set[String]` round-trips correctly on `UserState`
 *   - case-insensitive `findByEmail` works against the actual Mongo
 *     regex (the Java regex flag in the driver maps to PCRE i flag)
 *   - `delete(id)` matches by the same key the writes index by
 */
class UserRepoIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val users  = new MongoUserRepo()
  private val states = new MongoUserStateRepo()

  // Strip every sentinel doc from both collections at exit so the row count
  // doesn't grow across CI runs.
  override protected def afterAll(): Unit = try {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    Await.ready(db.getCollection("users")     .deleteMany(Filters.regex("id",     "^__integration-test-")).toFuture(), 10.seconds)
    Await.ready(db.getCollection("userStates").deleteMany(Filters.regex("userId", "^__integration-test-")).toFuture(), 10.seconds)
    client.close()
    users.close()
    states.close()
  } finally super.afterAll()

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  // Scala 3 doesn't let one default param see another, so the "no
  // email override" case calls `sentinelUser(suffix, email = None)`
  // explicitly rather than relying on a default referencing `suffix`.
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

  "MongoUserRepo" should "be enabled when MONGODB_URI is set" in {
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
    // Wrong provider must NOT match same sub.
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

  "MongoUserStateRepo" should "be enabled when MONGODB_URI is set" in {
    states.enabled shouldBe true
  }

  it should "round-trip a UserState (Set[String] fields survive the BSON Array codec)" in {
    val s = UserState(
      userId              = "__integration-test-state-rt",
      favouriteMovies     = Set("Conclave", "Dune Part Two"),
      favouriteScreenings = Set("Conclave|Multikino|2026-05-20T18:00", "Dune|Helios|2026-05-21T20:30"),
      hiddenFilms         = Set("Madagaskar"),
      disabledCinemas     = Set("Kino Apollo", "Cinema City"),
      updatedAt           = Now
    )
    states.upsert(s)
    states.find(s.userId).value shouldBe s
  }

  it should "return None for a userId with no row" in {
    states.find("__integration-test-no-such-user") shouldBe empty
  }

  it should "let upsert replace the previous state — last write wins, not merge" in {
    val first  = UserState("__integration-test-state-replace", Set("A"),    Set.empty, Set.empty, Set.empty, Now)
    val second = UserState(first.userId,                       Set("A","B"), Set.empty, Set.empty, Set.empty, Now.plusSeconds(60))
    states.upsert(first)
    states.upsert(second)
    states.find(first.userId).value.favouriteMovies shouldBe Set("A", "B")
  }

  it should "delete the state row by userId" in {
    val s = UserState("__integration-test-state-delete", Set("X"), Set.empty, Set.empty, Set.empty, Now)
    states.upsert(s)
    states.find(s.userId) should be (defined)
    states.delete(s.userId)
    states.find(s.userId) shouldBe empty
  }

}
