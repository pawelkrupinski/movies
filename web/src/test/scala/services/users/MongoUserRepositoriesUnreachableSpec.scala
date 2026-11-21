package services.users

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The Mongo user stores against a Mongo that cannot be reached (port 1 refuses; the
 * short selection timeout keeps each op to ~200ms). A lookup that could not run must
 * THROW: `None` is "no such user" / "no state yet", and every caller acts on it — the
 * session is signed out, the user rebuilt as new, a Facebook deletion confirmed without
 * running, an empty hidden-films list served with a fresh validator. A DELETE that could
 * not run must throw too: an account deletion answers "done" — a 204, Meta's receipt —
 * on its return.
 */
class MongoUserRepositoriesUnreachableSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
  private val db     = client.getDatabase("unreachable")

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  private lazy val users  = new MongoUserRepository(Some(db))
  private lazy val states = new MongoUserStateRepository(Some(db))

  "MongoUserRepository" should "throw, not answer None, when a lookup cannot reach Mongo" in {
    an[Exception] should be thrownBy users.findById("alice@example.com")
    an[Exception] should be thrownBy users.findByProviderSub("facebook", "fb-1")
  }

  // The `id` index used to be built inside a bare `Try`: a failure left no trace at all.
  it should "log, not swallow, a failed build of its `id` index" in {
    val logged = tools.LogCapture.thisThread("tools.MongoIndex") {
      new MongoUserRepository(Some(db)).enabled shouldBe true
    }
    logged.map(_.getFormattedMessage).filter(_.contains("unreachable.users")) should not be empty
  }

  it should "throw, not return as if done, when a delete cannot reach Mongo" in {
    an[Exception] should be thrownBy users.delete("alice@example.com")
  }

  // A sign-in answers with a cookie naming the row `upsert` returns: one never written
  // is a "signed in" redirect onto a session every later request reads as signed out.
  it should "throw, not answer the row as stored, when an upsert cannot reach Mongo" in {
    an[Exception] should be thrownBy users.upsert(models.User(
      id = "alice@example.com", provider = "google", providerSub = "G-1", email = Some("alice@example.com"),
      displayName = None, avatarUrl = None, createdAt = java.time.Instant.EPOCH, lastSeenAt = java.time.Instant.EPOCH))
  }

  "MongoUserStateRepository.find" should "throw, not answer None, when it cannot reach Mongo" in {
    an[Exception] should be thrownBy states.find("alice@example.com")
  }

  "MongoUserStateRepository.delete" should "throw, not return as if done, when it cannot reach Mongo" in {
    an[Exception] should be thrownBy states.delete("alice@example.com")
  }
}
