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
 * running, an empty hidden-films list served with a fresh validator.
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
    an[Exception] should be thrownBy users.findByEmail("alice@example.com")
  }

  "MongoUserStateRepository.find" should "throw, not answer None, when it cannot reach Mongo" in {
    an[Exception] should be thrownBy states.find("alice@example.com")
  }
}
