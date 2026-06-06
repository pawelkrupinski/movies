package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The boot-failure contract: with `required = true` (production), a missing
 * or unreachable Mongo must throw out of construction so the app refuses to
 * start. With `required = false` (dev / tests) the same conditions disable
 * the connection and `database` is `None`.
 *
 * Both failure cases use inputs that fail synchronously (absent URI, or a
 * malformed connection string the driver rejects on parse), so no test here
 * touches the network or waits on the 10s connect probe.
 */
class MongoConnectionSpec extends AnyFlatSpec with Matchers {

  // `mongodb://`-less string → the driver rejects it on parse, before any IO.
  private val MalformedUri = "not-a-mongo-uri"

  "MongoConnection with required = true" should "throw when MONGODB_URI is absent" in {
    val ex = intercept[IllegalStateException] {
      new MongoConnection(uri = None, dbName = "kinowo", required = true)
    }
    ex.getMessage should include ("required")
  }

  it should "throw when the connection can't be established" in {
    val ex = intercept[IllegalStateException] {
      new MongoConnection(uri = Some(MalformedUri), dbName = "kinowo", required = true)
    }
    ex.getMessage should include ("required")
  }

  "MongoConnection with required = false" should "disable (database None) when MONGODB_URI is absent" in {
    val conn = new MongoConnection(uri = None, dbName = "kinowo", required = false)
    conn.database shouldBe None
    conn.close()  // idempotent no-op when nothing was opened
  }

  it should "disable (database None) when the connection can't be established" in {
    val conn = new MongoConnection(uri = Some(MalformedUri), dbName = "kinowo", required = false)
    conn.database shouldBe None
    conn.close()
  }

  // The mode/opt-out → required policy used by Wiring.
  "MongoConnection.isRequired" should "require Mongo outside test mode by default" in {
    MongoConnection.isRequired(testMode = false, optedOut = false) shouldBe true
  }

  it should "never require Mongo in test mode" in {
    MongoConnection.isRequired(testMode = true, optedOut = false) shouldBe false
  }

  it should "let a local opt-out disable the requirement outside tests" in {
    MongoConnection.isRequired(testMode = false, optedOut = true) shouldBe false
  }
}
