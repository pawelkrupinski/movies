package integration

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.lock.MongoLock
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of `MongoLock` against real MongoDB Atlas. Requires MONGODB_URI
 * to be set. Skips otherwise so CI without secrets keeps passing.
 *
 * Uses a sentinel collection name (`__integration_test_locks`) so the test
 * never touches the production `locks` collection. The collection is dropped
 * in `afterAll`.
 */
class MongoLockIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    .getCollection[Document]("__integration_test_locks")

  private def fresh = new MongoLock(coll)

  private def cleanup(name: String): Unit =
    Await.ready(coll.deleteOne(Filters.eq("_id", name)).toFuture(), 5.seconds)

  override protected def afterAll(): Unit = try {
    Await.ready(coll.drop().toFuture(), 10.seconds)
    client.close()
  } finally super.afterAll()

  "MongoLock.acquire" should "claim a never-before-seen lock" in {
    val name = "test-fresh-claim"
    cleanup(name)
    fresh.acquire(name, 30.seconds) shouldBe true
    cleanup(name)
  }

  it should "refuse a second acquire while the first holder is still alive" in {
    val name = "test-contention"
    cleanup(name)
    val a = fresh
    val b = fresh   // same holderId since both share host+pid — represents a re-entrant attempt
    a.acquire(name, 60.seconds) shouldBe true
    // Second acquire by same holder still hits the duplicate-key path because
    // the filter requires expired-or-missing, so it shouldn't succeed.
    b.acquire(name, 60.seconds) shouldBe false
    a.release(name)
    cleanup(name)
  }

  it should "let a takeover succeed after the previous TTL expires" in {
    val name = "test-takeover"
    cleanup(name)
    val a = fresh
    a.acquire(name, 1.second) shouldBe true
    Thread.sleep(1500)
    // TTL elapsed → another acquire matches the `expiresAt < now` filter clause.
    fresh.acquire(name, 30.seconds) shouldBe true
    cleanup(name)
  }

  "release" should "remove the lock so the next acquirer can claim it immediately" in {
    val name = "test-release"
    cleanup(name)
    val a = fresh
    a.acquire(name, 60.seconds) shouldBe true
    a.release(name)
    fresh.acquire(name, 60.seconds) shouldBe true
    cleanup(name)
  }

  "heartbeat" should "extend the TTL on a held lock and return true" in {
    val name = "test-heartbeat"
    cleanup(name)
    val a = fresh
    a.acquire(name, 60.seconds) shouldBe true
    a.heartbeat(name, 120.seconds) shouldBe true
    a.release(name)
    cleanup(name)
  }

  it should "return false when this holder doesn't own the lock" in {
    val name = "test-heartbeat-not-owner"
    cleanup(name)
    fresh.heartbeat(name, 30.seconds) shouldBe false
    cleanup(name)
  }

  "withLock" should "run the action and return Some when the lock is acquired" in {
    val name = "test-withlock-success"
    cleanup(name)
    val result = fresh.withLock(name, 30.seconds) { 42 }
    result shouldBe Some(42)
    cleanup(name)
  }

  it should "return None when the lock is already held" in {
    val name = "test-withlock-blocked"
    cleanup(name)
    val a = fresh
    a.acquire(name, 60.seconds) shouldBe true
    val result = fresh.withLock(name, 30.seconds) { 42 }
    result shouldBe None
    a.release(name)
    cleanup(name)
  }
}
