package integration

import java.time.Instant

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.auth.{AuthExchangeCodeStore, InMemoryAuthExchangeCodeStore, MongoAuthExchangeCodeStore, PendingExchangeCode}
import tools.{IsolatedMongoDatabase, MongoTtlSpecClock}

import scala.concurrent.duration.*

/** The handoff codes, as both stores keep them: a code is handed back whole, once (a second redeem and a code never
 *  minted find nothing), a code minted twice keeps its first, and a store without a database holds nothing. The slow-Mongo
 *  and binding fidelity cases are `MongoAuthExchangeCodeStoreIntegrationSpec`'s. */
class AuthExchangeCodeStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolated = IsolatedMongoDatabase.open(mongoTarget, "auth-code-contract")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  // The TTL-safe clock, not a past literal: the store's expiry would delete a code minted in the past before it is redeemed.
  private val clock = MongoTtlSpecClock.Pinned
  private val Now   = clock.instant()

  private val implementations: Seq[(String, () => AuthExchangeCodeStore)] = Seq(
    "InMemoryAuthExchangeCodeStore" -> (() => new InMemoryAuthExchangeCodeStore(clock)),
    "MongoAuthExchangeCodeStore"    -> (() => {
      isolated.freshCollection(MongoAuthExchangeCodeStore.CollectionName)
      new MongoAuthExchangeCodeStore(Some(isolated.database), timeout = 60.seconds)
    }))

  implementations.foreach { case (name, fresh) =>
    s"[$name]" should "hand a code back whole, once" in {
      val store   = fresh()
      val pending = PendingExchangeCode("code", "alice@example.com", Now, Some("alices-browser"), Some("challenge"))
      store.put(pending)
      store.remove("code") shouldBe Some(pending)
      store.remove("code") shouldBe None
      store.remove("never-minted") shouldBe None
    }

    it should "keep the first of a code minted twice" in {
      val store = fresh()
      store.put(PendingExchangeCode("code", "alice@example.com", Now))
      store.put(PendingExchangeCode("code", "mallory@example.com", Now))
      store.remove("code").map(_.userId) shouldBe Some("alice@example.com")
    }
  }

  "a store without a database" should "hold nothing" in {
    val store = new MongoAuthExchangeCodeStore(None)
    store.put(PendingExchangeCode("code", "alice@example.com", Now))
    store.remove("code") shouldBe None
  }

  "the in-memory store" should "let a code go a minute past its two, as the TTL index deletes it" in {
    val moving = new tools.MutableClock(Instant.parse("2026-11-26T12:00:00Z"))
    val store  = new InMemoryAuthExchangeCodeStore(moving)
    store.put(PendingExchangeCode("code", "alice@example.com", moving.instant()))
    moving.advance(java.time.Duration.ofSeconds(179))
    store.size shouldBe 1
    moving.advance(java.time.Duration.ofSeconds(1))
    store.size shouldBe 0
  }
}
