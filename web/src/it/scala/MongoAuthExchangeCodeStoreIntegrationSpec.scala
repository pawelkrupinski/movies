package integration

import org.mongodb.scala.model.{Filters, Updates}
import org.mongodb.scala.{Document, SingleObservableFuture, ToSingleObservableUnit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.auth.{MongoAuthExchangeCodeStore, PendingExchangeCode}
import tools.IsolatedMongoDatabase

import java.time.Instant
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/** The handoff code's browser binding survives the real store: the kinowo.net
 *  pod mints, the showtimes.cc pod redeems, and all they share is this
 *  collection — a binding dropped on the way through would make every bound
 *  handoff land signed out. The same goes for a native code's challenge. */
class MongoAuthExchangeCodeStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolated = IsolatedMongoDatabase.open(mongoTarget, "auth-exchange-codes")
  // Not the production 5s redirect budget: that is a latency policy, and this
  // spec checks field fidelity on a Mongo a parallel `itAll` can stall for 15s.
  private lazy val store = new MongoAuthExchangeCodeStore(Some(isolated.database), timeout = 60.seconds)

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val Now = Instant.parse("2026-09-23T12:00:00Z")

  "MongoAuthExchangeCodeStore" should "hand back a code's binding" in {
    store.put(PendingExchangeCode("bound-code", "alice@example.com", Now, Some("alices-browser")))
    store.remove("bound-code").value shouldBe PendingExchangeCode("bound-code", "alice@example.com", Now, Some("alices-browser"))
  }

  it should "hand back an unbound code as unbound" in {
    store.put(PendingExchangeCode("app-code", "alice@example.com", Now))
    store.remove("app-code").value.binding shouldBe empty
  }

  // The native apps' PKCE-style challenge rides the same collection from the
  // callback that mints to the exchange that redeems — possibly another pod.
  it should "hand back a native code's challenge" in {
    val challenged = PendingExchangeCode("pkce-code", "alice@example.com", Now, challenge = Some("E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"))
    store.put(challenged)
    store.remove("pkce-code").value shouldBe challenged
    store.put(PendingExchangeCode("legacy-code", "alice@example.com", Now))
    store.remove("legacy-code").value.challenge shouldBe empty
  }

  // How this spec failed in a loaded parallel `itAll`: Mongo took 15s to answer
  // the redeem (majority write concern stuck behind the rest of the run), past
  // the store's 5s redirect budget, so `remove` gave up and said None for a code
  // the server then deleted. A transaction holding the code's document stands
  // in for that slow server: the redeem cannot finish until it lets go.
  it should "hand back a code Mongo is slower than the redirect budget to release" in {
    val slow = PendingExchangeCode("slow-code", "alice@example.com", Now)
    store.put(slow)
    val session = Await.result(isolated.client.startSession().toFuture(), 30.seconds)
    session.startTransaction()
    Await.result(isolated.database.getCollection[Document](MongoAuthExchangeCodeStore.CollectionName)
      .updateOne(session, Filters.eq("_id", slow.code), Updates.set("heldBy", "slow-server")).toFuture(), 30.seconds)
    val release = Future { Thread.sleep(MongoAuthExchangeCodeStore.Timeout.toMillis + 2000); Await.result(ToSingleObservableUnit(session.abortTransaction()).toFuture(), 30.seconds) }
    try store.remove(slow.code).value shouldBe slow
    finally { Await.ready(release, 60.seconds); session.close() }
  }
}
