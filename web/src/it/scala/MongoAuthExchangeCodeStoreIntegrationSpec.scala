package integration

import org.mongodb.scala.model.{Filters, Updates}
import org.mongodb.scala.{Document, ObservableFuture, SingleObservableFuture, ToSingleObservableUnit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.auth.{MongoAuthExchangeCodeStore, PendingExchangeCode}
import tools.{Eventually, IsolatedMongoDatabase, MongoTtlSpecClock}

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

  // From the TTL-safe clock, not a past literal: the store's TTL index on
  // `issuedAt` let Mongo's monitor delete an already-expired code between `put`
  // and `remove` — the held redeem below made that likely in a loaded `itAll`.
  private val Now = MongoTtlSpecClock.Pinned.instant()

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
    // Let go only once the redeem is in flight on the server and has waited on the
    // held document for longer than the redirect budget — read off the server
    // rather than slept for, so a stalled machine cannot release before the redeem
    // is even issued and turn this into a plain fast-path redeem.
    val release = Future {
      val outwaitedBudget = Eventually.poll(timeoutMs = 30000)(
        redeemWaitingMicros(slow.code).exists(_ > MongoAuthExchangeCodeStore.Timeout.toMicros))
      Await.result(ToSingleObservableUnit(session.abortTransaction()).toFuture(), 30.seconds)
      outwaitedBudget
    }
    try store.remove(slow.code).value shouldBe slow
    finally session.close()
    withClue("the redeem was never seen waiting on the held document past the budget: ") {
      Await.result(release, 60.seconds) shouldBe true
    }
  }

  /** How long the `findOneAndDelete` for `code` has been running on the server, if it is. */
  private def redeemWaitingMicros(code: String): Option[Long] =
    Await.result(isolated.client.getDatabase("admin").aggregate[Document](Seq(
      Document("$currentOp" -> Document()),
      Document("$match" -> Document(
        "ns" -> s"${isolated.database.name}.${MongoAuthExchangeCodeStore.CollectionName}",
        "command.findAndModify" -> MongoAuthExchangeCodeStore.CollectionName,
        "command.query._id" -> code)))).toFuture(), 30.seconds)
      .flatMap(_.get("microsecs_running")).map(_.asNumber().longValue()).maxOption
}
