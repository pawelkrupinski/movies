package integration

import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.auth.{MongoAuthExchangeCodeStore, PendingExchangeCode}
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/** The handoff code's browser binding survives the real store: the kinowo.net
 *  pod mints, the showtimes.cc pod redeems, and all they share is this
 *  collection — a binding dropped on the way through would make every bound
 *  handoff land signed out. The same goes for a native code's challenge. */
class MongoAuthExchangeCodeStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val DbName = "kinowo_it_authexchangecodes"
  private lazy val client: MongoClient = MongoClient(Env.get("MONGODB_URI").get)
  private lazy val store = new MongoAuthExchangeCodeStore(Some(client.getDatabase(DbName)))

  override protected def afterAll(): Unit = try {
    Await.ready(client.getDatabase(DbName).drop().toFuture(), 10.seconds)
    client.close()
  } finally super.afterAll()

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
}
