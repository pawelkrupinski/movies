package integration

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Indexes
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.sharecards.MongoFacebookRescrapeStore
import tools.{IsolatedMongoDatabase, SpecTimeouts}

import scala.concurrent.Await

/** The claim index was reordered — `enqueuedAt` before the `notBefore` range — and the old order was
 *  left behind on the fleet's collection, maintained on every write and read by nothing. */
class FacebookRescrapeIndexIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "facebook-rescrape-index")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def collection(name: String): MongoCollection[Document] = isolatedDatabase.database.getCollection[Document](name)
  private def indexNames(c: MongoCollection[Document]): Set[String] =
    Await.result(c.listIndexes().toFuture(), SpecTimeouts.Io).flatMap(_.get("name").map(_.asString.getValue)).toSet

  private val Superseded = "country_1_kind_1_notBefore_1_enqueuedAt_1"
  private val Claim      = "country_1_kind_1_enqueuedAt_1_notBefore_1"

  "the Facebook re-scrape store" should "drop the superseded claim index once its replacement is built" in {
    val c = collection("facebook_rescrapes_superseded")
    Await.result(c.createIndex(Indexes.ascending("country", "kind", "notBefore", "enqueuedAt")).toFuture(), SpecTimeouts.Io)
    indexNames(c) should contain(Superseded)
    new MongoFacebookRescrapeStore(c)
    tools.Eventually.eventually {
      val names = indexNames(c)
      names should contain(Claim)
      names should not contain Superseded
    }
  }

  it should "start cleanly on a collection that never had it" in {
    val c = collection("facebook_rescrapes_fresh")
    new MongoFacebookRescrapeStore(c)
    tools.Eventually.eventually(indexNames(c) should contain(Claim))
    new MongoFacebookRescrapeStore(c)                                     // a second boot: nothing left to drop
    tools.Eventually.eventually(indexNames(c) shouldBe Set("_id_", Claim))
  }
}
