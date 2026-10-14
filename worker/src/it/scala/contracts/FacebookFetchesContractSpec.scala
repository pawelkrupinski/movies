package services.contracts

import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.facebook.*
import tools.IsolatedMongoDatabase
import tools.contracts.Implementations

import java.util.concurrent.atomic.AtomicInteger

/**
 * ONE behaviour suite for [[FacebookFetches]], run against the fleet's Mongo collection and the
 * in-memory store the web's and the drain's specs share — so what those specs say about which
 * pages get re-scraped holds for production.
 */
class FacebookFetchesContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "facebook-fetches-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private val collections = new AtomicInteger
  private val t0          = tools.MongoTtlSpecClock.Pinned.instant()

  private def fresh(cls: Class[? <: FacebookFetches]): FacebookFetches =
    Implementations.construct(cls, _.getTypeName match {
      case "org.mongodb.scala.MongoCollection<org.mongodb.scala.bson.collection.immutable.Document>" =>
        Some(isolatedDatabase.database.getCollection[Document](s"facebook_fetches_${collections.incrementAndGet()}"))
      case _ => None
    }).fold(missing => fail(missing), identity)

  private val implementations = Implementations.of(classOf[FacebookFetches], classOf[MongoFacebookFetches], classOf[InMemoryFacebookFetches])

  "the FacebookFetches implementations" should "include the in-memory store and the Mongo store" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryFacebookFetches", "MongoFacebookFetches")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] answer only the pages fetched since the instant asked about" in {
      val store = fresh(cls)
      store.record(Map("https://a" -> t0, "https://b" -> t0.minusSeconds(3600)))
      store.fetchedSince(Seq("https://a", "https://b", "https://never"), t0.minusSeconds(60)) shouldBe Set("https://a")
      store.fetchedSince(Seq("https://a", "https://b"), t0.minusSeconds(7200)) shouldBe Set("https://a", "https://b")
    }

    it should s"[$name] keep a page's latest fetch, whichever order the fetches are recorded in" in {
      val store = fresh(cls)
      store.record(Map("https://a" -> t0))
      store.record(Map("https://a" -> t0.minusSeconds(3600)))
      store.fetchedSince(Seq("https://a"), t0) shouldBe Set("https://a")
      store.record(Map("https://b" -> t0.minusSeconds(3600)))
      store.record(Map("https://b" -> t0))
      store.fetchedSince(Seq("https://b"), t0) shouldBe Set("https://b")
    }

    it should s"[$name] answer nothing for no pages, and record nothing for none" in {
      val store = fresh(cls)
      store.record(Map.empty)
      store.fetchedSince(Nil, t0) shouldBe empty
    }
  }
}
