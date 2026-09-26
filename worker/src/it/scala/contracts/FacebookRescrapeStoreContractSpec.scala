package services.contracts

import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.sharecards.*
import tools.IsolatedMongoDatabase
import tools.contracts.Implementations

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

/**
 * ONE behaviour suite for [[FacebookRescrapeStore]], run against every implementation on the
 * class path: the fleet's Mongo collection, and the in-memory store the drain's specs share
 * between simulated workers. The drain's pacing, retry and rate-limit specs run on the in-memory
 * one; this is what makes them say something about production.
 */
class FacebookRescrapeStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "facebook-rescrape-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private val collections = new AtomicInteger
  private val t0          = Instant.parse("2026-09-25T16:18:00Z")

  private def fresh(cls: Class[? <: FacebookRescrapeStore]): FacebookRescrapeStore =
    Implementations.construct(cls, _.getTypeName match {
      case "org.mongodb.scala.MongoCollection<org.mongodb.scala.bson.collection.immutable.Document>" =>
        Some(isolatedDatabase.database.getCollection[Document](s"facebook_rescrapes_${collections.incrementAndGet()}"))
      case _ => None
    }).fold(missing => fail(missing), identity)

  private val implementations = Implementations.of(classOf[FacebookRescrapeStore], classOf[MongoFacebookRescrapeStore], classOf[InMemoryFacebookRescrapeStore])

  private def page(url: String, at: Instant = t0, country: String = "us") = RescrapeEntry(country, RescrapeTarget.Page(url), at)
  private def film(id: String, at: Instant = t0, country: String = "us") = RescrapeEntry(country, RescrapeTarget.FilmPages(country, id), at)

  "the FacebookRescrapeStore implementations" should "include the in-memory store and the Mongo store" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryFacebookRescrapeStore", "MongoFacebookRescrapeStore")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] add an entry once, however often it is asked for" in {
      val store = fresh(cls)
      store.add(Seq(page("a"), page("b"), page("a"))) shouldBe 2
      store.add(Seq(page("a", t0.plusSeconds(60)))) shouldBe 0
      store.waitingPages("us") shouldBe 2
    }

    it should s"[$name] hand out the country's oldest due entry of the kind, leased" in {
      val store = fresh(cls)
      store.add(Seq(page("late", t0.plusSeconds(30)), page("early"), page("uk", country = "uk"), film("f")))
      store.hasDue("us", RescrapeKind.Page, t0) shouldBe true
      val claimed = store.claim("us", RescrapeKind.Page, t0, 5.minutes)
      claimed.map(_.target) shouldBe Some(RescrapeTarget.Page("early"))
      claimed.map(_.attempts) shouldBe Some(1)
      store.claim("us", RescrapeKind.Page, t0, 5.minutes) shouldBe None                 // "late" not due, "early" leased
      store.claim("us", RescrapeKind.Film, t0, 5.minutes).map(_.target) shouldBe Some(RescrapeTarget.FilmPages("us", "f"))
      store.claim("us", RescrapeKind.Page, t0.plusSeconds(301), 5.minutes).map(_.target) shouldBe Some(RescrapeTarget.Page("early"))
    }

    it should s"[$name] complete or retry only the claim it was given, not one made since" in {
      val store = fresh(cls)
      store.add(Seq(page("a")))
      val stale = store.claim("us", RescrapeKind.Page, t0, 1.minute).get
      val fresh2 = store.claim("us", RescrapeKind.Page, t0.plusSeconds(61), 1.minute).get
      store.complete(stale)
      store.waitingPages("us") shouldBe 1
      store.retry(fresh2, t0.plusSeconds(600), countAttempt = false)
      store.claim("us", RescrapeKind.Page, t0.plusSeconds(599), 1.minute) shouldBe None
      store.claim("us", RescrapeKind.Page, t0.plusSeconds(600), 1.minute).map(_.attempts) shouldBe Some(2)
    }

    it should s"[$name] give out each slot once, and none while held" in {
      val store = fresh(cls)
      store.takeSlot(t0, 20.seconds) shouldBe true
      store.takeSlot(t0.plusSeconds(19), 20.seconds) shouldBe false
      store.takeSlot(t0.plusSeconds(20), 20.seconds) shouldBe true
      store.holdSlots(t0.plusSeconds(3600))
      store.takeSlot(t0.plusSeconds(3599), 20.seconds) shouldBe false
      store.holdSlots(t0.plusSeconds(100))                                            // never pulls a hold in
      store.takeSlot(t0.plusSeconds(3599), 20.seconds) shouldBe false
      store.takeSlot(t0.plusSeconds(3600), 20.seconds) shouldBe true
    }
  }
}
