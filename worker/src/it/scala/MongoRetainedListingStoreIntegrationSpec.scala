package integration

import models._
import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{ListingRetention, ModelRead, ScreeningDays}
import services.identity.adapters.MongoRetainedListingStore
import services.movies.SingleCountryNormalizer
import tools.{IsolatedMongoDatabase, MutableClock, SpecTimeouts}

import java.time.{Duration, Instant, LocalDateTime}
import scala.concurrent.Await

/** `identity_retained` through a real Mongo, as an archive (the owner, 2026-11-07: "write and update all listings forever
 *  to mongo db (but not memory) ... marked as deleted when they would be deleted", "still wipe showtimes", "keep updated
 *  time so we could wipe by time in the future"): a row the store would have deleted is EXPIRED, kept with its details
 *  and timestamps and without its screening days; the store never reads an expired row back; a listing that departs
 *  again rewrites its one row; and the reader's query and a future wipe by age each run on the index, reading only the
 *  live rows however many expired ones the collection holds. */
class MongoRetainedListingStoreIntegrationSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement with tools.IntegrationMongoSuite {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val cinema     = Multikino
  private def listing(title: String) = ModelRead.corpus(Seq(cinema -> Seq(CinemaMovie(Movie(title), cinema, None,
    Some(s"${cinema.displayName}/$title"), None, Nil, Nil, Seq(Showtime(LocalDateTime.of(2026, 10, 1, 18, 0), None))))), normalizer).head
    // its screening days, as a kept relay's are held (ListingRetention.kept): what an expired row must not carry
    .copy(screenings = ScreeningDays.of(Seq(java.time.LocalDate.of(2026, 10, 1))))
  private val t0 = Instant.parse("2026-11-07T10:00:00Z")

  private def docs(db: org.mongodb.scala.MongoDatabase) =
    Await.result(db.getCollection[Document](MongoRetainedListingStore.Collection).find().toFuture(), SpecTimeouts.Io)
      .map(_.toBsonDocument).filterNot(_.getString("_id").getValue == MongoRetainedListingStore.Written)

  "the retained listings' store" should "expire a row instead of deleting it, keep its details without its days, and never read it back" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "identity-retained-archive") { db =>
      val clock = new MutableClock(t0)
      val store = new MongoRetainedListingStore(db, clock)
      val lalka = listing("Lalka")
      store.update(Seq(ListingRetention.Departed(lalka, t0.toEpochMilli)), Nil)
      store.read().get.map(_.listing.key) shouldBe Seq(lalka.key)
      docs(db).loneElement.getInt64(MongoRetainedListingStore.Updated).getValue shouldBe t0.toEpochMilli
      docs(db).loneElement.getDocument("listing").containsKey("screenings") shouldBe true

      clock.advance(Duration.ofDays(61))                                         // past the window, or billed again
      store.update(Nil, Seq(lalka.key))
      store.read().get shouldBe empty                                            // never an input again
      val expired = docs(db).loneElement
      expired.getInt64(MongoRetainedListingStore.Expired).getValue shouldBe t0.plus(Duration.ofDays(61)).toEpochMilli
      expired.getInt64(MongoRetainedListingStore.Updated).getValue shouldBe t0.plus(Duration.ofDays(61)).toEpochMilli
      expired.getDocument("listing").containsKey("screenings") shouldBe false   // its showings' days wiped
      expired.getDocument("listing").getString("rawTitle").getValue shouldBe "Lalka" // its details kept

      clock.advance(Duration.ofDays(1))                                          // it departs again: its one row, live
      store.update(Seq(ListingRetention.Departed(lalka, clock.millis())), Nil)
      docs(db).loneElement.containsKey(MongoRetainedListingStore.Expired) shouldBe false
      store.read().get.map(_.listing.key) shouldBe Seq(lalka.key)
      // a replace expires what it does not name, and deletes nothing
      store.replace(Nil)
      store.read().get shouldBe empty
      docs(db).size shouldBe 1
    }
  }

  it should "hold its index from boot, before any write: a fresh worker's first take-up read scans nothing" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "identity-retained-boot-index") { db =>
      val store = MongoRetainedListingStore.indexed(db, new MutableClock(t0))
      val keys  = Await.result(db.getCollection[Document](MongoRetainedListingStore.Collection).listIndexes().toFuture(), SpecTimeouts.Io)
        .map(_.toBsonDocument.getDocument("key"))
      keys should contain(MongoRetainedListingStore.WipeIndex.toBsonDocument)
      store.read() shouldBe None                                                // nothing written, yet the index is there
    }
  }

  it should "read only the live rows, and wipe by age, through its index, however many rows have expired" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "identity-retained-index") { db =>
      val clock = new MutableClock(t0)
      val store = MongoRetainedListingStore.indexed(db, clock)
      // ten expired rows for every live one: the archive's growth against what the worker reads
      val live    = (1 to 50).map(i => ListingRetention.Departed(listing(s"Live $i"), t0.toEpochMilli))
      val expired = (1 to 500).map(i => ListingRetention.Departed(listing(s"Gone $i"), t0.toEpochMilli))
      store.update(live ++ expired, Nil)
      clock.advance(Duration.ofDays(61))
      store.update(Nil, expired.map(_.listing.key))
      store.read().get.size shouldBe live.size

      val coll = db.getCollection[Document](MongoRetainedListingStore.Collection)
      def plan(filter: org.bson.conversions.Bson) =
        Await.result(coll.find(filter).explain[Document](com.mongodb.ExplainVerbosity.EXECUTION_STATS).head(), SpecTimeouts.Io).toBsonDocument
      val reader = plan(MongoRetainedListingStore.Live)
      reader.toJson should include ("IXSCAN")
      // the working set: the live rows and the marker, never the expired ones
      reader.getDocument("executionStats").getNumber("totalDocsExamined").longValue shouldBe (live.size + 1).toLong
      val wipe = plan(MongoRetainedListingStore.expiredBefore(clock.millis() + 1))
      wipe.toJson should include ("IXSCAN")
      wipe.getDocument("executionStats").getNumber("totalDocsExamined").longValue shouldBe expired.size.toLong
    }
  }
}
