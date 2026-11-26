package services.contracts

import java.time.LocalDateTime

import scala.concurrent.Await

import models.*
import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ModelRead
import services.identityadapters.{MongoPendingListingStore, PendingListingStore}
import services.identityadapters.PendingListingStore.Pending
import services.movies.SingleCountryNormalizer
import tools.{IsolatedMongoDatabase, SpecTimeouts}

/** The listings the model's input gate holds, as both stores keep them: what `update` writes `read` gives back, a key
 *  written again replaces its row, a released one is gone (a key both released and pending stays pending), and a row
 *  whose key no longer parses is skipped. */
class PendingListingStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "pending-listing-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def listing(title: String) = ModelRead.corpus(Seq(Multikino -> Seq(CinemaMovie(Movie(title), Multikino, None,
    Some(s"${Multikino.displayName}/$title"), None, Nil, Nil, Seq(Showtime(LocalDateTime.of(2026, 10, 1, 18, 0), None))))),
    SingleCountryNormalizer.titleNormalizer).head

  private val lalka = listing("Lalka")
  private val diuna = listing("Diuna")
  private val (first, second) = if (ListingKey.serialised(lalka.key) < ListingKey.serialised(diuna.key)) (lalka, diuna) else (diuna, lalka)

  private def mongo() = {
    isolatedDatabase.freshCollection(MongoPendingListingStore.Collection)
    new MongoPendingListingStore(isolatedDatabase.database)
  }

  private val implementations: Seq[(String, () => PendingListingStore)] =
    Seq("InMemory" -> (() => new PendingListingStore.InMemory), "MongoPendingListingStore" -> (() => mongo()))

  implementations.foreach { case (name, fresh) =>
    s"[$name]" should "read back what an update wrote, its ready version and since" in {
      val store = fresh()
      store.read() shouldBe empty
      store.update(Seq(Pending(second.key, None, 2L), Pending(first.key, Some(first), 1L)), Nil)
      val read = store.read().sortBy(p => ListingKey.serialised(p.key))   // the Mongo store read in natural order until 2026-11-26
      read.map(_.key) shouldBe Seq(first.key, second.key)
      read.map(_.since) shouldBe Seq(1L, 2L)
      read.head.ready.map(_.key) shouldBe Some(first.key)
      read.head.ready.map(_.rawTitle) shouldBe Some(first.rawTitle)
      read(1).ready shouldBe None
    }

    it should "replace a key written again, drop a released one, and keep a key released and pending at once" in {
      val store = fresh()
      store.update(Seq(Pending(first.key, None, 1L), Pending(second.key, None, 2L)), Nil)
      store.update(Seq(Pending(first.key, Some(first), 5L)), Seq(second.key))
      store.read().map(p => p.key -> p.since) shouldBe Seq(first.key -> 5L)
      store.update(Seq(Pending(first.key, None, 7L)), Seq(first.key))
      store.read().map(p => p.key -> p.since) shouldBe Seq(first.key -> 7L)
      store.update(Nil, Seq(first.key))
      store.read() shouldBe empty
    }
  }

  "MongoPendingListingStore" should "skip a row whose key no longer parses" in {
    val store = mongo()
    store.update(Seq(Pending(first.key, None, 1L)), Nil)
    Await.result(isolatedDatabase.database.getCollection[Document](MongoPendingListingStore.Collection)
      .insertOne(Document("_id" -> "not a key", "since" -> 3L)).toFuture(), SpecTimeouts.Io)
    store.read().map(_.key) shouldBe Seq(first.key)
  }
}
