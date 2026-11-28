package services.contracts

import java.time.{Instant, LocalDateTime}

import models.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{ListingRetention, ModelRead}
import services.identity.vocabulary.ScreeningDays
import services.tmdb.{MongoRetainedListingStore, RetainedListingStore}
import services.movies.SingleCountryNormalizer
import tools.{IsolatedMongoDatabase, MutableClock}

/** The departed listings, as both stores keep them: nothing read until something was written, the rows in key order with
 *  the time they left, a released row no input until it departs again, a replace leaving exactly the listings it names,
 *  a delete for good. (Mongo's expiry of a released row -- kept, days stripped, indexed -- is
 *  `MongoRetainedListingStoreIntegrationSpec`'s.) */
class RetainedListingStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolated = IsolatedMongoDatabase.open(mongoTarget, "retained-contract")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val t0 = Instant.parse("2026-11-07T10:00:00Z")
  private def listing(title: String) = ModelRead.corpus(Seq(Multikino -> Seq(CinemaMovie(Movie(title), Multikino, None,
    Some(s"${Multikino.displayName}/$title"), None, Nil, Nil, Seq(Showtime(LocalDateTime.of(2026, 10, 1, 18, 0), None))))),
    SingleCountryNormalizer.titleNormalizer).head.copy(screenings = ScreeningDays.of(Seq(java.time.LocalDate.of(2026, 10, 1))))
  private def departed(title: String, at: Long = t0.toEpochMilli) = ListingRetention.Departed(listing(title), at)
  private val Seq(first, second, third) = Seq("Lalka", "Diuna", "Zjawa").map(departed(_)).sortBy(d => models.ListingKey.serialised(d.listing.key))

  private val implementations: Seq[(String, () => RetainedListingStore)] = Seq(
    "InMemory" -> (() => new RetainedListingStore.InMemory(None)),
    "MongoRetainedListingStore" -> (() => {
      isolated.freshCollection(MongoRetainedListingStore.Collection)
      MongoRetainedListingStore.indexed(isolated.database, new MutableClock(t0))
    }))

  private def keys(store: RetainedListingStore) = store.read().map(_.map(_.listing.key))

  implementations.foreach { case (name, fresh) =>
    s"[$name]" should "read nothing until something was written, and an empty set once an empty one was" in {
      val store = fresh()
      store.read() shouldBe None
      store.update(Nil, Nil)
      store.read() shouldBe Some(Nil)
    }

    it should "read what an update wrote in key order, each with the time it left" in {
      val store = fresh()
      store.update(Seq(second, first.copy(at = 5L)), Nil)
      store.read().map(_.map(d => d.listing.key -> d.at)) shouldBe Some(Seq(first.listing.key -> 5L, second.listing.key -> t0.toEpochMilli))
    }

    it should "stop reading a released listing, and read it again when it departs again, once" in {
      val store = fresh()
      store.update(Seq(first, second), Nil)
      store.update(Nil, Seq(first.listing.key, third.listing.key))
      keys(store) shouldBe Some(Seq(second.listing.key))
      store.update(Seq(first), Nil)
      store.update(Seq(first), Nil)
      keys(store) shouldBe Some(Seq(first.listing.key, second.listing.key))
    }

    it should "keep exactly the listings a replace names, and nothing it does not" in {
      val store = fresh()
      store.update(Seq(first, second), Nil)
      store.replace(Seq(second, third))
      keys(store) shouldBe Some(Seq(second.listing.key, third.listing.key))
      store.replace(Nil)
      store.read() shouldBe Some(Nil)
    }

    it should "forget a deleted listing for good" in {
      val store = fresh()
      store.update(Seq(first, second), Nil)
      store.delete(Seq(first.listing.key))
      store.delete(Nil)
      keys(store) shouldBe Some(Seq(second.listing.key))
    }

    it should "read a listing written before departures were dated as Unknown" in {
      val store = fresh()
      store.update(RetainedListingStore.undated(Seq(first.listing)), Nil)
      store.read().map(_.map(_.at)) shouldBe Some(Seq(ListingRetention.Unknown))
    }
  }
}
