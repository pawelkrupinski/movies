package services.contracts

import tools.SpecTimeouts

import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.FilmDetail
import services.venuepages.*
import tools.IsolatedMongoDatabase
import tools.contracts.Implementations

import java.time.Instant
import scala.collection.mutable
import scala.concurrent.Await

/**
 * ONE behaviour suite for [[VenuePageStore]], run against every implementation on the class path:
 * the `venue_pages` collection and the in-memory store. A page round-trips with every fact it
 * stated, a gone page with its code, a re-read replaces the earlier one, and a read-through
 * delivers every page once.
 */
class VenuePageStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "venue-pages-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def fresh(cls: Class[? <: VenuePageStore]): VenuePageStore = {
    Await.result(isolatedDatabase.database.getCollection(MongoVenuePageStore.Collection).drop().toFuture(), SpecTimeouts.Io)
    Implementations.construct(cls, _.getTypeName match {
      case "org.mongodb.scala.MongoDatabase" => Some(isolatedDatabase.database)
      case _                                 => None
    }).fold(missing => fail(missing), identity)
  }

  private val implementations =
    Implementations.of(classOf[VenuePageStore], classOf[MongoVenuePageStore], classOf[InMemoryVenuePageStore])

  private val at = Instant.parse("2026-10-01T09:00:00Z")
  private val lalka = VenuePageKey("pionier", "https://pionier1907.pl/event/lalka")
  private val everything = FilmDetail(synopsis = Some("Warszawa, 1878."), cast = Seq("Marie Vinck", "Kacper Olszewski"),
    director = Seq("Maciej Kawalski"), runtimeMinutes = Some(162), releaseYear = Some(2026), originalTitle = Some("Lalka"),
    countries = Seq("Polska"), genres = Seq("Dramat"), posterUrl = Some("https://pionier1907.pl/lalka.jpg"),
    trailerUrl = Some("https://youtu.be/x"), ageRating = Some("12"), format = List("NAP"))

  "the VenuePageStore implementations" should "include the in-memory store and the Mongo store" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryVenuePageStore", "MongoVenuePageStore")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] give a read page back with every fact it stated, and a gone page with its code" in {
      val store = fresh(cls)
      val read  = VenuePage(lalka, VenuePage.Read(everything), at)
      val gone  = VenuePage(VenuePageKey("cinema-city", "https://cinema-city.pl/films/old"), VenuePage.Gone(404), at)
      store.put(read) shouldBe true
      store.put(gone) shouldBe true
      store.get(lalka) shouldBe Some(read)
      store.get(gone.key) shouldBe Some(gone)
      store.get(VenuePageKey("pionier", "https://pionier1907.pl/event/other")) shouldBe None
    }

    it should s"[$name] keep only the latest read of a page" in {
      // Kino Pionier reused /event/lalka for the 2026 film after Has's 1968 one: the re-read wins.
      val store = fresh(cls)
      store.put(VenuePage(lalka, VenuePage.Read(FilmDetail(director = Seq("Wojciech Has"), releaseYear = Some(1968))), at))
      val later = VenuePage(lalka, VenuePage.Read(everything), at.plusSeconds(3600))
      store.put(later) shouldBe true
      store.get(lalka) shouldBe Some(later)
    }

    it should s"[$name] deliver every page once on a read-through" in {
      val store = fresh(cls)
      val pages = (1 to 5).map(i => VenuePage(VenuePageKey("helios", s"https://helios.pl/film/$i"), VenuePage.Read(FilmDetail(runtimeMinutes = Some(90 + i))), at))
      pages.foreach(store.put)
      val seen = mutable.ListBuffer.empty[VenuePage]
      store.foreach(seen += _) shouldBe tools.ScanOutcome.Complete
      seen.toSeq.sortBy(_.key.id) shouldBe pages.sortBy(_.key.id)
    }
  }
}
