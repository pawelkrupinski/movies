package services.contracts

import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.*
import services.movies.ListingKey
import tools.IsolatedMongoDatabase
import tools.contracts.Implementations

import scala.concurrent.Await
import scala.concurrent.duration.*

/**
 * ONE behaviour suite for [[FilmIdCounterStore]], run against every implementation on the class
 * path: the `identity_film_ids` collection and the in-memory store. Both must refuse — never
 * overwrite — an entry whose film id or counter is taken, which is what keeps the FilmId map
 * append-only whatever writes to it; and [[FilmIdMapping]] over each must seed the same map.
 */
class FilmIdCounterStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "film-id-counters-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def fresh(cls: Class[? <: FilmIdCounterStore]): FilmIdCounterStore = {
    Await.result(isolatedDatabase.database.getCollection(MongoFilmIdCounterStore.Collection).drop().toFuture(), 30.seconds)
    Implementations.construct(cls, _.getTypeName match {
      case "org.mongodb.scala.MongoDatabase" => Some(isolatedDatabase.database)
      case _                                 => None
    }).fold(missing => fail(missing), identity)
  }

  private val implementations =
    Implementations.of(classOf[FilmIdCounterStore], classOf[MongoFilmIdCounterStore], classOf[InMemoryFilmIdCounterStore])

  private def film(id: String, listings: Int) =
    IdSeeding.Film(id, (1 to listings).map(i => ListingKey.Native(s"venue$i", s"https://venue$i/$id", id): ListingKey).toSet)

  "the FilmIdCounterStore implementations" should "include the in-memory store and the Mongo store" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryFilmIdCounterStore", "MongoFilmIdCounterStore")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] refuse, never overwrite, an entry whose film id or counter is already stored" in {
      val store = fresh(cls)
      store.insert(Seq(FilmIdCounter("belle|2013", 1), FilmIdCounter("f00a", 2))) shouldBe 2
      store.insert(Seq(FilmIdCounter("belle|2013", 9), FilmIdCounter("dune|2021", 2), FilmIdCounter("dune|2021", 3))) shouldBe 1
      store.allChecked().required.sortBy(_.counter) shouldBe Seq(FilmIdCounter("belle|2013", 1), FilmIdCounter("f00a", 2), FilmIdCounter("dune|2021", 3))
    }

    it should s"[$name] seed the same map through FilmIdMapping, and add nothing on a second run" in {
      val mapping = new FilmIdMapping(fresh(cls))
      val today   = Seq(film("small", 1), film("big", 3), film("mid", 2))
      mapping.append(today) shouldBe Right(3)
      mapping.append(today :+ film("new", 5)) shouldBe Right(1)
      mapping.append(today :+ film("new", 5)) shouldBe Right(0)
      mapping.load().map(_.entries) shouldBe Right(Seq(
        FilmIdCounter("big", 1), FilmIdCounter("mid", 2), FilmIdCounter("small", 3), FilmIdCounter("new", 4)))
    }
  }

  // An entry the store cannot decode failed the whole read, which stopped every film id being
  // assigned until the collection was edited by hand. It is skipped now — but what it holds is not
  // freed: its film id stays refused by the store's `_id`, so it is still never handed out twice.
  "MongoFilmIdCounterStore" should "skip an entry it cannot decode, never handing its film id out again" in {
    val store = fresh(classOf[MongoFilmIdCounterStore])
    store.insert(Seq(FilmIdCounter("belle|2013", 1))) shouldBe 1
    Await.result(isolatedDatabase.database.getCollection(MongoFilmIdCounterStore.Collection)
      .insertOne(org.mongodb.scala.Document("_id" -> "dune|2021", "counter" -> "two")).toFuture(), 30.seconds)
    store.allChecked().required shouldBe Seq(FilmIdCounter("belle|2013", 1))
    val mapping = new FilmIdMapping(store)
    mapping.append(Seq(film("dune|2021", 1), film("arrival|2016", 1))) shouldBe Right(1)
    mapping.load().map(_.counterOf("arrival|2016")) shouldBe Right(Some(2))
  }
}
