package services.contracts

import models.{Showtime, SourceData}
import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemorySlotsRepository, SlotsRepository, WriteOutcome}
import services.staging.InMemoryStagingRepository
import tools.contracts.Implementations
import tools.IsolatedMongoDatabase

import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration.*

/**
 * ONE behaviour suite for [[SlotsRepository]], run against every implementation found on the
 * class path — `MongoSlotsRepository` and the `InMemorySlotsRepository` every unit spec and
 * the fixture harness run. Both claim to "just store"; this is what holds them to it: the
 * same writes, read back the same way, including what a slot does NOT keep.
 */
class SlotsRepositoryContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "slots-contract")

  private lazy val database = isolatedDatabase.database
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def fresh(cls: Class[? <: SlotsRepository]): SlotsRepository = {
    Await.result(database.getCollection(SlotsRepository.Collection).drop().toFuture(), 30.seconds)
    Implementations.construct(cls, _.getTypeName match {
      case "scala.Option<org.mongodb.scala.MongoDatabase>" => Some(Some(database))
      // A decorator (the fixpoint harness's write counter) is held to the contract over the in-memory store.
      case "services.movies.SlotsRepository"              => Some(new InMemorySlotsRepository())
      case _                                               => None
    }).fold(missing => fail(missing), identity)
  }

  private def slot(title: String): SourceData =
    SourceData(title = Some(title), rawTitle = Some(title), synopsis = Some(s"$title, the synopsis"), releaseYear = Some(2026),
      cast = Seq("A", "B"), showtimes = Seq(Showtime(LocalDateTime.of(2026, 10, 1, 19, 15), Some("https://book"), Some("1"), List("2D"))))

  private val implementations =
    Implementations.of(classOf[SlotsRepository], classOf[SlotsRepository], classOf[InMemoryStagingRepository])

  "the SlotsRepository implementations" should "include the in-memory store and the Mongo repository" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemorySlotsRepository", "MongoSlotsRepository")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] set a film's slots to exactly what replaceFilm names, leaving other films alone" in {
      val slots = fresh(cls)
      slots.replaceFilm("film|2026", Map("Helios" -> slot("Film"), "Multikino" -> slot("Film"))) shouldBe WriteOutcome.Written
      slots.replaceFilm("other|2026", Map("Helios" -> slot("Other"))) shouldBe WriteOutcome.Written
      slots.replaceFilm("film|2026", Map("Multikino" -> slot("Film (2D)"))) shouldBe WriteOutcome.Written
      slots.findForFilm("film|2026")  shouldBe Map("Multikino" -> slot("Film (2D)"))
      slots.findForFilm("other|2026") shouldBe Map("Helios" -> slot("Other"))
      slots.replaceFilm("film|2026", Map.empty) shouldBe WriteOutcome.Written
      slots.findForFilm("film|2026") shouldBe empty
    }

    it should s"[$name] upsert, delete one, and delete a whole film's slots" in {
      val slots = fresh(cls)
      slots.upsertSlot("film|2026", "Helios", slot("Film"))
      slots.upsertSlot("film|2026", "Multikino", slot("Film"))
      slots.upsertSlot("film|2026", "Helios", slot("Film, again"))
      slots.upsertSlot("other|2026", "Helios", slot("Other"))
      slots.findForFilm("film|2026") shouldBe Map("Helios" -> slot("Film, again"), "Multikino" -> slot("Film"))
      slots.deleteSlot("film|2026", "Multikino")
      slots.findForFilm("film|2026").keySet shouldBe Set("Helios")
      slots.deleteFilm("film|2026")
      slots.findForFilm("film|2026") shouldBe empty
      slots.findAll() shouldBe Map("other|2026" -> Map("Helios" -> slot("Other")))
    }

    it should s"[$name] read several films at once, completely, and only those asked for" in {
      val slots = fresh(cls)
      Seq("a|2026", "b|2026", "c|2026").foreach(id => slots.upsertSlot(id, "Helios", slot(id)))
      slots.findForFilmsChecked(Set("a|2026", "c|2026", "absent|2026")) shouldBe
        (Map("a|2026" -> Map("Helios" -> slot("a|2026")), "c|2026" -> Map("Helios" -> slot("c|2026"))), true)
    }

    it should s"[$name] never keep a slot's cache-only fields" in {
      val slots  = fresh(cls)
      val cached = slot("Film").copy(showtimesDigest = Some(42), showtimeStartMinutes = Some(IArray(1, 2)))
      slots.upsertSlot("film|2026", "Helios", cached)
      slots.replaceFilm("other|2026", Map("Helios" -> cached))
      Seq("film|2026", "other|2026").foreach { id =>
        val back = slots.findForFilm(id)("Helios")
        withClue(s"$id: ")((back.showtimesDigest, back.showtimeStartMinutes) shouldBe (None, None))
      }
    }

    it should s"[$name] answer the listing-key reads: every row with its stamp, and the rows one listing's key finds" in {
      val slots   = fresh(cls)
      val muranow = models.CinemaShowing(models.KinoMuranow, "belle").displayName
      val belle   = slot("Belle").copy(filmUrl = Some("https://muranow.pl/belle"))
      val belleKey = services.movies.StoredSlotDto.listingKeyOf(muranow, belle).get
      slots.upsertSlot("belle|2013", muranow, belle)
      slots.upsertSlot("belle|2021", muranow, belle)                       // one listing, filed on two films
      slots.upsertSlot("belle|2013", models.Tmdb.displayName, slot("Belle"))   // no venue listing: no stamp
      val id = services.movies.SlotKeyed.idOf
      slots.rowListingKeysChecked() shouldBe (Map(id("belle|2013", muranow) -> Some(belleKey), id("belle|2021", muranow) -> Some(belleKey),
                                                  id("belle|2013", models.Tmdb.displayName) -> None), true)
      slots.rowIdsForListingKeyChecked(belleKey) shouldBe (Set(id("belle|2013", muranow), id("belle|2021", muranow)), true)
      slots.rowIdsForListingKeyChecked(belleKey + "x") shouldBe (Set.empty, true)
    }
  }
}
