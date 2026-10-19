package services.contracts

import tools.SpecTimeouts

import models.Showtime
import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, InMemoryScreeningsRepository, ListedShowtimes, ListingKey, ScreeningsRepository, SlotKeyed}
import tools.contracts.Implementations
import tools.IsolatedMongoDatabase

import java.time.LocalDateTime
import scala.concurrent.Await

/**
 * ONE behaviour suite for [[ScreeningsRepository]], run against every implementation found on the
 * class path — `MongoScreeningsRepository` and the `InMemoryScreeningsRepository` every unit spec and
 * the fixture harness run — as [[SlotsRepositoryContractSpec]] holds the slot stores to theirs.
 */
class ScreeningsRepositoryContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "screenings-contract")

  private lazy val database = isolatedDatabase.database
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private def fresh(cls: Class[? <: ScreeningsRepository], clock: java.time.Clock): ScreeningsRepository = {
    Await.result(database.getCollection(ScreeningsRepository.Collection).drop().toFuture(), SpecTimeouts.Io)
    Implementations.construct(cls, _.getTypeName match {
      case "scala.Option<org.mongodb.scala.MongoDatabase>" => Some(Some(database))
      case "java.time.Clock"                               => Some(clock)
      case "tools.Stopwatch"                               => Some(_root_.tools.Stopwatch.System)
      // A decorator (the fixpoint harness's write counter) is held to the contract over the in-memory store.
      case "services.movies.ScreeningsRepository"          => Some(new InMemoryScreeningsRepository(clock))
      case _                                               => None
    }).fold(missing => fail(missing), identity)
  }

  private def showtimes(hour: Int): ListedShowtimes =
    ListedShowtimes(Seq(Showtime(LocalDateTime.of(2026, 10, 1, hour, 15), Some("https://book"), Some("1"), List("2D"))), None)

  private val implementations =
    Implementations.of(classOf[ScreeningsRepository], classOf[ScreeningsRepository], classOf[InMemoryMovieRepository])

  "the ScreeningsRepository implementations" should "include the in-memory store and the Mongo repository" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryScreeningsRepository", "MongoScreeningsRepository")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] stamp each row a write changes with its own clock's instant, on every write path" in {
      val clock      = new _root_.tools.MutableClock(_root_.tools.SpecClock.Pinned.instant())
      val screenings = fresh(cls, clock)
      val start      = clock.instant()
      screenings.replaceFilm("a|2026", Map("Helios" -> showtimes(18)))
      clock.advanceSeconds(60)
      screenings.upsertSlot("b|2026", "Helios", showtimes(19))
      clock.advanceSeconds(60)
      screenings.restampSlot("a|2026", "Helios", ListingKey.Native("Helios", "a-1", "A"))
      screenings.rowWrittenAtChecked() shouldBe tools.ReadOutcome.Answered(Map(
        SlotKeyed.idOf("a|2026", "Helios") -> start.plusSeconds(120),
        SlotKeyed.idOf("b|2026", "Helios") -> start.plusSeconds(60)))
    }
  }
}
