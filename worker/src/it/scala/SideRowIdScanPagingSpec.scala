package services.movies

import com.mongodb.event.{CommandListener, CommandStartedEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import models.{Showtime, SourceData}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The retired-venue sweep and the side-row censuses read EVERY row id of `screenings` and
 * `movie_slots`. One unbounded `find()` over a whole collection is the shape that overflowed
 * the async driver's completion chain on `movies` and then `screenings` (see [[KeysetScan]]):
 * the crash lands off the caller's thread, and the caller only ever sees a timeout. These
 * reads must page like every other whole-collection read — every `find` they send carries a
 * limit no bigger than the store's page size.
 */
class SideRowIdScanPagingSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val PageSize = 3
  private val Films    = 8

  "the side collections' row-id scans" should "read every row in bounded pages" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val settings = MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      })
      .build()
    val client = MongoClient(settings)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "side-row-id-paging"))
    try {
      val screenings = new MongoScreeningsRepository(Some(db), findAllBatchSize = PageSize)
      val slots      = new MongoSlotsRepository(Some(db), findAllBatchSize = PageSize)
      val when       = LocalDateTime.now().plusDays(2).withNano(0)
      (1 to Films).foreach { n =>
        screenings.upsertSlot(s"film$n|2026", s"Kino␟film $n", ListedShowtimes(Seq(Showtime(when, None)), None))
        slots.upsertSlot(s"film$n|2026", s"Kino␟film $n", SourceData(title = Some(s"Film $n")))
      }
      val expected = (1 to Films).map(n => SlotKeyed.idOf(s"film$n|2026", s"Kino␟film $n")).toSet

      finds.clear()
      screenings.rowIdsChecked()       shouldBe (expected, true)
      slots.rowIdsChecked()            shouldBe (expected, true)
      screenings.rowWrittenAtChecked()._1.keySet shouldBe expected
      slots.rowWrittenAtChecked()._1.keySet      shouldBe expected

      val sent = finds.asScala.toSeq
      sent should not be empty
      sent.foreach { cmd =>
        withClue(s"an unbounded whole-collection find: $cmd ") {
          cmd.containsKey("limit") shouldBe true
          cmd.getNumber("limit").intValue() should (be > 0 and be <= PageSize)
        }
      }
    } finally {
      Await.result(db.drop().toFuture(), 60.seconds)
      client.close()
    }
  }
}
