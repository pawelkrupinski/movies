package services.movies

import tools.SpecTimeouts

import com.mongodb.event.{CommandListener, CommandStartedEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import models.{Showtime, SourceData}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import scala.concurrent.Await
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
      screenings.rowIdsChecked()       shouldBe tools.ReadOutcome.Answered(expected)
      slots.rowIdsChecked()            shouldBe tools.ReadOutcome.Answered(expected)
      screenings.rowWrittenAtChecked().required.keySet shouldBe expected
      slots.rowWrittenAtChecked().required.keySet      shouldBe expected

      val sent = finds.asScala.toSeq
      sent should not be empty
      sent.foreach { cmd =>
        withClue(s"an unbounded whole-collection find: $cmd ") {
          cmd.containsKey("limit") shouldBe true
          cmd.getNumber("limit").intValue() should (be > 0 and be <= PageSize)
        }
      }
    } finally {
      Await.result(db.drop().toFuture(), SpecTimeouts.Io)
      client.close()
    }
  }

  // The shadow read asks only whether its sampled rows exist. The whole-collection scan above is
  // the wrong answer for that — and reading the films' showtimes timed out in the United States.
  "the side collections' existence check" should "look up exactly the ids it is asked for, never scan" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val settings = MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      })
      .build()
    val client = MongoClient(settings)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "side-row-exists"))
    try {
      val screenings = new MongoScreeningsRepository(Some(db), findAllBatchSize = PageSize)
      val when       = LocalDateTime.now().plusDays(2).withNano(0)
      (1 to Films).foreach(n => screenings.upsertSlot(s"film$n|2026", s"Kino␟film $n", ListedShowtimes(Seq(Showtime(when, None)), None)))
      val asked = Set(SlotKeyed.idOf("film2|2026", "Kino␟film 2"), SlotKeyed.idOf("film5|2026", "Kino␟film 5"), SlotKeyed.idOf("film9|2026", "Kino␟film 9"))

      finds.clear()
      screenings.existingRowIdsChecked(asked) shouldBe tools.ReadOutcome.Answered(asked - SlotKeyed.idOf("film9|2026", "Kino␟film 9"))
      val sent = finds.asScala.toSeq
      sent should have size 1
      sent.head.getDocument("filter").getDocument("_id").getArray("$in").size shouldBe 3
    } finally {
      Await.result(db.drop().toFuture(), SpecTimeouts.Io)
      client.close()
    }
  }

  // A find read to completion with `toFuture()` asks the server for batchSize = Int.MaxValue, so every
  // reply fills to Mongo's 16 MB cap and the driver keeps a buffer that size pooled: worker-uk's
  // per-film reads returned 9-15 MB replies several times an hour, and 32 MB of idle pooled
  // buffers sat in its heap (live dump, 2026-09-29). Rows are ~1 KB, so a bounded batch keeps
  // each reply near a megabyte.
  "the side collections' per-film reads" should "ask for bounded batches, never the whole result in one reply" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val settings = MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      })
      .build()
    val client = MongoClient(settings)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "side-row-batches"))
    try {
      val screenings = new MongoScreeningsRepository(Some(db), findAllBatchSize = PageSize)
      val slots      = new MongoSlotsRepository(Some(db), findAllBatchSize = PageSize)
      val when       = LocalDateTime.now().plusDays(2).withNano(0)
      (1 to Films).foreach { n =>
        screenings.upsertSlot(s"film$n|2026", s"Kino␟film $n", ListedShowtimes(Seq(Showtime(when, None)), None))
        slots.upsertSlot(s"film$n|2026", s"Kino␟film $n", SourceData(title = Some(s"Film $n")))
      }
      val films = (1 to Films).map(n => s"film$n|2026").toSet

      finds.clear()
      screenings.findForFilmsChecked(films).required.keySet shouldBe films
      slots.findForFilmsChecked(films).required.keySet shouldBe films
      val sent = finds.asScala.toSeq
      sent should have size 2
      sent.foreach { cmd =>
        withClue(s"an unbounded batch: $cmd ")(cmd.getNumber("batchSize").intValue() should (be > 0 and be <= tools.MongoReplies.Default))
      }
    } finally {
      Await.result(db.drop().toFuture(), SpecTimeouts.Io)
      client.close()
    }
  }
}
