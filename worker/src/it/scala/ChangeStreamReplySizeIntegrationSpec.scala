package services

import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent, CommandSucceededEvent}
import models.Showtime
import org.bson.BsonBinaryWriter
import org.bson.codecs.{BsonDocumentCodec, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ChangeStreamDemand, ListedShowtimes, MongoScreeningsRepository}

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.concurrent.Await

/**
 * A burst of a wide release's venue rows — a presale's ~20 KB each — reaches the worker's `screenings` change stream in
 * replies the driver's read buffer pool keeps in its 1 MB class at most. Read a demand window (256 events) to a reply,
 * the hourly reconcile's and a wide film's writes came back in 4-16 MB replies, each a pooled buffer made again a minute
 * later and promoted (the worker-us old-generation heap dump, 2026-10-05: 1-8 MB dead pooled read buffers).
 */
class ChangeStreamReplySizeIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val streamCursors = java.util.concurrent.ConcurrentHashMap.newKeySet[Long]()
  private val largest       = new AtomicLong
  private final class Replies extends CommandListener {
    private val getMores = new java.util.concurrent.ConcurrentHashMap[Int, Long]()
    override def commandStarted(event: CommandStartedEvent): Unit =
      if (event.getCommandName == "getMore") { getMores.put(event.getRequestId, event.getCommand.getInt64("getMore").getValue); () }
    override def commandSucceeded(event: CommandSucceededEvent): Unit = {
      if (event.getCommandName == "aggregate" && event.getResponse.containsKey("cursor")) {
        val cursor = event.getResponse.getDocument("cursor")
        if (cursor.getString("ns").getValue.endsWith(".screenings")) { streamCursors.add(cursor.getInt64("id").getValue); () }
      }
      Option(getMores.remove(event.getRequestId)).filter(streamCursors.contains).foreach { _ =>
        val out = new BasicOutputBuffer()
        new BsonDocumentCodec().encode(new BsonBinaryWriter(out), event.getResponse, EncoderContext.builder().build())
        largest.accumulateAndGet(out.getSize.toLong, math.max); ()
      }
    }
  }

  "a burst of wide venue rows" should "reach the change stream in replies of a megabyte at most" in {
    val client = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new Replies).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "stream-replies"))
    try {
      val screenings = new MongoScreeningsRepository(Some(db))
      val seen   = new AtomicInteger
      val demand = new ChangeStreamDemand(ChangeStreamDemand.DefaultWindow)
      val handle = screenings.watchApplied((_, applied) => { seen.incrementAndGet(); applied(); demand.applied() }, demand).get
      try {
        org.scalatest.concurrent.Eventually.eventually(org.scalatest.concurrent.Eventually.timeout(SpecTimeouts.Settle))(streamCursors.isEmpty shouldBe false)
        val start = java.time.LocalDateTime.parse("2031-06-12T10:00")
        val rows  = 240
        // ~20 KB a row: a presale's 150 showtimes, each with its booking link.
        screenings.replaceFilm("presale|2031", (1 to rows).map { v =>
          s"Venue $v Cinema␟presale" -> ListedShowtimes((1 to 150).map(n => Showtime(start.plusMinutes(n * 37L + v),
            bookingUrl = Some(s"https://tickets.example.com/venue-$v/showtime/$n?seats=select&format=imax"))), None)
        }.toMap)
        org.scalatest.concurrent.Eventually.eventually(org.scalatest.concurrent.Eventually.timeout(SpecTimeouts.Settle))(seen.get shouldBe rows)
        info(s"largest screenings change-stream reply: ${largest.get} B")
        largest.get should (be > 0L and be <= 1024L * 1024)
      } finally handle.close()
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }
}
