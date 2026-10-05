package services

import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent, CommandSucceededEvent}
import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.bson.BsonBinaryWriter
import org.bson.codecs.{BsonDocumentCodec, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeAttempt}

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await

/**
 * The venue rows a projection rebuilds slots from — every film's showtimes, ~240 KB for a large US multiplex — read back
 * in replies the driver's read buffer pool keeps in its 1 MB class at most. Eight to a reply, a big chain's venues made
 * 2 MB replies on every projection; the pool drops a buffer idle for a minute, so each was made again and promoted (the
 * worker-us old-generation heap dump, 2026-10-05: 49 MB of dead pooled read buffers).
 */
class ScrapeArchiveReplySizeIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val largest = new AtomicLong
  private final class Replies extends CommandListener {
    private val finds = new java.util.concurrent.ConcurrentHashMap[Int, String]()
    override def commandStarted(event: CommandStartedEvent): Unit = event.getCommandName match {
      case "find"    => finds.put(event.getRequestId, event.getCommand.getString("find").getValue); ()
      case "getMore" => finds.put(event.getRequestId, event.getCommand.getString("collection").getValue); ()
      case _         => ()
    }
    override def commandSucceeded(event: CommandSucceededEvent): Unit = Option(finds.remove(event.getRequestId)).filter(_ == "cinema_scrapes")
      .foreach { _ =>
        val out = new BasicOutputBuffer()
        new BsonDocumentCodec().encode(new BsonBinaryWriter(out), event.getResponse, EncoderContext.builder().build())
        largest.accumulateAndGet(out.getSize.toLong, math.max); ()
      }
  }

  "a large chain's venues read with their showtimes" should "come back in replies of a megabyte at most" in {
    val client = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new Replies).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "archive-replies"))
    try {
      val archive = new MongoScrapeArchiveRepository(Some(db))
      val venues  = Cinema.byDisplayName.values.toSeq.sortBy(_.displayName).take(16)
      val start   = java.time.LocalDateTime.parse("2031-06-12T10:00")
      // ~240 KB a venue: 40 films, a fortnight of showtimes each with its booking link.
      venues.foreach { cinema =>
        archive.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), java.time.Instant.parse("2031-06-12T08:00:00Z"), listingComplete = true,
          (1 to 40).map(f => CinemaMovie(Movie(s"Film $f"), cinema, None, Some(s"https://venue.example.com/film/$f"), None, Nil, Nil,
            (1 to 40).map(n => Showtime(start.plusHours(n * 7L + f), bookingUrl = Some(s"https://tickets.example.com/${cinema.pillName}/$f/$n?seats=select&format=standard")))))))
      }
      largest.set(0)
      var read = 0
      archive.scanVenues(venues.toSet)(_.foreach(_ => read += 1)).isComplete shouldBe true
      read shouldBe venues.size
      info(s"largest cinema_scrapes reply: ${largest.get} B")
      largest.get should (be > 0L and be <= 1024L * 1024)
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }
}
