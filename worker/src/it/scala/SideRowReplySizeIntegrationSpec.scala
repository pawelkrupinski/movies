package services

import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent, CommandSucceededEvent}
import models.{Showtime, SourceData}
import org.bson.codecs.{BsonDocumentCodec, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.bson.BsonBinaryWriter
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListedShowtimes, MongoScreeningsRepository, MongoSlotsRepository}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await

/**
 * A wide film's side rows read back in replies the driver's read buffer pool keeps in its 1 MB class at most. The pool
 * sizes each buffer to the reply's next power of two and drops one idle for a minute: replies of 1,000 `screenings` rows
 * (~2 KB each on worker-us's wide films, p90 3.6 KB) needed 2 and 4 MB buffers, used every couple of minutes by the
 * re-reads of wide films, so each was dropped and made again — and promoted in between (prod JFR 2026-10-05: 16 of 45
 * old-object samples were those buffers, 1.5 MB/min of new ones).
 */
class SideRowReplySizeIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val Venues  = 2000
  private val MaxReply = 1024L * 1024

  /** The largest find/getMore reply per collection, in bytes as sent. */
  private final class Replies extends CommandListener {
    private val collectionOf = new ConcurrentHashMap[Int, String]()
    val largest = new ConcurrentHashMap[String, AtomicLong]()
    override def commandStarted(event: CommandStartedEvent): Unit = event.getCommandName match {
      case "find"    => collectionOf.put(event.getRequestId, event.getCommand.getString("find").getValue); ()
      case "getMore" => collectionOf.put(event.getRequestId, event.getCommand.getString("collection").getValue); ()
      case _         => ()
    }
    override def commandSucceeded(event: CommandSucceededEvent): Unit = Option(collectionOf.remove(event.getRequestId)).foreach { c =>
      val out = new BasicOutputBuffer()
      new BsonDocumentCodec().encode(new BsonBinaryWriter(out), event.getResponse, EncoderContext.builder().build())
      largest.computeIfAbsent(c, _ => new AtomicLong()).accumulateAndGet(out.getSize.toLong, math.max); ()
    }
    def of(collection: String): Long = Option(largest.get(collection)).fold(0L)(_.get())
    def reset(): Unit = largest.clear()
  }

  "a wide film's screenings and slots" should "come back in replies of a megabyte at most" in {
    val replies = new Replies
    val client  = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(replies).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "side-row-replies"))
    try {
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      val start      = java.time.LocalDateTime.parse("2031-06-12T10:00")
      // As a wide release's rows are: a fortnight of showtimes a venue, each with its booking link (~2 KB a row).
      val listed = (1 to Venues).map { v =>
        s"Venue $v Cinema 16␟verity" -> ListedShowtimes((1 to 14).map(n => Showtime(start.plusHours(n * 9L + v % 7),
          bookingUrl = Some(s"https://tickets.example-cinemas.com/purchase/venue-$v/showtime/${v * 100 + n}?format=standard&seats=select"))), None)
      }.toMap
      screenings.replaceFilm("verity|2026", listed)
      slots.replaceFilm("verity|2026", listed.keys.map(k => k -> SourceData(title = Some("Verity"), rawTitle = Some("VERITY"),
        releaseYear = Some(2026), director = Seq("Michael Showalter"), cast = Seq("Anne Hathaway", "Dakota Johnson", "Josh Hartnett"),
        posterUrl = Some(s"https://images.example-cinemas.com/posters/verity-${k.length}.jpg"),
        filmUrl = Some(s"https://www.example-cinemas.com/movies/verity-${k.hashCode.abs}"))).toMap)
      replies.reset()
      screenings.findListedForFilmChecked("verity|2026").answered.map(_.size) shouldBe Some(Venues)
      screenings.findForFilmsChecked(Set("verity|2026")).answered.map(_.values.map(_.size).sum) shouldBe Some(Venues)
      slots.findForFilmChecked("verity|2026").answered.map(_.size) shouldBe Some(Venues)
      slots.findForFilmsChecked(Set("verity|2026")).answered.map(_.values.map(_.size).sum) shouldBe Some(Venues)
      info(s"largest replies: screenings ${replies.of("screenings")} B, movie_slots ${replies.of("movie_slots")} B")
      replies.of("screenings") should (be > 0L and be <= MaxReply)
      replies.of("movie_slots") should (be > 0L and be <= MaxReply)
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }

  "a presale's wide rows" should "come back in replies of half a megabyte at most, once their store knows the film" in {
    val replies = new Replies
    val client  = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(replies).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "presale-replies"))
    try {
      val screenings = new MongoScreeningsRepository(Some(db))
      val start      = java.time.LocalDateTime.parse("2031-12-17T10:00")
      // ~55 KB a row, as the widest US presale's venues: 500 showtimes each.
      val listed = (1 to 400).map { v =>
        s"Venue $v Cinema 24\u241fpresale" -> ListedShowtimes((1 to 500).map(n => Showtime(start.plusMinutes(n * 15L + v),
          bookingUrl = Some(s"https://tickets.example.com/venue-$v/$n"))), None)
      }.toMap
      screenings.replaceFilm("presale|2031", listed)
      replies.reset()
      screenings.findListedForFilmChecked("presale|2031").answered.map(_.size) shouldBe Some(listed.size)
      screenings.findForFilmsChecked(Set("presale|2031")).answered.map(_.values.map(_.size).sum) shouldBe Some(listed.size)
      screenings.findAtCinemasChecked("presale|2031", (1 to 400).map(v => s"Venue $v Cinema 24").toSet).answered.map(_.size) shouldBe Some(listed.size)
      info(s"largest presale reply: ${replies.of("screenings")} B")
      replies.of("screenings") should (be > 0L and be <= 512L * 1024)
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }

  "a film's row-size estimate" should "be let go once the film has no rows left" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "rowsize-forget") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val rows = Map("Venue 1 Cinema\u241ffilm" -> ListedShowtimes(Seq(Showtime(java.time.LocalDateTime.parse("2031-06-12T10:00"), None)), None))
      Seq("deleted", "emptied", "pruned").foreach(film => screenings.replaceFilm(film, rows))
      Seq("deleted", "emptied", "pruned").foreach(film => withClue(film)(screenings.knowsRowsOf(film) shouldBe true))
      screenings.deleteFilm("deleted")
      screenings.replaceFilm("emptied", Map.empty)
      screenings.deleteFilms(Set("pruned"))
      Seq("deleted", "emptied", "pruned").foreach(film => withClue(film)(screenings.knowsRowsOf(film) shouldBe false))
    }
  }
}

