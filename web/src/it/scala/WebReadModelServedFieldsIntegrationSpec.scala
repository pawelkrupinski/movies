package integration

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent, CommandSucceededEvent}
import models.{CityScreening, ResolvedMovie, ResolvedRatings, Showtime}
import org.bson.{BsonArray, BsonDocument}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.{MongoReadModelRepository, WebReadModel}
import tools.{Eventually, SpecTimeouts}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await

/**
 * A web pod reads `web_screenings` without the fields only the worker reads. `listingKeys` (the
 * identity migration's per-row listing keys) is 6–13% of the collection's bytes in prod — 11 MB
 * of US's 187 MB — and the web neither renders nor compares it, yet every boot's hydrate read it
 * over the wire, and every change event's post-image carried it, into a model that held it on the
 * heap. Measured on the wire, against real Mongo: the in-memory store has no wire to count on.
 */
class WebReadModelServedFieldsIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  /** Every `web_screenings` row Mongo returned — scan pages and change events alike — and how
   *  many of them carried `listingKeys`. */
  private final class ScreeningRows extends CommandListener {
    private val onScreenings = ConcurrentHashMap.newKeySet[Int]()
    val rows             = new AtomicLong()
    val withListingKeys  = new AtomicLong()
    val bytes            = new AtomicLong()
    override def commandStarted(event: CommandStartedEvent): Unit = {
      val command = event.getCommand
      val collection = event.getCommandName match {
        case "find" | "aggregate" => Option(command.get(event.getCommandName)).filter(_.isString).map(_.asString.getValue)
        case "getMore"            => Option(command.getString("collection", null)).map(_.getValue)
        case _                    => None
      }
      if (collection.contains(MongoReadModelRepository.ScreeningsCollection)) { onScreenings.add(event.getRequestId); () }
    }
    override def commandSucceeded(event: CommandSucceededEvent): Unit = if (onScreenings.remove(event.getRequestId)) {
      val cursor = event.getResponse.getDocument("cursor", new BsonDocument())
      val batch  = if (cursor.containsKey("firstBatch")) cursor.getArray("firstBatch") else cursor.getArray("nextBatch", new BsonArray())
      batch.forEach { value =>
        val document = value.asDocument
        // A change event carries the row as its `fullDocument`; a scan page carries it bare.
        val row = if (document.containsKey("fullDocument")) document.getDocument("fullDocument") else document
        rows.incrementAndGet()
        bytes.addAndGet(new org.bson.RawBsonDocument(row, new org.bson.codecs.BsonDocumentCodec()).getByteBuffer.remaining().toLong)
        if (row.containsKey("listingKeys")) withListingKeys.incrementAndGet()
      }
    }
  }

  private def withDatabase(body: (MongoDatabase, ScreeningRows) => Unit): Unit = {
    val rows   = new ScreeningRows
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(rows).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "web_served_fields"))
    try body(db, rows)
    finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }

  private val ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private val film    = ResolvedMovie("dune|2021", "Dune", None, None, Nil, None, None, Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def row(cinema: String) = CityScreening(s"dune|2021|wroclaw|$cinema", "dune|2021", "wroclaw", cinema, None,
    Seq(Showtime(java.time.LocalDateTime.parse("2031-06-12T18:00"), bookingUrl = Some(s"https://book/$cinema/1"))),
    listingKeys = Seq(s"""{"venue":"$cinema","title":"Dune: Part One | klasyka"}"""))

  "a web pod" should "read web_screenings without the worker-only listingKeys — at boot and from the change stream" in
    withDatabase { (db, rows) =>
      val repository = new MongoReadModelRepository(Some(db), findAllBatchSize = 2)
      repository.upsertMovie(film)
      Seq("Kino A", "Kino B", "Kino C").map(row).foreach(repository.upsertScreening)

      // The watches replay from this checkpoint, so "Kino D" below reaches the stream however late
      // its cursor opens. Without one they watch "from now", and on a loaded machine the write
      // beats the asynchronous cursor open and is never delivered. This client's Java-default
      // codec registry once made every checkpoint here fail.
      repository.streamCheckpoint() shouldBe defined
      val model = new WebReadModel(repository, clock = tools.SpecClock.Pinned)
      model.start()
      try {
        model.hydrated shouldBe true
        repository.upsertScreening(row("Kino D"))
        Eventually.eventually(model.screeningsForCity("wroclaw").map(_.cinema) should contain("Kino D"))

        info(s"web_screenings rows read: ${rows.rows.get} (${rows.bytes.get} bytes), with listingKeys: ${rows.withListingKeys.get}")
        rows.rows.get should be >= 4L
        rows.withListingKeys.get shouldBe 0L
        model.allScreenings().flatMap(_.listingKeys) shouldBe empty
        // Everything the web DOES serve still arrives.
        model.screeningsForCity("wroclaw").map(_.showtimes.size) shouldBe Seq(1, 1, 1, 1)
      } finally model.stop()

      // The worker's own read keeps them: its projector compares the rows it holds whole.
      var workerRows = Seq.empty[CityScreening]
      repository.foreachScreening(s => workerRows :+= s).isComplete shouldBe true
      workerRows.map(_.listingKeys.size) shouldBe Seq(1, 1, 1, 1)
    }
}
