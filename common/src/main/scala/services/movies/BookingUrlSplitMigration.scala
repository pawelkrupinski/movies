package services.movies

import com.mongodb.client.model.{BulkWriteOptions, ReplaceOneModel}
import models.CityScreening
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import services.Stoppable
import services.readmodel.{MongoReadModelRepository, ReadModelCodecs}
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * One pass over `screenings` and `web_screenings` that rewrites every row stored before
 * booking URLs were split at their row's shared prefix (`ShowtimeCodec.writeShowtimes`).
 *
 * Nothing else would: both collections skip a write whose row decodes to what is stored
 * (`SlotKeyed.changedRows`, the projector's output hash), and a row stored whole decodes to
 * exactly what the split one does — so a row whose showtimes never change would keep its whole
 * URLs for good. On prod those URLs are 42-70% of both collections, ~450 MB of a 1.1 GB
 * WiredTiger cache (2026-10-03).
 *
 * A row is replaced only while its stored `showtimes` are still the ones it read: a scrape or a
 * projection that rewrote it meanwhile wrote the split shape itself, and is never overwritten
 * with what this pass read before it. A collection whose pass reached its end is recorded in
 * `migrations`, so a later boot does not scan it again; an incomplete pass runs again on the
 * next boot. Rows whose URLs share no prefix are left as they are.
 *
 * Remove once every country's `migrations` holds both collections.
 */
final class BookingUrlSplitMigration(
  database:  MongoDatabase,
  clock:     java.time.Clock,
  scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("booking-url-split")
) extends Stoppable with Logging {
  import BookingUrlSplitMigration._

  def start(): Unit = {
    scheduler.schedule((() => {
      Try(run()).recover { case exception => logger.warn(s"Booking-URL split migration failed: ${exception.getMessage}") }
      ()
    }): Runnable, StartupDelaySeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = { scheduler.shutdownNow(); () }

  /** Every collection not yet recorded as migrated: how many of its rows were rewritten, and
   *  whether its pass reached the end (and is now recorded). Public so a spec can run it. */
  def run(): Seq[Outcome] = Seq(
    migrate(ScreeningsRepository.Collection, MovieCodecs.registry.get(classOf[StoredScreeningsDto])),
    migrate(MongoReadModelRepository.ScreeningsCollection, ReadModelCodecs.registry.get(classOf[CityScreening])))

  private def migrate[A](collection: String, codec: Codec[A]): Outcome =
    if (recorded(collection)) Outcome(collection, rewritten = 0, complete = true)
    else {
      val rows     = database.getCollection[BsonDocument](collection)
      var rewrote  = 0L
      val complete = KeysetScan.scan[BsonDocument](
        label          = s"booking-url-split $collection",
        batchSize      = PageSize,
        maxAttempts    = 3,
        initialBackoff = 2.seconds,
        keyOf          = _.getString("_id").getValue,
        fetchPage      = (afterId, limit) => {
          val unsplit = Filters.exists(ShowtimeCodec.RowPrefixField, exists = false)
          val filter  = afterId.fold(unsplit)(id => Filters.and(unsplit, Filters.gt("_id", id)))
          Await.result(rows.find[BsonDocument](filter).sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
        },
        onIncomplete   = exception => logger.warn(s"Booking-URL split of $collection stopped: ${exception.getMessage} — resumes next boot")
      ) { page =>
        val replacements = page.flatMap(row => resplit(row, codec))
        if (replacements.nonEmpty)
          rewrote += Await.result(rows.bulkWrite(replacements, new BulkWriteOptions().ordered(false)).toFuture(), Timeout).getModifiedCount
      }
      if (complete) record(collection)
      logger.info(s"Booking-URL split of $collection: $rewrote row(s) rewritten, ${if (complete) "done" else "incomplete"}")
      Outcome(collection, rewrote, complete)
    }

  /** `row` written again through its codec — when that splits its URLs — guarded on its stored
   *  showtimes being the ones read; `None` for a row that decodes badly or shares no prefix. */
  private[movies] def resplit[A](row: BsonDocument, codec: Codec[A]): Option[ReplaceOneModel[BsonDocument]] =
    Try(codec.decode(new BsonDocumentReader(row), DecoderContext.builder().build())).toOption.flatMap { decoded =>
      val written = new BsonDocument()
      codec.encode(new BsonDocumentWriter(written), decoded, EncoderContext.builder().build())
      Option.when(written.containsKey(ShowtimeCodec.RowPrefixField))(new ReplaceOneModel[BsonDocument](
        Filters.and(Filters.eq("_id", row.get("_id")), Filters.eq("showtimes", row.get("showtimes"))), written))
    }

  private def recorded(collection: String): Boolean =
    Await.result(database.getCollection[Document](Migrations).find(Filters.eq("_id", markerOf(collection))).headOption(), Timeout).isDefined

  private def record(collection: String): Unit = {
    Await.result(database.getCollection[Document](Migrations)
      .insertOne(Document("_id" -> markerOf(collection), "completedAt" -> java.util.Date.from(clock.instant()))).toFuture(), Timeout)
    ()
  }
}

object BookingUrlSplitMigration {
  final case class Outcome(collection: String, rewritten: Long, complete: Boolean)

  /** Where a completed pass is recorded — one document per collection. */
  val Migrations = "migrations"
  def markerOf(collection: String): String = s"booking-url-split:$collection"

  // Off the boot window, as the other boot-time sweeps are; rows stored whole read fine meanwhile.
  private val StartupDelaySeconds = 180L
  private val PageSize            = 200
  private val Timeout             = 60.seconds
}
