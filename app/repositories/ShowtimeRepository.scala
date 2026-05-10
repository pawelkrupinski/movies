package repositories

import models.Showtime
import org.bson.types.ObjectId
import org.mongodb.scala._
import org.mongodb.scala.bson.{BsonDateTime, BsonString, Document}
import org.mongodb.scala.model.Filters._
import play.api.Logger

import java.time.Instant
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ShowtimeRepository @Inject() (db: MongoDatabase)(implicit ec: ExecutionContext) {

  private val logger     = Logger(getClass)
  private val collection = db.getCollection("showtimes")

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def docToShowtime(doc: Document): Showtime =
    Showtime(
      id              = Some(doc.getObjectId("_id").toHexString),
      movieId         = doc.getObjectId("movieId").toHexString,
      cinema          = doc.getString("cinema"),
      date            = doc.getString("date"),
      time            = doc.getString("time"),
      screeningFormat = doc.getString("screeningFormat"),
      language        = doc.getString("language"),
      bookingUrl      = Option(doc.getString("bookingUrl")),
      createdAt       = Some(Instant.ofEpochMilli(doc("createdAt").asDateTime().getValue))
    )

  private def showtimeToDoc(showtime: Showtime, id: ObjectId, timestamp: BsonDateTime): Document = {
    val doc = Document(
      "_id"             -> id,
      "movieId"         -> new ObjectId(showtime.movieId),
      "cinema"          -> BsonString(showtime.cinema),
      "date"            -> BsonString(showtime.date),
      "time"            -> BsonString(showtime.time),
      "screeningFormat" -> BsonString(showtime.screeningFormat),
      "language"        -> BsonString(showtime.language),
      "createdAt"       -> timestamp
    )
    showtime.bookingUrl.foreach(v => doc.append("bookingUrl", BsonString(v)))
    doc
  }

  private def now = BsonDateTime(Instant.now.toEpochMilli)

  // ── Queries ────────────────────────────────────────────────────────────────

  def findAll(): Future[Seq[Showtime]] =
    collection.find().toFuture().map(_.map(docToShowtime))

  def findById(id: String): Future[Option[Showtime]] = {
    if (!ObjectId.isValid(id)) return Future.successful(None)
    collection
      .find(equal("_id", new ObjectId(id)))
      .headOption()
      .map(_.map(docToShowtime))
  }

  def findByMovieId(movieId: String): Future[Seq[Showtime]] = {
    if (!ObjectId.isValid(movieId)) return Future.successful(Nil)
    collection
      .find(equal("movieId", new ObjectId(movieId)))
      .toFuture()
      .map(_.map(docToShowtime))
  }

  def findByCinemaAndDate(cinema: String, date: String): Future[Seq[Showtime]] =
    collection
      .find(and(equal("cinema", cinema), equal("date", date)))
      .toFuture()
      .map(_.map(docToShowtime))

  // ── Writes ─────────────────────────────────────────────────────────────────

  def insert(showtime: Showtime): Future[Showtime] = {
    val timestamp = now
    val id        = new ObjectId()
    val doc       = showtimeToDoc(showtime, id, timestamp)

    collection.insertOne(doc).toFuture().map { _ =>
      showtime.copy(
        id        = Some(id.toHexString),
        createdAt = Some(Instant.ofEpochMilli(timestamp.getValue))
      )
    }
  }

  /** Bulk insert — used by the scraper after fetching a full day's schedule. */
  def insertMany(showtimes: Seq[Showtime]): Future[Int] = {
    if (showtimes.isEmpty) return Future.successful(0)
    val timestamp = now
    val docs = showtimes.map(s => showtimeToDoc(s, new ObjectId(), timestamp))
    collection.insertMany(docs).toFuture().map(_ => docs.size)
  }

  /** Remove all showtimes for a given cinema on a given date.
    * Call this before re-inserting a freshly scraped day to avoid duplicates.
    */
  def deleteByCinemaAndDate(cinema: String, date: String): Future[Long] =
    collection
      .deleteMany(and(equal("cinema", cinema), equal("date", date)))
      .toFuture()
      .map(_.getDeletedCount)

  def delete(id: String): Future[Boolean] = {
    if (!ObjectId.isValid(id)) return Future.successful(false)
    collection
      .deleteOne(equal("_id", new ObjectId(id)))
      .toFuture()
      .map(_.getDeletedCount > 0)
  }
}
