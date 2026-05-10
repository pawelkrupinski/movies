package repositories

import models.Movie
import org.bson.types.ObjectId
import org.mongodb.scala._
import org.mongodb.scala.bson.{BsonDateTime, BsonInt32, BsonString, Document}
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.model.{FindOneAndUpdateOptions, ReturnDocument}
import org.bson.conversions.Bson
import play.api.Logger

import java.time.Instant
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MovieRepository @Inject() (db: MongoDatabase)(implicit ec: ExecutionContext) {

  private val logger     = Logger(getClass)
  private val collection = db.getCollection("movies")

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def docToMovie(doc: Document): Movie =
    Movie(
      id              = Some(doc.getObjectId("_id").toHexString),
      title           = doc.getString("title"),
      originalTitle   = Option(doc.getString("originalTitle")),
      genre           = Option(doc.getString("genre")),
      durationMinutes = Option(doc.getInteger("durationMinutes")).map(_.intValue),
      rating          = Option(doc.getString("rating")),
      posterUrl       = Option(doc.getString("posterUrl")),
      multikinoId     = Option(doc.getString("multikinoId")),
      createdAt       = Some(Instant.ofEpochMilli(doc("createdAt").asDateTime().getValue)),
      updatedAt       = Some(Instant.ofEpochMilli(doc("updatedAt").asDateTime().getValue))
    )

  private def now = BsonDateTime(Instant.now.toEpochMilli)

  // ── Queries ────────────────────────────────────────────────────────────────

  def findAll(): Future[Seq[Movie]] =
    collection.find().toFuture().map(_.map(docToMovie))

  def findById(id: String): Future[Option[Movie]] = {
    if (!ObjectId.isValid(id)) return Future.successful(None)
    collection
      .find(equal("_id", new ObjectId(id)))
      .headOption()
      .map(_.map(docToMovie))
  }

  def findByTitle(title: String): Future[Option[Movie]] =
    collection
      .find(equal("title", title))
      .headOption()
      .map(_.map(docToMovie))

  // ── Writes ─────────────────────────────────────────────────────────────────

  /** Insert a new movie. */
  def insert(movie: Movie): Future[Movie] = {
    val timestamp = now
    val id        = new ObjectId()

    val doc = Document(
      "_id"       -> id,
      "title"     -> BsonString(movie.title),
      "createdAt" -> timestamp,
      "updatedAt" -> timestamp
    )
    movie.originalTitle.foreach(v   => doc.append("originalTitle", BsonString(v)))
    movie.genre.foreach(v           => doc.append("genre", BsonString(v)))
    movie.durationMinutes.foreach(v => doc.append("durationMinutes", BsonInt32(v)))
    movie.rating.foreach(v          => doc.append("rating", BsonString(v)))
    movie.posterUrl.foreach(v       => doc.append("posterUrl", BsonString(v)))
    movie.multikinoId.foreach(v     => doc.append("multikinoId", BsonString(v)))

    collection.insertOne(doc).toFuture().map { _ =>
      movie.copy(
        id        = Some(id.toHexString),
        createdAt = Some(Instant.ofEpochMilli(timestamp.getValue)),
        updatedAt = Some(Instant.ofEpochMilli(timestamp.getValue))
      )
    }
  }

  /** Insert-or-update keyed on `multikinoId` (falls back to `title`).
    * Only sets optional fields when they are present — never clears existing data.
    */
  def upsertByMultikinoId(movie: Movie): Future[Movie] = {
    val filter = movie.multikinoId match {
      case Some(mid) => equal("multikinoId", mid)
      case None      => equal("title", movie.title)
    }

    val timestamp = now
    val newId     = new ObjectId()

    val setUpdates: List[Bson] =
      List(
        Some(set("title", movie.title)),
        movie.originalTitle.map(v   => set("originalTitle", v)),
        movie.genre.map(v           => set("genre", v)),
        movie.durationMinutes.map(v => set("durationMinutes", v)),
        movie.rating.map(v          => set("rating", v)),
        movie.posterUrl.map(v       => set("posterUrl", v)),
        movie.multikinoId.map(v     => set("multikinoId", v)),
        Some(set("updatedAt", timestamp))
      ).flatten

    val updates = combine(
      (setUpdates
        :+ setOnInsert("_id", newId)
        :+ setOnInsert("createdAt", timestamp)
      ): _*
    )

    collection
      .findOneAndUpdate(
        filter,
        updates,
        FindOneAndUpdateOptions().upsert(true).returnDocument(ReturnDocument.AFTER)
      )
      .toFuture()
      .map(docToMovie)
  }

  def delete(id: String): Future[Boolean] = {
    if (!ObjectId.isValid(id)) return Future.successful(false)
    collection
      .deleteOne(equal("_id", new ObjectId(id)))
      .toFuture()
      .map(_.getDeletedCount > 0)
  }
}
