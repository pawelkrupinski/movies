package repositories

import models.{Item, UpdateItemRequest}
import org.bson.types.ObjectId
import org.mongodb.scala._
import org.mongodb.scala.bson.{BsonDateTime, BsonString, Document}
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.model.{FindOneAndUpdateOptions, ReturnDocument}
import play.api.Logger

import java.time.Instant
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/** Low-level data access for the `items` collection. */
@Singleton
class ItemRepository @Inject() (db: MongoDatabase)(implicit ec: ExecutionContext) {

  private val logger     = Logger(getClass)
  private val collection = db.getCollection("items")

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def docToItem(doc: Document): Item =
    Item(
      id          = Some(doc.getObjectId("_id").toHexString),
      name        = doc.getString("name"),
      description = doc.getString("description"),
      price       = BigDecimal(doc.getDouble("price")),
      tags        = Option(doc.getList("tags", classOf[String]))
                      .map(l => List.from(l.toArray.map(_.toString)))
                      .getOrElse(Nil),
      createdAt   = Some(Instant.ofEpochMilli(doc.get("createdAt").asDateTime().getValue)),
      updatedAt   = Some(Instant.ofEpochMilli(doc.get("updatedAt").asDateTime().getValue))
    )

  private def now = BsonDateTime(Instant.now.toEpochMilli)

  // ── CRUD ───────────────────────────────────────────────────────────────────

  def findAll(): Future[Seq[Item]] = {
    collection.find().toFuture().map(_.map(docToItem))
  }

  def findById(id: String): Future[Option[Item]] = {
    if (!ObjectId.isValid(id)) return Future.successful(None)
    collection
      .find(equal("_id", new ObjectId(id)))
      .headOption()
      .map(_.map(docToItem))
  }

  def insert(item: Item): Future[Item] = {
    val timestamp = now
    val doc = Document(
      "_id"         -> new ObjectId(),
      "name"        -> BsonString(item.name),
      "description" -> BsonString(item.description),
      "price"       -> item.price.toDouble,
      "tags"        -> item.tags,
      "createdAt"   -> timestamp,
      "updatedAt"   -> timestamp
    )
    collection.insertOne(doc).toFuture().map { _ =>
      item.copy(
        id        = Some(doc.getObjectId("_id").toHexString),
        createdAt = Some(Instant.ofEpochMilli(timestamp.getValue)),
        updatedAt = Some(Instant.ofEpochMilli(timestamp.getValue))
      )
    }
  }

  def update(id: String, req: UpdateItemRequest): Future[Option[Item]] = {
    if (!ObjectId.isValid(id)) return Future.successful(None)

    // Build only the fields that were supplied
    val updates = List(
      req.name.map(v        => set("name", v)),
      req.description.map(v => set("description", v)),
      req.price.map(v       => set("price", v.toDouble)),
      req.tags.map(v        => set("tags", v)),
      Some(set("updatedAt", now))
    ).flatten

    if (updates.isEmpty) return findById(id)

    collection
      .findOneAndUpdate(
        equal("_id", new ObjectId(id)),
        combine(updates: _*),
        FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
      )
      .headOption()
      .map(_.map(docToItem))
  }

  def delete(id: String): Future[Boolean] = {
    if (!ObjectId.isValid(id)) return Future.successful(false)
    collection
      .deleteOne(equal("_id", new ObjectId(id)))
      .toFuture()
      .map(_.getDeletedCount > 0)
  }
}
