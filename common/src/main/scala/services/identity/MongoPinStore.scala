package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonInt32, BsonNull, BsonValue}
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The `identity_pins` collection: one document per pin, `_id` = the pin's content id. Tiny (an
 * escape hatch, not a workflow), so every read is a whole-collection find. `sharedDb` None →
 * disabled: reads are empty and writes are refused, as for the other admin stores without Mongo.
 */
final class MongoPinStore(sharedDb: Option[MongoDatabase]) extends PinStore with Logging {
  private val coll: Option[MongoCollection[Document]] = sharedDb.map(_.getCollection[Document](MongoPinStore.Collection))

  def all(): Seq[Pin] =
    coll.toSeq.flatMap(c => Await.result(c.find().toFuture(), 10.seconds)).flatMap { d =>
      val pin = MongoPinStore.decode(d)
      if (pin.isEmpty) logger.warn(s"identity_pins: undecodable pin ${d.get("_id")}; ignored")
      pin
    }

  def insert(pin: Pin): Unit = coll.fold(throw new IllegalStateException("identity_pins: no database")) { c =>
    Await.result(c.replaceOne(Filters.eq("_id", pin.id), MongoPinStore.encode(pin), ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
    ()
  }

  def delete(id: String): Boolean =
    coll.exists(c => Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds).getDeletedCount > 0)
}

object MongoPinStore {
  val Collection = "identity_pins"

  def encode(pin: Pin): Document = {
    val (kind, tmdbId) = pin.claim match {
      case PinClaim.IsFilm(id)    => ("is-film", Some(id))
      case PinClaim.SameFilm      => ("same-film", None)
      case PinClaim.NeverFilm(id) => ("never-film", Some(id))
    }
    Document(
      "_id"       -> pin.id,
      "kind"      -> kind,
      "tmdbId"    -> tmdbId.fold[BsonValue](BsonNull())(BsonInt32(_)),
      "listings"  -> ListingKeyBson.encodeAll(pin.listings),
      "author"    -> pin.author,
      "reason"    -> pin.reason,
      "createdAt" -> pin.createdAt.toString)
  }

  def decode(d: Document): Option[Pin] = Try {
    val doc    = d.toBsonDocument
    val tmdbId = Option(doc.get("tmdbId")).filter(_.isInt32).map(_.asInt32.getValue)
    val claim  = doc.getString("kind").getValue match {
      case "is-film"    => PinClaim.IsFilm(tmdbId.get)
      case "same-film"  => PinClaim.SameFilm
      case "never-film" => PinClaim.NeverFilm(tmdbId.get)
    }
    Pin(ListingKeyBson.decodeAll(doc.getArray("listings")), claim,
      doc.getString("author").getValue, doc.getString("reason").getValue, Instant.parse(doc.getString("createdAt").getValue))
  }.toOption
}
