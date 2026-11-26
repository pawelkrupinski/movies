package services.identity

import java.time.Instant

import scala.util.Try

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonInt32, BsonNull, BsonValue}
import org.mongodb.scala.MongoDatabase
import play.api.Logging
import services.identity.vocabulary.{ListingKeyBson, Pin, PinClaim, PinStore}

/**
 * The pins in a keyed store ([[tools.KeyedStore]]): one document per pin, `_id` = the pin's content id. Tiny (an
 * escape hatch, not a workflow), so every read is a whole-collection scan. `documents` None -> disabled: reads are
 * empty and writes are refused, as for the other admin stores without Mongo.
 */
abstract class KeyedPinStore(documents: Option[tools.StoreDocuments]) extends PinStore {
  private val pins = new tools.KeyedStore(documents.getOrElse(tools.StoreDocuments.Absent), tools.KeyCodec.Text, KeyedPinStore.Codec)

  def all(): Seq[Pin] = pins.all().map(_._2)

  def insert(pin: Pin): Unit = {
    if (documents.isEmpty) throw new IllegalStateException("identity_pins: no database")
    pins.put(pin.id, pin)
  }

  def delete(id: String): Boolean = pins.delete(id)
}

object KeyedPinStore extends Logging {
  /** A pin's fields beside its `_id`; a document that is no pin is logged and ignored. */
  val Codec: tools.DocCodec[Pin] = new tools.DocCodec[Pin] {
    def encode(pin: Pin): Document = MongoPinStore.encode(pin)
    def decode(document: Document): Option[Pin] = {
      val pin = MongoPinStore.decode(document)
      if (pin.isEmpty) logger.warn(s"identity_pins: undecodable pin ${document.get("_id")}; ignored")
      pin
    }
  }
}

/** The `identity_pins` collection of the shared database, when there is one. */
final class MongoPinStore(sharedDb: Option[MongoDatabase]) extends KeyedPinStore(
  sharedDb.map(db => tools.MongoStoreDocuments.of(db.getCollection(MongoPinStore.Collection))))

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
