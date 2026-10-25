package services

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Updates

/** Updates built from a store's document codec, so the write and the in-memory stand-in ([[BsonRoundTrip]]) store the
 *  same fields. */
object MongoUpdates {

  /** Each of `fields` set: an upsert writing a stored value's fields without replacing the document's others. */
  def setEach(fields: Document): Bson = Updates.combine(fields.toSeq.map { case (field, value) => Updates.set(field, value) }*)
}
