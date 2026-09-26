package services.identity

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString, BsonValue}
import services.movies.ListingKey

import scala.jdk.CollectionConverters._

/** A [[ListingKey]] as a BSON subdocument — the one encoding every identity collection
 *  (`identity_pins`, `identity_shadow_decisions`, `identity_shadow_diff`) stores a listing in. */
object ListingKeyBson {

  def encode(k: ListingKey): BsonDocument = k match {
    case ListingKey.Native(venue, page, raw) =>
      new BsonDocument().append("venue", BsonString(venue)).append("page", BsonString(page)).append("rawTitle", BsonString(raw))
    case ListingKey.Published(venue, raw, year, directors) =>
      new BsonDocument().append("venue", BsonString(venue)).append("rawTitle", BsonString(raw))
        .append("year", year.fold[BsonValue](BsonNull())(BsonInt32(_)))
        .append("directors", BsonArray.fromIterable(directors.map(BsonString(_))))
  }

  def decode(d: BsonDocument): ListingKey = {
    val venue = d.getString("venue").getValue
    val raw   = d.getString("rawTitle").getValue
    if (d.containsKey("page")) ListingKey.Native(venue, d.getString("page").getValue, raw)
    else ListingKey.Published(venue, raw, Option(d.get("year")).filter(_.isInt32).map(_.asInt32.getValue),
      d.getArray("directors").getValues.asScala.toSeq.map(_.asString.getValue))
  }

  def encodeAll(keys: Iterable[ListingKey]): BsonArray = BsonArray.fromIterable(keys.map(encode))

  def decodeAll(a: BsonArray): Seq[ListingKey] = a.getValues.asScala.toSeq.map(v => decode(v.asDocument))
}
