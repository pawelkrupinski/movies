package services.identity

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString, BsonValue}
import services.movies.ListingKey

import scala.jdk.CollectionConverters._

/** A [[ListingKey]] as a BSON subdocument — the one encoding every identity collection
 *  (`identity_pins`, `identity_model_families`, `identity_traces`) stores a listing in. */
object ListingKeyBson {

  def encode(k: ListingKey): BsonDocument = k match {
    case ListingKey.Native(venue, page, raw) =>
      new BsonDocument().append("venue", BsonString(venue)).append("page", BsonString(page)).append("rawTitle", BsonString(raw))
    case ListingKey.Published(venue, raw, year, directors) =>
      new BsonDocument().append("venue", BsonString(venue)).append("rawTitle", BsonString(raw))
        .append("year", year.fold[BsonValue](BsonNull())(BsonInt32(_)))
        .append("directors", BsonArray.fromIterable(directors.map(BsonString(_))))
  }

  def decode(d: BsonDocument): ListingKey = read(new org.bson.BsonDocumentReader(d))

  /** The key the reader is at, read field by field (see [[BsonFields]]). */
  def read(reader: org.bson.BsonReader): ListingKey = {
    var venue, page, raw: String = null
    var year: Option[Int]        = None
    var directors: Seq[String]   = null
    BsonFields.document(reader) {
      case "venue"     => venue = reader.readString()
      case "page"      => page = reader.readString()
      case "rawTitle"  => raw = reader.readString()
      case "year"      => year = BsonFields.when(reader, org.bson.BsonType.INT32)(reader.readInt32())
      case "directors" => directors = BsonFields.strings(reader)
      case _           => reader.skipValue()
    }
    BsonFields.required(venue, "venue"); BsonFields.required(raw, "rawTitle")
    if (page != null) ListingKey.Native(venue, page, raw)
    else ListingKey.Published(venue, raw, year, BsonFields.required(directors, "directors"))
  }

  def encodeAll(keys: Iterable[ListingKey]): BsonArray = BsonArray.fromIterable(keys.map(encode))

  def decodeAll(a: BsonArray): Seq[ListingKey] = a.getValues.asScala.toSeq.map(v => decode(v.asDocument))
}
