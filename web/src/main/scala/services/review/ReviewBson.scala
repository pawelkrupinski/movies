package services.review

import java.time.{Instant, LocalDateTime, ZoneOffset}

import scala.jdk.CollectionConverters.*

import org.mongodb.scala.bson.{BsonDocument, BsonValue}

/** The review reads' field readers over the mirror's BSON documents ([[MongoReviewSource]]). */
private[review] object ReviewBson {
  def idText(v: BsonValue): String = if (v.isString) v.asString.getValue else if (v.isObjectId) v.asObjectId.getValue.toHexString else v.toString
  def string(d: BsonDocument, name: String): Option[String] =
    Option(d.get(name)).filter(_.isString).map(_.asString.getValue.trim).filter(_.nonEmpty)
  def int(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filter(_.isNumber).map(_.asNumber.intValue)
  def strings(d: BsonDocument, name: String): Seq[String] =
    Option(d.get(name)).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.collect { case s if s.isString => s.asString.getValue })
  def doc(d: BsonDocument, name: String): Option[BsonDocument] = Option(d.get(name)).filter(_.isDocument).map(_.asDocument)
  def instant(d: BsonDocument, name: String): Option[Instant] =
    Option(d.get(name)).filter(_.isDateTime).map(v => Instant.ofEpochMilli(v.asDateTime.getValue))
  /** A showtime's stored date-time is a local time written as UTC. */
  def localTime(at: Instant): String = LocalDateTime.ofInstant(at, ZoneOffset.UTC).toString.replace("T", " ")

  /** The facts a slot, venue page or served film document holds, under their field names. */
  def facts(d: BsonDocument, poster: String, directors: String = "director", synopsis: String = "synopsis"): VenueFacts =
    VenueFacts(string(d, "title"), string(d, "originalTitle"), int(d, "releaseYear"), strings(d, directors), int(d, "runtimeMinutes"),
      strings(d, "cast"), strings(d, "countries"), string(d, poster), string(d, synopsis))
}
