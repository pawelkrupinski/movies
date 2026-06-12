package services.movies

import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonWriter}

import java.time.{Instant, LocalDateTime, ZoneOffset}

/**
 * Hand-written JSR-310 codecs the mongo-scala driver doesn't ship. Shared by
 * every collection whose documents embed these types — `movies`
 * (`MovieCodecs`) and the read-model collections (`ReadModelCodecs`) — so the
 * wire shape is identical across them and defined once.
 */
private[services] object JavaTimeCodecs {

  /** `LocalDateTime` ↔ BSON UTC datetime. LocalDateTime carries no zone, so we
   *  pin UTC on the way out and back — matching the long-standing `movies`
   *  showtime encoding, so existing rows decode unchanged. */
  val localDateTime: Codec[LocalDateTime] = new Codec[LocalDateTime] {
    override def encode(w: BsonWriter, v: LocalDateTime, c: EncoderContext): Unit =
      w.writeDateTime(v.toInstant(ZoneOffset.UTC).toEpochMilli)
    override def decode(r: BsonReader, c: DecoderContext): LocalDateTime =
      LocalDateTime.ofInstant(Instant.ofEpochMilli(r.readDateTime()), ZoneOffset.UTC)
    override def getEncoderClass: Class[LocalDateTime] = classOf[LocalDateTime]
  }
}
