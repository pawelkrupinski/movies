package services.readmodel

import models.{CityScreening, Showtime}
import services.movies.ShowtimeCodec
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonType, BsonWriter}

/**
 * A `web_screenings` row read field by field, its showtimes through the movies' hand-written
 * `ShowtimeCodec`. It reads as [[DefaultingCodec]] over the macro codec did — a missing field takes the empty
 * screening's value, an absent or null `filmUrl` is `None`, any other null is null, an unknown field
 * is skipped — without
 * first decoding each row into a `BsonDocument` and then decoding that: every row's showtimes went
 * through both, and a whole-collection read is ~108k rows and ~1.7M showtimes on the US (JFR).
 * Written by its own `encode` in the macro codec's shape, booking URLs split at their row's shared
 * prefix. `ReadModelCodecsSpec` pins every stored shape to the old reading.
 */
private[readmodel] object StreamingCityScreeningCodec extends Codec[CityScreening] {
  override def getEncoderClass: Class[CityScreening] = classOf[CityScreening]
  /** As the macro codec wrote it (`IgnoreNone`: an absent `filmUrl` is omitted), its showtimes
   *  split at their row's prefix ([[ShowtimeCodec.writeShowtimes]]). */
  override def encode(w: BsonWriter, v: CityScreening, c: EncoderContext): Unit = {
    w.writeStartDocument()
    w.writeString("_id", v._id)
    w.writeString("filmId", v.filmId)
    w.writeString("city", v.city)
    w.writeString("cinema", v.cinema)
    v.filmUrl.foreach(w.writeString("filmUrl", _))
    ShowtimeCodec.writeShowtimes(w, v.showtimes, c)
    w.writeStartArray("listingKeys")
    v.listingKeys.foreach(w.writeString)
    w.writeEndArray()
    w.writeEndDocument()
  }
  override def decode(r: BsonReader, c: DecoderContext): CityScreening = {
    var id, filmId, city, cinema = ""
    var filmUrl     = Option.empty[String]
    var shows       = Seq.empty[Showtime]
    var listingKeys = Seq.empty[String]
    var urlPrefix: String = null
    // A stored null reads as the macro reads it: `None` for the optional, null for anything else.
    def nullable[A](read: => A): A = if (r.getCurrentBsonType == BsonType.NULL) { r.readNull(); null.asInstanceOf[A] } else read
    def array[A](element: => A): Seq[A] = {
      val out = Vector.newBuilder[A]
      r.readStartArray()
      while (r.readBsonType() != BsonType.END_OF_DOCUMENT) out += element
      r.readEndArray()
      out.result()
    }
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      r.readName() match {
        case "_id"         => id = nullable(r.readString())
        case "filmId"      => filmId = nullable(r.readString())
        case "city"        => city = nullable(r.readString())
        case "cinema"      => cinema = nullable(r.readString())
        case "filmUrl"     => filmUrl = Option(nullable(r.readString()))
        case "showtimes"   => shows = nullable(array(ShowtimeCodec.read(r, c, urlPrefix)))
        case ShowtimeCodec.RowPrefixField => urlPrefix = nullable(r.readString())
        case "listingKeys" => listingKeys = nullable(array(r.readString()))
        case _             => r.skipValue()
      }
    }
    r.readEndDocument()
    CityScreening(id, filmId, city, cinema, filmUrl, if (shows == null) null else ShowtimeCodec.completed(shows, urlPrefix), listingKeys)
  }
}
