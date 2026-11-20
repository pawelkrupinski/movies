package services.movies

import java.time.Instant

import models.{Showtime, SourceData}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonType, BsonWriter}

/** The fields every slot-keyed side row carries — its composite `_id`, `filmId`, `slotKey`, `updatedAt` and
 *  `listingKey` — read field by field as the macro codec reads them (an absent or null `listingKey` is `None`); each
 *  row's own fields are its `readOwn`'s, which skips what it does not know. */
private[movies] abstract class SlotKeyedRowFields {
  protected var id, filmId, slotKey: String = null
  protected var updatedAt: Instant          = null
  protected var listingKey                  = Option.empty[String]

  /** Read one field of the row. */
  final def read(name: String, r: BsonReader, c: DecoderContext): Unit = name match {
    case "_id"        => id = r.readString()
    case "filmId"     => filmId = r.readString()
    case "slotKey"    => slotKey = r.readString()
    case "updatedAt"  => updatedAt = Instant.ofEpochMilli(r.readDateTime())
    case "listingKey" => listingKey = BsonReads.optionalString(r)
    case other        => readOwn(other, r, c)
  }

  /** Read every field of the document `r` is at. */
  final def readDocument(r: BsonReader, c: DecoderContext): this.type = {
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) read(r.readName(), r, c)
    r.readEndDocument()
    this
  }

  protected def readOwn(name: String, r: BsonReader, c: DecoderContext): Unit

  /** Whether the row carried every key field it requires. */
  protected def keyed: Boolean = Seq(id, filmId, slotKey, updatedAt).forall(_ != null)
}

/** A `screenings` row read field by field — its showtimes through [[ShowtimeCodec]].
 *  ~108k rows a US pass. Written by the macro codec; read as it reads, as above. */
private[movies] object StreamingScreeningsCodec extends Codec[StoredScreeningsDto] {
  override def getEncoderClass: Class[StoredScreeningsDto] = classOf[StoredScreeningsDto]
  /** As the macro codec wrote it (`WritingNone`: an absent `listingKey` is null), its showtimes
   *  split at their row's prefix ([[ShowtimeCodec.writeShowtimes]]). */
  override def encode(w: BsonWriter, v: StoredScreeningsDto, c: EncoderContext): Unit = {
    w.writeStartDocument()
    w.writeString("_id", v._id)
    w.writeString("filmId", v.filmId)
    w.writeString("slotKey", v.slotKey)
    ShowtimeCodec.writeShowtimes(w, v.showtimes, c)
    w.writeDateTime("updatedAt", v.updatedAt.toEpochMilli)
    v.listingKey match {
      case Some(key) => w.writeString("listingKey", key)
      case None      => w.writeNull("listingKey")
    }
    w.writeEndDocument()
  }
  override def decode(r: BsonReader, c: DecoderContext): StoredScreeningsDto = new ScreeningsRowFields().readDocument(r, c).result

  /** A `screenings` row's fields as [[StreamingScreeningsCodec]] reads them; `result` fails on a missing required one. */
  private final class ScreeningsRowFields extends SlotKeyedRowFields {
    private var urlPrefix: String = null
    private val showtimes         = Vector.newBuilder[Showtime]
    private var sawShowtimes      = false

    protected def readOwn(name: String, r: BsonReader, c: DecoderContext): Unit = name match {
      case ShowtimeCodec.RowPrefixField => urlPrefix = BsonReads.optionalString(r).orNull
      case "showtimes"                  => readShowtimes(r, c)
      case _                            => r.skipValue()
    }

    private def readShowtimes(r: BsonReader, c: DecoderContext): Unit = {
      sawShowtimes = true
      r.readStartArray()
      while (r.readBsonType() != BsonType.END_OF_DOCUMENT) showtimes += ShowtimeCodec.read(r, c, urlPrefix)
      r.readEndArray()
    }

    def result: StoredScreeningsDto = {
      BsonReads.require(keyed && sawShowtimes, "StoredScreeningsDto", id)
      StoredScreeningsDto(id, filmId, slotKey, ShowtimeCodec.completed(showtimes.result(), urlPrefix), updatedAt, listingKey)
    }
  }
}

/** A `movie_slots` row read field by field — its slot through the backward-compatible
 *  [[SourceData]] codec. ~113k rows a US pass. Written by the macro codec; read as it reads:
 *  an absent or null `listingKey` is `None`, an unknown field is skipped, a missing required
 *  field fails (`SlotDecodeSpec`). */
private[movies] final class StreamingSlotCodec(macroCodec: Codec[StoredSlotDto], sourceData: Codec[SourceData])
    extends Codec[StoredSlotDto] {
  override def getEncoderClass: Class[StoredSlotDto] = classOf[StoredSlotDto]
  override def encode(w: BsonWriter, v: StoredSlotDto, c: EncoderContext): Unit = macroCodec.encode(w, v, c)
  override def decode(r: BsonReader, c: DecoderContext): StoredSlotDto =
    new StreamingSlotCodec.SlotRowFields(sourceData).readDocument(r, c).result
}

private[movies] object StreamingSlotCodec {
  /** A `movie_slots` row's fields as [[StreamingSlotCodec]] reads them; `result` fails on a missing required one. */
  final class SlotRowFields(sourceData: Codec[SourceData]) extends SlotKeyedRowFields {
    private var slot: SourceData = null

    protected def readOwn(name: String, r: BsonReader, c: DecoderContext): Unit = name match {
      case "slot" => slot = sourceData.decode(r, c)
      case _      => r.skipValue()
    }

    def result: StoredSlotDto = {
      BsonReads.require(keyed && slot != null, "StoredSlotDto", id)
      StoredSlotDto(id, filmId, slotKey, slot, updatedAt, listingKey)
    }
  }
}
