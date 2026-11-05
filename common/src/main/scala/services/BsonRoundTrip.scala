package services

import org.bson.codecs.{BsonDocumentCodec, Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter, RawBsonDocument}
import org.mongodb.scala.bson.collection.immutable.Document

/**
 * A value as a Mongo store would hand it back: encoded by the store's own codec, written to BSON bytes as the driver
 * writes it, and decoded again. THE BASE every in-memory stand-in for a Mongo-backed store writes through, so a spec
 * against it reads what Mongo would hold — and fails where Mongo would refuse.
 *
 * Why: an in-memory store that keeps the value it was handed never encodes, so every unit spec passed while Mongo
 * refused each agreement verdict whose receipt named a field with a NUL in it (a NUL ends a BSON cstring,
 * 2026-10-25). The bytes step is the load-bearing one: a [[BsonDocument]] or a Scala [[Document]] holds such a field
 * name happily, only writing it out throws. It also drops what BSON cannot carry: an `Instant`'s sub-millisecond part,
 * a field the codec leaves out. `InMemoryStoresRoundTripSpec` fails on an in-memory stand-in that does not use it.
 */
object BsonRoundTrip {

  private val Documents = new BsonDocumentCodec()

  /** `document` written to BSON bytes and read back; THROWS what the driver's write would (a NUL in a field name). */
  def apply(document: BsonDocument): BsonDocument = read(bytes(document))

  /** The write half: `document` as the BSON bytes the driver would write; THROWS what that write would. For a store that
   *  keeps the bytes and decodes each read from them ([[read]]), as one holding a replay's every answer does. */
  def bytes(document: BsonDocument): RawBsonDocument = new RawBsonDocument(document, Documents)

  /** The read half: a fresh document decoded from `bytes`, its reader's own copy, as a driver read's is. */
  def read(bytes: RawBsonDocument): BsonDocument = bytes.decode(Documents)

  /** `document` written to BSON bytes and read back. */
  def apply(document: Document): Document = Document(apply(document.toBsonDocument))

  /** `value` through `encode`, the bytes, and `decode`: a store whose codec is a pair of functions. */
  def via[T](encode: T => BsonDocument, decode: BsonDocument => T)(value: T): T = decode(apply(encode(value)))

  /** `value` through a driver [[Codec]], the bytes, and back: a store whose collection is typed by a codec registry. */
  def throughCodec[T](codec: Codec[T])(value: T): T = {
    val document = new BsonDocument()
    codec.encode(new BsonDocumentWriter(document), value, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(apply(document)), DecoderContext.builder().build())
  }
}
