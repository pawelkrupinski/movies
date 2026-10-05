package services.identity

import org.bson.{BsonInvalidOperationException, BsonReader, BsonType}

/** Reading a stored identity document field by field off a [[BsonReader]] — the wire bytes of a reply, or a
 *  `BsonDocumentReader` over a document in hand — straight into the model's types, with no tree of maps between:
 *  decoded as `BsonDocument`s first, a US take-up's families were ~400k maps and ~1M key strings, all garbage the
 *  moment the families were built from them. */
private[identity] object BsonFields {
  /** The fields of the document the reader is at: `field` reads each value by name, or skips it. */
  def document(reader: BsonReader)(field: String => Unit): Unit = {
    reader.readStartDocument()
    while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) field(reader.readName())
    reader.readEndDocument()
  }

  /** The elements of the array the reader is at, each read by `element`. */
  def array(reader: BsonReader)(element: => Unit): Unit = {
    reader.readStartArray()
    while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) element
    reader.readEndArray()
  }

  def strings(reader: BsonReader): Vector[String] = { val all = Vector.newBuilder[String]; array(reader)(all += reader.readString()); all.result() }
  def stringSet(reader: BsonReader): Set[String]  = { val all = Set.newBuilder[String]; array(reader)(all += reader.readString()); all.result() }
  def intSet(reader: BsonReader): Set[Int]        = { val all = Set.newBuilder[Int]; array(reader)(all += reader.readInt32()); all.result() }

  /** A number stored as any BSON number: a copier that round-trips through JavaScript numbers (mongosh, the local
   *  mirror) writes a whole-number double such as 1.0 back as an Int32. */
  def number(reader: BsonReader, name: String): java.lang.Double = reader.getCurrentBsonType match {
    case BsonType.DOUBLE     => reader.readDouble()
    case BsonType.INT32      => reader.readInt32().toDouble
    case BsonType.INT64      => reader.readInt64().toDouble
    case BsonType.DECIMAL128 => reader.readDecimal128().doubleValue
    case other               => throw new BsonInvalidOperationException(s"$name: expected a number, found $other")
  }

  /** The value when it is of `kind`, else none — the value skipped. */
  def when[A](reader: BsonReader, kind: BsonType)(read: => A): Option[A] =
    if (reader.getCurrentBsonType == kind) Some(read) else { reader.skipValue(); None }

  /** `value`, which a document must hold: absent, it is no document of this kind. */
  def required[A <: AnyRef](value: A, name: String): A =
    if (value == null) throw new BsonInvalidOperationException(s"no '$name' field") else value
}
