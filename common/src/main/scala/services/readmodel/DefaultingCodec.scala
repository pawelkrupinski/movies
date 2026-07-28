package services.readmodel

import org.bson.codecs.{BsonDocumentCodec, Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter, BsonReader, BsonWriter}

/**
 * Decodes a stored document that is MISSING fields, by filling them from a template of the
 * same type before handing it to the real codec. Encoding is untouched.
 *
 * WHY. The driver's macro codec fails a missing non-`Option` field with
 * `BsonInvalidOperationException: Missing field: x`, and that failure is not local — it
 * kills the whole keyset batch, so ONE old row takes an entire collection scan down with
 * it. That has cost two outages: `web_movies.ratings` 404'd the served corpus, and
 * `movies.sourceData` — `$unset` by a migration — broke the corpus scan and silently
 * aborted every staging fold for hours.
 *
 * The read model is the sharpest exposure left, because full re-projection is RETIRED: a
 * quiescent row is rewritten only when its film changes, so a newly-added field breaks
 * every row nobody has touched, indefinitely.
 *
 * The template is built by ENCODING an empty instance, so the defaults are whatever that
 * type's own empty value serialises to — no per-field decode logic to hand-write and drift
 * (the hand-written `BackwardCompatibleSourceDataCodec` is the warning here). Adding a
 * field to the case class extends the template automatically.
 *
 * A field the template also lacks (an `Option` that encodes to nothing under
 * `IgnoreNone`) needs no default: the macro codec already reads its absence as `None`.
 */
final class DefaultingCodec[A](inner: Codec[A], template: BsonDocument) extends Codec[A] {

  override def getEncoderClass: Class[A] = inner.getEncoderClass

  /** Unchanged — this only makes READS tolerant. Writing still emits the full shape, so a
   *  row this process writes is never the degraded one. */
  override def encode(writer: BsonWriter, value: A, context: EncoderContext): Unit =
    inner.encode(writer, value, context)

  override def decode(reader: BsonReader, context: DecoderContext): A = {
    val document = new BsonDocumentCodec().decode(reader, context)
    template.forEach { (field, default) =>
      if (!document.containsKey(field)) document.put(field, default)
      ()
    }
    inner.decode(new BsonDocumentReader(document), context)
  }
}

object DefaultingCodec {

  /** Wrap `inner` so any field absent from a stored document is filled from `empty`'s
   *  encoded form. */
  def apply[A](inner: Codec[A], empty: A): Codec[A] = {
    val template = new BsonDocument()
    inner.encode(new BsonDocumentWriter(template), empty, EncoderContext.builder().build())
    new DefaultingCodec(inner, template)
  }
}
