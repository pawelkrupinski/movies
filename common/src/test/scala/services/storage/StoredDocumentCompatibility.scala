package services.storage

import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The harness behind the storage-compatibility specs: encode a document, then take one
 * field away at a time and see whether it still decodes.
 *
 * WHY THIS EXISTS AS ITS OWN KIND OF TEST. A Mongo document in this repo outlives the
 * class that describes it. Fields get added, `$unset` by migrations, or were simply never
 * written by an older build — so at any moment the collection holds documents that do not
 * match the current case class. The driver's macro codec fails a MISSING non-`Option`
 * field with `BsonInvalidOperationException: Missing field: x`, and that failure is not
 * local: it kills the whole keyset batch, so one migrated document takes the entire corpus
 * scan down with it. Round-trip specs never see this, because they only ever decode what
 * they just encoded.
 *
 * It has now cost two outages. `web_movies.ratings` made the served corpus 404, and
 * `movies.sourceData` — `$unset` by the 2026-07-27 slot migration — broke the corpus scan
 * and aborted every staging fold for hours behind a WARN. Both were one word (`Option`)
 * away, and both were invisible until prod.
 *
 * So the rule is checked mechanically, for every persisted document type, instead of
 * remembered: every field must decode when ABSENT, unless it is named in the spec's
 * `required` set with a reason. Adding a genuinely-required field is then a deliberate
 * act with a written justification, which is the review conversation this needs to force.
 */
object StoredDocumentCompatibility {

  /** One field's verdict: the field name, and the decode failure it caused (if any). */
  final case class Missing(field: String, error: String)

  /** Encode `value`, then for each TOP-LEVEL field in turn remove it and try to decode
   *  what's left. Returns one [[Missing]] per field whose absence breaks the decode.
   *
   *  Top-level only, deliberately: that is the granularity a `$unset` and a schema
   *  addition operate at, and it keeps the failure message pointing at a field a human
   *  can act on. */
  def fieldsThatBreakDecode[A](codec: Codec[A], value: A): Seq[Missing] = {
    val full = encode(codec, value)
    full.keySet().asScala.toSeq.sorted.flatMap { field =>
      val without = full.clone()
      without.remove(field)
      Try(decode(codec, without)) match {
        case scala.util.Success(_) => None
        case scala.util.Failure(e) => Some(Missing(field, s"${e.getClass.getSimpleName}: ${e.getMessage}"))
      }
    }
  }

  /** The field names a document of this type actually carries when fully populated — so a
   *  spec can prove its sample exercises every field rather than silently drifting behind
   *  the case class as fields are added. */
  def encodedFields[A](codec: Codec[A], value: A): Set[String] =
    encode(codec, value).keySet().asScala.toSet

  private def encode[A](codec: Codec[A], value: A): BsonDocument = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), value, EncoderContext.builder().build())
    out
  }

  private def decode[A](codec: Codec[A], document: BsonDocument): A =
    codec.decode(new BsonDocumentReader(document), DecoderContext.builder().build())
}
