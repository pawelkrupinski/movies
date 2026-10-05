package tools

import org.bson.{BsonReader, BsonValue, BsonWriter}
import org.bson.codecs.{CollectibleCodec, DecoderContext, EncoderContext, RawBsonDocumentCodec}
import org.bson.codecs.configuration.CodecRegistries
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.bson.codecs.ImmutableDocumentCodec
import org.mongodb.scala.bson.collection.immutable.Document

/**
 * A database view whose Scala `Document`s are read as the bytes they arrived in (`org.bson.RawBsonDocument`), not as a tree of
 * maps: a field is decoded when it is asked for, and an embedded document or array shares its parent's bytes. Writes
 * are unchanged.
 *
 * For a store that reads large documents once and keeps only what it decodes from them. Decoded as a tree, every
 * embedded document is a `LinkedHashMap` with a key string per field: worker-us's identity families (~2,100 documents,
 * 53 MB of BSON) came to ~400k maps and ~2M strings at each boot's take-up, alive long enough beside the boot's other
 * reads to be promoted, and then dropped once the families were decoded (2026-10-05).
 */
object RawDocuments {
  def over(database: MongoDatabase): MongoDatabase =
    database.withCodecRegistry(CodecRegistries.fromRegistries(CodecRegistries.fromCodecs(RawDocumentCodec), database.codecRegistry))

  private[tools] object RawDocumentCodec extends CollectibleCodec[Document] {
    private val raw   = new RawBsonDocumentCodec
    private val whole = ImmutableDocumentCodec()

    def decode(reader: BsonReader, context: DecoderContext): Document = new Document(raw.decode(reader, context))
    def encode(writer: BsonWriter, value: Document, context: EncoderContext): Unit = whole.encode(writer, value, context)
    def getEncoderClass: Class[Document] = classOf[Document]

    def generateIdIfAbsentFromDocument(document: Document): Document = whole.generateIdIfAbsentFromDocument(document)
    def documentHasId(document: Document): Boolean = whole.documentHasId(document)
    def getDocumentId(document: Document): BsonValue = whole.getDocumentId(document)
  }
}
