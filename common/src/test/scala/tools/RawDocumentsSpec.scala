package tools

import org.bson.{BsonDocumentReader, BsonDocumentWriter, RawBsonDocument}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt64, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RawDocumentsSpec extends AnyFlatSpec with Matchers {
  private val stored = BsonDocument("_id" -> BsonString("family"), "digest" -> BsonInt64(7L),
    "reads" -> BsonDocument("titles" -> BsonArray(BsonString("a"), BsonString("b"))),
    "nodes" -> BsonArray(BsonDocument("listing" -> BsonDocument("venue" -> BsonString("v")), "node" -> BsonString("n"))))

  private def read(document: org.bson.BsonDocument): Document =
    RawDocuments.RawDocumentCodec.decode(new BsonDocumentReader(document), DecoderContext.builder().build())

  // Decoded as a tree, every embedded document is a map with a key string per field, all of it alive until the reader
  // has decoded what it keeps — at a US boot's take-up, ~400k of them, promoted and then dropped.
  "a document read raw" should "keep its bytes, decoding an embedded document only when asked, as bytes too" in {
    val document = read(stored)
    document.toBsonDocument shouldBe a[RawBsonDocument]
    document.toBsonDocument.getDocument("reads") shouldBe a[RawBsonDocument]
    document.toBsonDocument.getArray("nodes").get(0).asDocument shouldBe a[RawBsonDocument]
  }

  it should "read as the document it was" in {
    read(stored).toBsonDocument shouldBe stored
    read(stored).get[BsonString]("_id").map(_.getValue) shouldBe Some("family")
  }

  it should "write as the document it is" in {
    val written = new org.bson.BsonDocument()
    RawDocuments.RawDocumentCodec.encode(new BsonDocumentWriter(written), Document(stored), EncoderContext.builder().build())
    written shouldBe stored
    RawDocuments.RawDocumentCodec.documentHasId(Document(stored)) shouldBe true
  }
}
