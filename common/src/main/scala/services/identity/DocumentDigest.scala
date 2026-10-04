package services.identity

import org.mongodb.scala.bson.{BsonDocument, BsonInt64}

/** A stored document's content digest, which a rewrite of an unchanged document is skipped by: the model's families
 *  (`MongoIdentityModelStore`) and the traces (`MongoIdentityTraceStore`) both keep one. In a file of its own so the
 *  model store, which the rules version digests, does not reach the trace store, which changes no decision. */
private[identity] object DocumentDigest {
  /** `doc` with its content digest under `field`: 64 bits from its canonical JSON, two MurmurHash3 seeds. */
  def of(doc: BsonDocument, field: String): BsonDocument = {
    val json = doc.toJson
    val high = scala.util.hashing.MurmurHash3.stringHash(json, 0x2f1d7a3b).toLong
    val low  = scala.util.hashing.MurmurHash3.stringHash(json, 0x6c8e9cf5).toLong & 0xffffffffL
    doc.clone().append(field, BsonInt64((high << 32) | low))
  }
}
