package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonDocument, BsonInt64}
import org.mongodb.scala.model.{Filters, Projections, WriteModel}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

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

  /** Writes those of `docs` (id → document, digested under `field`) whose digest `collection` does not hold already,
   *  each as `write` models it; how many it wrote. Unguarded by design: each store has one writer per country (the
   *  model's update), the read only decides which documents moved, and a document another process wrote meanwhile is
   *  rewritten whole by the next update. */
  def writeMoved(collection: MongoCollection[Document], docs: Seq[(String, BsonDocument)], field: String, timeout: FiniteDuration)
                (write: (String, BsonDocument) => WriteModel[? <: Document]): Int = {
    val stored = Await.result(collection.find(Filters.in("_id", docs.map(_._1)*)).projection(Projections.include(field))
      .batchSize(tools.MongoReplies.Default).toFuture(), timeout)
      .flatMap(d => d.get("_id").map(_.asString.getValue -> d.get(field).flatMap(BsonFields.int64))).toMap
    val moved  = docs.filterNot { case (id, doc) => stored.get(id).flatten.contains(doc.getInt64(field).getValue) }
    if (moved.nonEmpty) Await.result(collection.bulkWrite(moved.map(write.tupled)).toFuture(), timeout)
    moved.size
  }
}
