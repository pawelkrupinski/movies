package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MongoStoreDocumentsBulkSpec extends AnyFlatSpec with Matchers {

  "MongoStoreDocuments.onlyAlreadyHeld" should "read duplicate-key refusals as already held, but not a write-concern failure" in {
    import com.mongodb.{MongoBulkWriteException, ServerAddress}
    import com.mongodb.bulk.{BulkWriteError, BulkWriteResult, WriteConcernError}
    import org.bson.BsonDocument
    import java.util.Collections
    def bulk(errors: java.util.List[BulkWriteError], concern: WriteConcernError) =
      new MongoBulkWriteException(BulkWriteResult.unacknowledged(), errors, concern, new ServerAddress(), Collections.emptySet[String]())
    val duplicate = new BulkWriteError(11000, "E11000 duplicate key", new BsonDocument(), 0)
    MongoStoreDocuments.onlyAlreadyHeld(bulk(java.util.List.of(duplicate), null)) shouldBe true
    MongoStoreDocuments.onlyAlreadyHeld(bulk(java.util.List.of(new BulkWriteError(121, "validation", new BsonDocument(), 0)), null)) shouldBe false
    MongoStoreDocuments.onlyAlreadyHeld(
      bulk(Collections.emptyList(), new WriteConcernError(64, "WriteConcernFailed", "waiting for replication timed out", new BsonDocument()))) shouldBe false
  }
}
