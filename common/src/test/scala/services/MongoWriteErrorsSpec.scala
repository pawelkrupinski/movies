package services

import com.mongodb.{MongoCommandException, MongoWriteException, ServerAddress, WriteError}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.bson.{BsonDocument, BsonInt32, BsonSerializationException, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

/** A failed Mongo write is counted by what failed it, a refused grant on its own counter, and the expected duplicate
 *  key, a timeout or a closing client not at all. */
class MongoWriteErrorsSpec extends AnyFlatSpec with Matchers {

  private def writeError(code: Int) =
    new MongoWriteException(new WriteError(code, s"E$code", new BsonDocument()), new ServerAddress(), java.util.Collections.emptySet[String]())

  private def commandError(code: Int, message: String) =
    new MongoCommandException(new BsonDocument("ok", new BsonInt32(0)).append("code", new BsonInt32(code))
      .append("errmsg", new BsonString(message)), new ServerAddress())

  "a failed write" should "count as the kind of failure it is" in {
    MongoWriteErrors.kindOf(new BsonSerializationException("BSON cstring 'tmdb|candidates|a\u0000Pat' is not valid because it contains a null character")) shouldBe
      Some(MongoWriteErrors.Kind.Serialization)
    MongoWriteErrors.kindOf(new IllegalStateException("bulk", writeError(121))) shouldBe Some(MongoWriteErrors.Kind.Rejected)
    MongoWriteErrors.kindOf(commandError(10107, "not primary")) shouldBe Some(MongoWriteErrors.Kind.Failed)
  }

  it should "not count a duplicate key, a timeout or a client closing under a shutdown" in {
    MongoWriteErrors.kindOf(writeError(MongoWriteErrors.DuplicateKeyCode)) shouldBe None
    MongoWriteErrors.kindOf(new java.util.concurrent.TimeoutException("Future timed out after [10 seconds]")) shouldBe None
    MongoWriteErrors.kindOf(new IllegalStateException("state should be: open")) shouldBe None
  }

  "a refused grant" should "be told from every other command failure" in {
    MongoWriteErrors.isUnauthorized(commandError(MongoWriteErrors.UnauthorizedCode, "not authorized on kinowo_es to execute command { insert: \"tasks\" }")) shouldBe true
    MongoWriteErrors.isUnauthorized(commandError(10107, "not primary")) shouldBe false
  }

  "a counted write" should "rethrow its failure, counted under its database and collection, every collection seeded at 0" in {
    val registry = new PrometheusRegistry()
    val series   = MongoWriteErrors.Series.worker(registry)
    val failure  = new BsonSerializationException("a null character")
    series.touched("kinowo_spec", "quiet")
    the[BsonSerializationException] thrownBy series.counting("kinowo_spec", "identity_agreements")(throw failure) shouldBe failure
    a[MongoCommandException] should be thrownBy series.counting("kinowo_spec", "tasks")(
      throw commandError(MongoWriteErrors.UnauthorizedCode, "not authorized on kinowo_spec"))
    val counts = registry.scrape().asScala.flatMap(family => family.getDataPoints.asScala.map { point =>
      val labels = point.getLabels.asScala.map(l => l.getName -> l.getValue).toMap
      (family.getMetadata.getPrometheusName, labels.get("collection").orElse(labels.get("database")).getOrElse(""), labels.getOrElse("kind", "")) ->
        point.asInstanceOf[io.prometheus.metrics.model.snapshots.CounterSnapshot.CounterDataPointSnapshot].getValue
    }).toMap
    counts(("kinowo_worker_mongo_write_errors", "identity_agreements", "serialization")) shouldBe 1.0
    counts(("kinowo_worker_mongo_write_errors", "identity_agreements", "rejected")) shouldBe 0.0
    counts(("kinowo_worker_mongo_write_errors", "quiet", "failed")) shouldBe 0.0
    counts(("kinowo_worker_mongo_unauthorized", "kinowo_spec", "")) shouldBe 1.0
  }
}
