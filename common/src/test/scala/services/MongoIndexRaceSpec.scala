package services

import com.mongodb.{MongoCommandException, ServerAddress}
import org.bson.{BsonDocument, BsonInt32, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The retry of a unique conversion two booting pods raced on: paused between attempts, growing,
 *  so the attempts outlast the other pod's conversion rather than all landing inside it. */
class MongoIndexRaceSpec extends AnyFlatSpec with Matchers {

  private def raced(code: Int) = new MongoCommandException(
    new BsonDocument("ok", new BsonInt32(0)).append("code", new BsonInt32(code)).append("errmsg", new BsonString("raced")),
    new ServerAddress())

  "a raced unique conversion" should "pause, longer each time, between its attempts" in {
    val pauses = scala.collection.mutable.ArrayBuffer.empty[Long]
    var calls  = 0
    val result = MongoIndex.retryRaced(attempts = 4, pauses += _)(() => { calls += 1; if (calls < 3) throw raced(125) }, () => false)
    result.isSuccess shouldBe true
    calls shouldBe 3
    pauses.toSeq shouldBe Seq(MongoIndex.RaceBackoffMillis, MongoIndex.RaceBackoffMillis * 2)
  }

  it should "give up after its last attempt without pausing again, and succeed at once when the other pod won" in {
    val pauses = scala.collection.mutable.ArrayBuffer.empty[Long]
    MongoIndex.retryRaced(attempts = 3, pauses += _)(() => throw raced(72), () => false).isFailure shouldBe true
    pauses should have size 2
    pauses.clear()
    MongoIndex.retryRaced(attempts = 3, pauses += _)(() => throw raced(125), () => true).isSuccess shouldBe true
    pauses shouldBe empty
  }

  it should "not retry a failure that is not a race" in {
    var calls = 0
    MongoIndex.retryRaced(attempts = 3, _ => fail("paused"))(() => { calls += 1; throw raced(359) }, () => false).isFailure shouldBe true
    calls shouldBe 1
  }

  // The rollback after a conversion the duplicates refused was a bare `Try`: had it failed, the
  // plain index kept `prepareUnique` and began refusing duplicate writes with nothing said.
  "a refused unique conversion's rollback" should "name its own failure in the reason, and leave a clean one alone" in {
    MongoIndex.afterRollback("2 key value(s) are held by 4 documents", () => ()) shouldBe "2 key value(s) are held by 4 documents"
    val failed = MongoIndex.afterRollback("2 key value(s) are held by 4 documents", () => throw raced(13))
    failed should startWith("2 key value(s) are held by 4 documents, and clearing prepareUnique failed")
    failed should include("REFUSES new duplicate writes")
  }
}
