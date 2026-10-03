package integration

import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ChangeStreamResumeToken
import tools.ReadOutcome

/**
 * A saved resume position read back against real MongoDB — and a position that could not be read
 * told apart from one never saved. The load answered `None` for both, so a worker that could not
 * reach the token collection at boot opened its cursor at "now" exactly as on a first-ever start,
 * skipping every change since the last save without a word.
 *
 * Runs in a database of its own, dropped in `afterAll`.
 */
class ChangeStreamResumeTokenIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "resume-token")
  // Nothing listens on port 1: every read fails once server selection gives up.
  private val unreachable = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")

  override protected def afterAll(): Unit = try { unreachable.close(); isolated.drop() } finally super.afterAll()

  private val token = new BsonDocument("_data", new BsonString("826A92A31D00000103"))

  "ChangeStreamResumeToken.load" should "answer absent when no position was ever saved" in {
    new ChangeStreamResumeToken("never-saved", Some(isolated.database), enabled = true).load() shouldBe a[ReadOutcome.Absent]
  }

  it should "answer the position a save persisted" in {
    val saved = new ChangeStreamResumeToken("saved", Some(isolated.database), enabled = true)
    saved.advance(token, saved.generation)
    saved.save(force = true)
    new ChangeStreamResumeToken("saved", Some(isolated.database), enabled = true).load() shouldBe ReadOutcome.Answered(token)
  }

  it should "answer FAILED, not absent, when the position cannot be read" in {
    val blind = new ChangeStreamResumeToken("blind", Some(unreachable.getDatabase("resume-token")), enabled = true)
    blind.load() shouldBe a[ReadOutcome.Failed]
    // …and still opens, at now: a cursor that never opens is worse than one that skips.
    blind.openFrom() shouldBe None
  }
}
