package services.movies

import com.mongodb.{MongoCommandException, MongoSocketReadTimeoutException, ServerAddress}
import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `isInvalid` decides whether a persisted resume token must be THROWN AWAY. Getting it
 * wrong is silent and total: the token is kept, every reopen resumes from it, the server
 * rejects it the same way, and the change stream never comes back — which is exactly what
 * happened to all three workers after the 2026-08-29 Mongo migration.
 *
 * The payloads below are the real ones from that incident (`kinowo.movies`) and from the
 * oplog-window case, trimmed to the fields the predicate reads.
 */
class ChangeStreamResumeTokenSpec extends AnyFlatSpec with Matchers {

  private def commandFailure(code: Int, codeName: String, errmsg: String, labels: Seq[String]): MongoCommandException = {
    val response = new BsonDocument()
      .append("ok", new BsonInt32(0))
      .append("code", new BsonInt32(code))
      .append("codeName", new BsonString(codeName))
      .append("errmsg", new BsonString(errmsg))
    if (labels.nonEmpty) {
      val array = new BsonArray()
      labels.foreach(l => array.add(new BsonString(l)))
      response.append("errorLabels", array)
    }
    new MongoCommandException(response, new ServerAddress("10.20.0.10", 27017))
  }

  // THE MIGRATION SHAPE. A dump-and-restore drops every collection, so a token persisted
  // before the restore points into an oplog the restored collection no longer shares. The
  // server answers 280 / NonResumableChangeStreamError — NOT the 286 the predicate used to
  // be written around — and resuming from that token can only ever fail again.
  "isInvalid" should "reject a token the server says was not found (ChangeStreamFatalError 280)" in {
    val e = commandFailure(280, "ChangeStreamFatalError",
      "Executor error during aggregate command on namespace: kinowo.movies :: caused by :: " +
        "cannot resume stream; the resume token was not found. {_data: \"826A92A31D00000103\"}",
      Seq("NonResumableChangeStreamError"))
    ChangeStreamResumeToken.isInvalid(e) shouldBe true
  }

  it should "reject anything the driver labels NonResumableChangeStreamError, whatever the code" in {
    val e = commandFailure(1234, "SomeFutureChangeStreamError", "the cursor cannot continue",
      Seq("NonResumableChangeStreamError"))
    ChangeStreamResumeToken.isInvalid(e) shouldBe true
  }

  // Found by the integration test that drops the collection: a drop invalidates the cursor,
  // the invalidate event's token is what got saved, and the server refuses to resume from it
  // — with NO error label, so only the code and message identify it.
  it should "reject the token an invalidate notification left behind (InvalidResumeToken 260)" in {
    val e = commandFailure(260, "InvalidResumeToken",
      "Attempting to resume a change stream using 'resumeAfter' is not allowed from an invalidate notification", Nil)
    ChangeStreamResumeToken.isInvalid(e) shouldBe true
  }

  it should "reject a token that fell out of the oplog window (ChangeStreamHistoryLost 286)" in {
    val e = commandFailure(286, "ChangeStreamHistoryLost",
      "Resume of change stream was not possible, as the resume point may no longer be in the oplog.", Nil)
    ChangeStreamResumeToken.isInvalid(e) shouldBe true
  }

  // The other half of the contract: a token must NOT be discarded for a transient failure.
  // Clearing it there would silently skip every event that landed during the blip, since
  // the next open would start at "now" instead of replaying from the saved position.
  it should "keep the token across a transient blip" in {
    val timeout = new MongoSocketReadTimeoutException(
      "Timeout while receiving message", new ServerAddress("10.20.0.10", 27017), new java.io.IOException("read timed out"))
    ChangeStreamResumeToken.isInvalid(timeout) shouldBe false
    ChangeStreamResumeToken.isInvalid(commandFailure(11602, "InterruptedDueToReplStateChange",
      "operation was interrupted", Nil)) shouldBe false
    ChangeStreamResumeToken.isInvalid(new RuntimeException("connection reset")) shouldBe false
  }

  // THE GENERATION GUARD. A position is only advanced once its event is APPLIED, on the
  // apply thread — so an event queued before an invalid-token `clear()` can finish after it.
  // Letting it advance would re-arm the very token the clear threw away, and the next open
  // would resume from it and fail the same way.
  "advance" should "ignore a position captured before a clear()" in {
    val token = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val stale = token.generation
    token.clear()

    token.advance(new BsonDocument("_data", new BsonString("before-clear")), stale)
    token.current shouldBe None

    token.advance(new BsonDocument("_data", new BsonString("after-clear")), token.generation)
    token.current shouldBe Some(new BsonDocument("_data", new BsonString("after-clear")))
  }
}
