package services.movies

import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A throttled resume-token save is fire-and-forget, and a clear (an invalid token) deleted the stored one at once: a
 *  save still in flight could land after the delete and store the invalid token again, so the next open failed on it
 *  once more. */
class ResumeTokenClearOrderSpec extends AnyFlatSpec with Matchers {

  "ChangeStreamResumeToken.clear" should "delete the stored position only after a save still in flight has landed" in {
    val inFlight      = scala.concurrent.Promise[Unit]()
    val landedAtClear = new java.util.concurrent.atomic.AtomicReference[Option[Boolean]](None)
    val store = new ResumeTokenStore {
      def load(streamId: String): tools.ReadOutcome[BsonDocument] = tools.ReadOutcome.none(streamId)
      def save(streamId: String, token: BsonDocument, synchronously: Boolean): scala.concurrent.Future[Unit] = inFlight.future
      def clear(streamId: String): Unit = landedAtClear.set(Some(inFlight.isCompleted))
    }
    val token = new ChangeStreamResumeToken("movies", store, stopwatch = new _root_.tools.Stopwatch(() => 0L))
    token.advance(new BsonDocument("_data", new BsonString("82A1")), token.generation)
    token.save(force = false)

    // The save lands once the clearing thread is waiting for it -- or never, if the clear does not wait.
    val clearing = Thread.currentThread()
    val lander = new Thread(() => {
      if (_root_.tools.Eventually.poll(_root_.tools.SpecTimeouts.Io.toMillis, pollMs = 5)(
            landedAtClear.get.isDefined || clearing.getState == Thread.State.TIMED_WAITING))
        inFlight.trySuccess(())
      ()
    })
    lander.setDaemon(true); lander.start()
    token.clear()

    landedAtClear.get shouldBe Some(true)
  }
}
