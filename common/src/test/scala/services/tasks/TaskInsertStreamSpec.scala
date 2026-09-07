package services.tasks

import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.{Document, Observer, Subscription}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.RecordingSchedule

import scala.collection.mutable
import scala.concurrent.duration._

/**
 * The `tasks` insert cursor that wakes the worker pool. A change stream's `onError` is
 * TERMINAL — the driver never brings the cursor back — and until this driver existed
 * `watchWaiting` only logged the death, so one blip left every worker on this node
 * deaf to new tasks (parked on the 30s backstop) until the process restarted.
 */
class TaskInsertStreamSpec extends AnyFlatSpec with Matchers {

  /** Keeps every observer handed to `open`, so the spec can push events and deaths. */
  private final class HandFedCursor {
    val observers = mutable.Buffer.empty[Observer[ChangeStreamDocument[Document]]]
    val unsubscribed = mutable.Buffer.empty[Int]
    val open: Observer[ChangeStreamDocument[Document]] => Unit = { o =>
      val index = observers.size
      observers += o
      o.onSubscribe(new Subscription {
        override def request(n: Long): Unit  = ()
        override def unsubscribe(): Unit     = unsubscribed += index
        override def isUnsubscribed: Boolean = unsubscribed.contains(index)
      })
    }
    def current: Observer[ChangeStreamDocument[Document]] = observers.last
  }

  private def event(op: String) =
    new ChangeStreamDocument[Document](op, new BsonDocument("_data", new BsonString("token")),
      null, null, null, null, null, new BsonDocument("_id", new BsonString("t1")),
      null, null, null, null, null, null, null)

  private def stream(cursor: HandFedCursor, clock: RecordingSchedule, rings: mutable.Buffer[Int] = mutable.Buffer.empty) = {
    val s = new TaskInsertStream(cursor.open, () => rings += rings.size, clock.schedule)
    s.start()
    s
  }

  "TaskInsertStream" should "ring the doorbell once per insert and ignore other ops" in {
    val cursor = new HandFedCursor; val clock = new RecordingSchedule
    val rings = mutable.Buffer.empty[Int]
    stream(cursor, clock, rings)
    cursor.current.onNext(event("insert"))
    cursor.current.onNext(event("update"))
    cursor.current.onNext(event("insert"))
    rings should have size 2
  }

  it should "reopen the cursor after a terminal error, on the reopen backoff" in {
    val cursor = new HandFedCursor; val clock = new RecordingSchedule
    stream(cursor, clock)
    cursor.current.onError(new RuntimeException("primary stepped down"))
    cursor.observers should have size 1 // scheduled, not immediate
    clock.delays shouldBe Seq(1.second)
    clock.fire()
    cursor.observers should have size 2
    // The fresh cursor is live: its inserts ring again.
  }

  it should "reopen after the cursor completes, too" in {
    val cursor = new HandFedCursor; val clock = new RecordingSchedule
    stream(cursor, clock)
    cursor.current.onComplete()
    clock.fire()
    cursor.observers should have size 2
  }

  it should "back off across repeated deaths and reset once an event is delivered" in {
    val cursor = new HandFedCursor; val clock = new RecordingSchedule
    stream(cursor, clock)
    cursor.current.onError(new RuntimeException("1")); clock.fire()
    cursor.current.onError(new RuntimeException("2")); clock.fire()
    cursor.current.onNext(event("insert")) // proves the cursor healthy
    cursor.current.onError(new RuntimeException("3")); clock.fire()
    clock.delays shouldBe Seq(1.second, 5.seconds, 1.second)
    cursor.observers should have size 4
  }

  it should "unsubscribe and stop reopening once closed" in {
    val cursor = new HandFedCursor; val clock = new RecordingSchedule
    val s = stream(cursor, clock)
    s.close()
    cursor.unsubscribed shouldBe Seq(0)
    cursor.current.onError(new RuntimeException("after close"))
    clock.fire()
    cursor.observers should have size 1
  }
}
