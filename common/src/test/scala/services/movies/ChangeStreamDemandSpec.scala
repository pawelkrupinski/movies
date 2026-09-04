package services.movies

import org.mongodb.scala.Subscription
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/**
 * The backpressure that replaced `request(Long.MaxValue)` on both change-stream
 * cursors. Before it, the driver was told to deliver everything it could read while the
 * apply it feeds is one thread doing a blocking Mongo read per event — so the queue
 * between them was bounded only by heap.
 */
class ChangeStreamDemandSpec extends AnyFlatSpec with Matchers {

  /** Records what the cursor was asked for. `unsubscribe` is what a real close does. */
  private final class RecordingSubscription extends Subscription {
    val requested = mutable.Buffer.empty[Long]
    private var cancelled = false
    override def request(n: Long): Unit = requested += n
    override def unsubscribe(): Unit    = cancelled = true
    override def isUnsubscribed: Boolean = cancelled
  }

  "ChangeStreamDemand" should "prime the cursor with exactly one window, not everything" in {
    val sub = new RecordingSubscription
    new ChangeStreamDemand(4).opened(sub)

    sub.requested shouldBe Seq(4L)
    // The regression this class exists for: the old code asked for Long.MaxValue here.
    sub.requested should not contain Long.MaxValue
  }

  it should "release demand for exactly one more event per apply" in {
    val sub    = new RecordingSubscription
    val demand = new ChangeStreamDemand(4)
    demand.opened(sub)

    (1 to 3).foreach(_ => demand.applied())

    sub.requested shouldBe Seq(4L, 1L, 1L, 1L)
  }

  it should "never let outstanding demand exceed the window" in {
    val sub    = new RecordingSubscription
    val demand = new ChangeStreamDemand(8)
    demand.opened(sub)
    (1 to 50).foreach(_ => demand.applied())

    // Total requested = window + one per applied event, so delivered-but-unapplied
    // can never exceed the window however long the stream runs.
    val totalRequested = sub.requested.sum
    val applied        = 50L
    (totalRequested - applied) shouldBe 8L
  }

  it should "drop demand released after the cursor died instead of aiming it at a dead subscription" in {
    val sub    = new RecordingSubscription
    val demand = new ChangeStreamDemand(4)
    demand.opened(sub)
    demand.closed()

    demand.applied() // an apply that was still in flight when onError fired

    sub.requested shouldBe Seq(4L)
  }

  it should "prime a full window again when the cursor is reopened" in {
    val first  = new RecordingSubscription
    val second = new RecordingSubscription
    val demand = new ChangeStreamDemand(4)

    demand.opened(first)
    demand.applied()
    demand.closed()
    demand.opened(second)

    first.requested shouldBe Seq(4L, 1L)
    second.requested shouldBe Seq(4L)
  }

  it should "refuse a non-positive window rather than silently stall the stream" in {
    an[IllegalArgumentException] should be thrownBy new ChangeStreamDemand(0)
    an[IllegalArgumentException] should be thrownBy new ChangeStreamDemand(-1)
  }

  "ChangeStreamDemand.unbounded" should "exist for the impls that have no backlog to bound" in {
    val sub = new RecordingSubscription
    ChangeStreamDemand.unbounded.opened(sub)
    sub.requested shouldBe Seq(Int.MaxValue.toLong)
  }
}
