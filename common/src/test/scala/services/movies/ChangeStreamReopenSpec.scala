package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.duration._

/**
 * The driver that brings a dead change-stream cursor back. Before it existed, `onError`
 * only cleared the subscription and logged that "a reopen resumes from the persisted
 * token" — with nothing anywhere that performed one.
 */
class ChangeStreamReopenSpec extends AnyFlatSpec with Matchers {

  /** Records what was scheduled instead of sleeping; `fire()` runs the pending body. */
  private final class Clock {
    val delays = mutable.Buffer.empty[FiniteDuration]
    private val queued = mutable.Queue.empty[() => Unit]
    val schedule: (FiniteDuration, () => Unit) => Unit = (d, run) => { delays += d; queued.enqueue(run) }
    def fire(): Unit = while (queued.nonEmpty) queued.dequeue().apply()
  }

  private def driver(clock: Clock, opens: mutable.Buffer[Int], delays: Seq[FiniteDuration] = Seq(1.second, 5.seconds)) =
    new ChangeStreamReopen("test", () => { opens += opens.size; () }, clock.schedule, delays)

  "ChangeStreamReopen" should "reopen the cursor after a terminal error" in {
    val clock = new Clock; val opens = mutable.Buffer.empty[Int]
    val reopen = driver(clock, opens)
    reopen.failed()
    opens shouldBe empty // scheduled, not immediate
    clock.fire()
    opens should have size 1
  }

  it should "back off across consecutive failures and cap at the last delay" in {
    val clock = new Clock; val opens = mutable.Buffer.empty[Int]
    val reopen = driver(clock, opens)
    (1 to 4).foreach(_ => { reopen.failed(); clock.fire() })
    clock.delays shouldBe Seq(1.second, 5.seconds, 5.seconds, 5.seconds)
    opens should have size 4
  }

  // A cursor that opens and dies immediately has proved nothing, so only a DELIVERED event
  // resets the backoff — otherwise a permanently-broken stream would retry at the shortest
  // delay for ever.
  it should "reset the backoff only once the stream delivers an event" in {
    val clock = new Clock; val opens = mutable.Buffer.empty[Int]
    val reopen = driver(clock, opens)
    reopen.failed(); clock.fire()
    reopen.failed(); clock.fire()
    reopen.opened()
    reopen.failed(); clock.fire()
    clock.delays shouldBe Seq(1.second, 5.seconds, 1.second)
  }

  it should "collapse a double report of the same death into one reopen" in {
    val clock = new Clock; val opens = mutable.Buffer.empty[Int]
    val reopen = driver(clock, opens)
    reopen.failed() // onError
    reopen.failed() // onComplete for the same cursor
    clock.fire()
    opens should have size 1
  }

  it should "stop reopening once closed" in {
    val clock = new Clock; val opens = mutable.Buffer.empty[Int]
    val reopen = driver(clock, opens)
    reopen.failed()
    reopen.close()
    clock.fire()
    opens shouldBe empty
    reopen.failed()
    clock.fire()
    opens shouldBe empty
  }

  it should "retry when the reopen itself throws" in {
    val clock = new Clock
    var attempts = 0
    val reopen = new ChangeStreamReopen("test",
      () => { attempts += 1; if (attempts == 1) throw new IllegalStateException("mongo down") },
      clock.schedule, Seq(1.second, 5.seconds))
    reopen.failed()
    clock.fire() // first attempt throws and schedules another, which this same drain runs
    attempts shouldBe 2
    clock.delays shouldBe Seq(1.second, 5.seconds)
  }
}
