package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class StopwatchSpec extends AnyFlatSpec with Matchers {

  /** A clock the spec advances by hand. */
  private final class Ticks { var now = 1000L; def advance(by: FiniteDuration): Unit = now += by.toNanos }

  "a started reading" should "tell the time since it was taken, each time it is asked" in {
    val ticks   = new Ticks
    val started = new Stopwatch(() => ticks.now).start()
    ticks.advance(1500.millis)
    started.elapsed shouldBe 1500.millis
    started.seconds shouldBe 1.5
    started.millis shouldBe 1500L
    ticks.advance(500.millis)
    started.elapsed shouldBe 2.seconds
  }

  "timed" should "return the block's value and how long the block took" in {
    val ticks = new Ticks
    val timed = new Stopwatch(() => ticks.now).timed { ticks.advance(250.millis); "done" }
    timed shouldBe Stopwatch.Timed("done", 250.millis)
    timed.seconds shouldBe 0.25
  }

  "a total" should "sum every run of the block, a failed run included, and count them" in {
    val ticks = new Ticks
    val total = new Stopwatch(() => ticks.now).total()
    total { ticks.advance(1.second) } shouldBe ()
    total(ticks.advance(2.seconds))
    a[RuntimeException] should be thrownBy total { ticks.advance(3.seconds); throw new RuntimeException("boom") }
    total.elapsed shouldBe 6.seconds
    total.seconds shouldBe 6.0
    total.count shouldBe 3L
  }

  it should "sum runs from many threads without losing one" in {
    val total   = new Stopwatch(() => 0L).total()
    val threads = (1 to 8).map(_ => new Thread(() => (1 to 1000).foreach(_ => total(()))))
    threads.foreach(_.start()); threads.foreach(_.join())
    total.count shouldBe 8000L
  }
}
